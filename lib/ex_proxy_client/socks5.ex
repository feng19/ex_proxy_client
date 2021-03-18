defmodule ExProxyClient.Socks5 do
  use GenStateMachine, callback_mode: :state_functions
  require Logger
  alias Plug.Crypto.MessageEncryptor

  @behaviour :ranch_protocol
  @timeout 60000

  @ipv4 0x01
  @ipv6 0x04
  @domain 0x03

  # 1 => no encrypt packet
  @encrypt_none 1
  # 2 => just encrypt first packet
  @encrypt_once 2
  # 3 => encrypt all packet
  @encrypt_all 3

  @sign_secret "90de3456asxdfrtg"

  @impl :ranch_protocol
  def start_link(ref, transport, opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, opts}])}
  end

  @impl GenStateMachine
  def init({ref, transport, opts}) do
    Logger.debug("accept a socket")
    Process.flag(:trap_exit, true)
    {:ok, socket} = :ranch.handshake(ref)
    :ok = transport.setopts(socket, [{:active, :once}])

    {url, opts} = Map.pop!(opts, :url)
    uri = URI.parse(url)

    {encrypt_type, key} =
      case Map.get(opts, :encrypt, false) do
        false -> {@encrypt_none, nil}
        {:once, key} when is_binary(key) -> {@encrypt_once, key}
        {:all, key} when is_binary(key) -> {@encrypt_all, key}
      end

    :gen_statem.enter_loop(
      __MODULE__,
      [],
      :first_package,
      %{socket: socket, transport: transport, uri: uri, encrypt_type: encrypt_type, key: key},
      [@timeout]
    )
  end

  def first_package(
        :info,
        {:tcp, socket, <<5, _Nmethods, _Bin::binary>>},
        %{socket: socket, transport: transport} = state_data
      ) do
    # todo auth
    transport.setopts(socket, active: :once)
    transport.send(socket, <<5, 0>>)
    {:next_state, :second_package, state_data, _timeout = 1000}
  end

  def first_package(:info, {:tcp, _socket, _data}, _state_data), do: {:stop, :normal}
  def first_package(:timeout, _msg, _state_data), do: {:stop, :normal}

  def second_package(
        :info,
        {:tcp, _socket, <<5, 1, _Rsv, address_type, rest::binary>>},
        state_data
      ) do
    address_type
    |> adjust_target(rest)
    |> connect_remote(state_data)
  end

  def second_package(:info, {:tcp, _socket, _data}, _state_data), do: {:stop, :normal}
  def second_package(:info, {:tcp_closed, _socket}, _state_data), do: {:stop, :normal}
  def second_package(:timeout, _msg, _state_data), do: {:stop, :normal}

  def wait_upgrade(
        :info,
        {:gun_upgrade, conn_pid, stream_ref, [<<"websocket">>], _headers},
        %{socket: socket, transport: transport, target: target} = state_data
      ) do
    target =
      if key = state_data.key do
        MessageEncryptor.encrypt(target, key, @sign_secret)
      else
        target
      end

    transport.setopts(socket, active: :once)
    :gun.ws_send(conn_pid, stream_ref, {:binary, target})
    transport.send(socket, <<5, 0, 0, 1, 0, 0, 0, 0, 0, 0>>)
    {:next_state, :connected, state_data, @timeout}
  end

  def wait_upgrade(
        :info,
        {:gun_response, _conn_pid, _stream_ref, _, _status, _headers},
        _state_data
      ),
      do: {:stop, :normal}

  def wait_upgrade(:info, {:gun_error, _conn_pid, _stream_ref, _reason}, _state_data),
    do: {:stop, :normal}

  def wait_upgrade(:info, {:tcp_closed, _socket}, _state_data), do: {:stop, :normal}
  def wait_upgrade(:info, {:EXIT, _, _}, _state_data), do: {:stop, :normal}
  def wait_upgrade(:timeout, _msg, _state_data), do: {:stop, :normal}

  def connected(
        :info,
        {:tcp, socket, request},
        %{
          transport: transport,
          conn_pid: conn_pid,
          stream_ref: stream_ref
        } = state_data
      ) do
    transport.setopts(socket, active: :once)

    request =
      if state_data.encrypt_type == @encrypt_all do
        MessageEncryptor.encrypt(request, state_data.key, @sign_secret)
      else
        request
      end

    :gun.ws_send(conn_pid, stream_ref, {:binary, request})
    :keep_state_and_data
    # {:keep_state_and_data, @timeout}
  end

  def connected(
        :info,
        {:gun_ws, _conn_pid, _stream_ref, {:binary, response}},
        %{socket: socket, transport: transport} = state_data
      ) do
    response =
      if state_data.encrypt_type == @encrypt_all do
        MessageEncryptor.decrypt(response, state_data.key, @sign_secret)
      else
        response
      end

    transport.send(socket, response)
    :keep_state_and_data
    # {:keep_state_and_data, @timeout}
  end

  def connected(:info, {:tcp_closed, _socket}, _state_data), do: {:stop, :normal}

  def connected(:info, {:tcp_error, _, reason}, _state_data), do: {:stop, reason}

  def connected(:info, {:gun_ws, _conn_pid, _stream_ref, _frame}, _state_data),
    do: {:stop, :normal}

  def connected(:info, {:EXIT, _, _}, _state_data), do: {:stop, :normal}

  def connected({:call, from}, _request, _state_data) do
    GenStateMachine.reply(from, :ok)
    :keep_state_and_data
  end

  def connected(:cast, _msg, _state_data), do: :keep_state_and_data

  def connected(:timeout, _msg, _state_data), do: {:stop, :timeout}

  def connected(event_type, msg, _state_data) do
    Logger.debug("unknow_event: #{inspect(event_type)}, msg: #{inspect(msg)}")
    {:stop, :unknow_event}
  end

  @impl GenStateMachine
  def terminate(reason, state_name, state_data = %{socket: socket, transport: transport})
      when socket != nil and transport != nil do
    try do
      transport.close(socket)
    after
      terminate(reason, state_name, %{state_data | socket: nil, transport: nil})
    end
  end

  def terminate(reason, state_name, _state_data) do
    Logger.debug("state_name: #{state_name}, terminate #{inspect(reason)}")
    :ok
  end

  defp adjust_target(@ipv4, <<address::32, port::16, _::binary>>),
    do: <<@ipv4, port::16, address::32>>

  defp adjust_target(@ipv6, <<address::128, port::16, _::binary>>),
    do: <<@ipv6, port::16, address::128>>

  defp adjust_target(
         @domain,
         <<domain_len, domain_bin::binary-size(domain_len), port::16, _::binary>>
       ),
       do: <<@domain, port::16, domain_len, domain_bin::binary>>

  defp connect_remote(target, %{uri: uri} = state_data) do
    port =
      case uri.scheme do
        "ws" -> uri.port || 80
        "wss" -> uri.port || 443
      end

    url = String.to_charlist(uri.host)

    with {:ok, conn_pid} <- :gun.open(url, port, %{retry_timeout: 2, supervise: false}),
         {:ok, _} <- :gun.await_up(conn_pid, 10_000) do
      stream_ref = :gun.ws_upgrade(conn_pid, uri.path)

      state_data =
        Map.merge(state_data, %{target: target, conn_pid: conn_pid, stream_ref: stream_ref})

      {:next_state, :wait_upgrade, state_data, @timeout}
    else
      error ->
        Logger.error("connect_remote error: #{inspect(error)}")
        {:stop, :normal}
    end
  end
end
