defmodule ExProxyClient.Socks5 do
  use GenStateMachine, callback_mode: :state_functions
  require Logger
  alias ExProxyClient.WsClient

  @behaviour :ranch_protocol
  @timeout 60000

  @ipv4 0x01
  @ipv6 0x04
  @domain 0x03

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
    data = WsClient.init(opts) |> Map.merge(%{socket: socket, transport: transport})
    :gen_statem.enter_loop(__MODULE__, [], :first_package, data, [@timeout])
  end

  def first_package(
        :info,
        {:tcp, socket, <<5, _Nmethods, _Bin::binary>>},
        %{socket: socket, transport: transport} = data
      ) do
    # todo auth
    transport.setopts(socket, active: :once)
    transport.send(socket, <<5, 0>>)
    {:next_state, :second_package, data, _timeout = 1000}
  end

  def first_package(:info, {:tcp, _socket, _req}, _data), do: {:stop, :normal}
  def first_package(:info, {:tcp_closed, _socket}, _data), do: {:stop, :normal}
  def first_package(:timeout, _msg, _data), do: {:stop, :normal}

  def second_package(
        :info,
        {:tcp, _socket, <<5, 1, _Rsv, address_type, rest::binary>>},
        data
      ) do
    target = parse_target(address_type, rest)
    data |> Map.put(:target, target) |> WsClient.connect_remote()
  end

  def second_package(:info, {:tcp, _socket, _req}, _data), do: {:stop, :normal}
  def second_package(:info, {:tcp_closed, _socket}, _data), do: {:stop, :normal}
  def second_package(:timeout, _msg, _data), do: {:stop, :normal}

  def wait_upgrade(
        :info,
        {:gun_upgrade, _conn_pid, _stream_ref, [<<"websocket">>], _headers},
        %{socket: socket, transport: transport, target: target} = data
      ) do
    transport.setopts(socket, active: :once)
    WsClient.send_target(data, target)
    transport.send(socket, <<5, 0, 0, 1, 0, 0, 0, 0, 0, 0>>)
    {:next_state, :connected, data, @timeout}
  end

  def wait_upgrade(
        :info,
        {:gun_response, _conn_pid, _stream_ref, _, _status, _headers},
        _data
      ),
      do: {:stop, :normal}

  def wait_upgrade(:info, {:gun_error, _conn_pid, _stream_ref, _reason}, _data),
    do: {:stop, :normal}

  def wait_upgrade(:info, {:tcp_closed, _socket}, _data), do: {:stop, :normal}
  def wait_upgrade(:info, {:EXIT, _, _}, _data), do: {:stop, :normal}
  def wait_upgrade(:timeout, _msg, _data), do: {:stop, :normal}

  def connected(:info, {:tcp, socket, request}, %{transport: transport} = data) do
    transport.setopts(socket, active: :once)
    WsClient.send(data, request)
    :keep_state_and_data
    # {:keep_state_and_data, @timeout}
  end

  def connected(
        :info,
        {:gun_ws, _conn_pid, _stream_ref, {:binary, response}},
        %{socket: socket, transport: transport} = data
      ) do
    response = WsClient.handle_response(data, response)
    transport.send(socket, response)
    :keep_state_and_data
    # {:keep_state_and_data, @timeout}
  end

  def connected(:info, {:tcp_closed, _socket}, _data), do: {:stop, :normal}

  def connected(:info, {:tcp_error, _, reason}, _data), do: {:stop, reason}

  def connected(:info, {:gun_ws, _conn_pid, _stream_ref, _frame}, _data),
    do: {:stop, :normal}

  def connected(:info, {:gun_down, _conn_pid, _stream_ref, :closed, _}, _data),
    do: {:stop, :normal}

  def connected(:info, {:EXIT, _, _}, _data), do: {:stop, :normal}

  def connected({:call, from}, _request, _data) do
    GenStateMachine.reply(from, :ok)
    :keep_state_and_data
  end

  def connected(:cast, _msg, _data), do: :keep_state_and_data

  def connected(:timeout, _msg, _data), do: {:stop, :timeout}

  def connected(event_type, msg, _data) do
    Logger.debug("unknown_event: #{inspect(event_type)}, msg: #{inspect(msg)}")
    {:stop, :normal}
  end

  @impl GenStateMachine
  def terminate(reason, state_name, data = %{socket: socket, transport: transport})
      when socket != nil and transport != nil do
    try do
      transport.close(socket)
    after
      terminate(reason, state_name, %{data | socket: nil, transport: nil})
    end
  end

  def terminate(reason, state_name, _data) do
    Logger.debug("state_name: #{state_name}, terminate #{inspect(reason)}")
    :ok
  end

  defp parse_target(@ipv4, <<address::32, port::16, _::binary>>),
    do: <<@ipv4, port::16, address::32>>

  defp parse_target(@ipv6, <<address::128, port::16, _::binary>>),
    do: <<@ipv6, port::16, address::128>>

  defp parse_target(
         @domain,
         <<domain_len, domain_bin::binary-size(domain_len), port::16, _::binary>>
       ),
       do: <<@domain, port::16, domain_len, domain_bin::binary>>
end
