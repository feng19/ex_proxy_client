defmodule ExProxyClient.Http do
  use GenStateMachine, callback_mode: :state_functions
  require Logger
  alias ExProxyClient.WsClient

  @behaviour :ranch_protocol
  @timeout 60000
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
        {:tcp, socket, request},
        %{socket: socket, transport: transport} = data
      ) do
    # Logger.debug("request: #{request}")

    case parse_request(request) do
      {:ok, target, next_request} ->
        transport.setopts(socket, active: :once)

        data
        |> Map.merge(%{target: target, next_request: next_request})
        |> WsClient.connect_remote()

      {:error, error} ->
        Logger.debug("error stop: #{inspect(error)}")
        {:stop, :normal}
    end
  end

  def first_package(:info, {:tcp, _socket, _req}, _data), do: {:stop, :normal}
  def first_package(:info, {:tcp_closed, _socket}, _data), do: {:stop, :normal}
  def first_package(:timeout, _msg, _data), do: {:stop, :normal}

  def wait_upgrade(
        :info,
        {:gun_upgrade, _conn_pid, _stream_ref, [<<"websocket">>], _headers},
        %{socket: socket, transport: transport, target: target, next_request: next_request} = data
      ) do
    WsClient.send_target(data, target)

    if next_request do
      WsClient.send(data, next_request)
    else
      transport.send(socket, "HTTP/1.1 200 Connection Established\r\n\r\n")
    end

    transport.setopts(socket, active: :once)
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

  defp parse_request(<<"CONNECT ", rest::binary>>) do
    case String.split(rest, "\r\n", parts: 2) do
      [first_line, _rest_lines] ->
        [uri, _version] = String.split(first_line, " ", parts: 2)

        case parse_uri(uri) do
          %URI{host: domain, port: nil} when is_binary(domain) ->
            {:ok, <<@domain, 80::16, byte_size(domain), domain::binary>>, nil}

          %URI{host: domain, port: port} when is_binary(domain) and is_integer(port) ->
            {:ok, <<@domain, port::16, byte_size(domain), domain::binary>>, nil}

          bad_uri ->
            Logger.warning("parse uri: #{inspect(uri)} got bad uri: #{inspect(bad_uri)}")
            {:error, :error_uri}
        end

      _ ->
        {:error, :need_more}
    end
  end

  defp parse_request(request) do
    case String.split(request, "\r\n", parts: 2) do
      [first_line, rest_lines] ->
        [method, uri, version] = String.split(first_line, " ", parts: 2)

        case parse_uri(uri) do
          %URI{host: domain, port: nil, path: path} when is_binary(domain) ->
            next_request =
              <<method::binary, path::binary, " ", version::binary, "\r\n", rest_lines::binary>>

            {:ok, <<@domain, 80::16, byte_size(domain), domain::binary>>, next_request}

          %URI{host: domain, port: port, path: path}
          when is_binary(domain) and is_integer(port) ->
            next_request =
              <<method::binary, path::binary, " ", version::binary, "\r\n", rest_lines::binary>>

            {:ok, <<@domain, port::16, byte_size(domain), domain::binary>>, next_request}

          bad_uri ->
            Logger.warning("parse uri: #{inspect(uri)} got bad uri: #{inspect(bad_uri)}")
            {:error, :error_uri}
        end

      _ ->
        {:error, :need_more}
    end
  end

  defp parse_uri(uri) do
    case URI.parse(uri) do
      %URI{host: nil, port: nil} -> URI.parse("//" <> uri)
      uri -> uri
    end
  end
end
