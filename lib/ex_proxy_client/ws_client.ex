defmodule ExProxyClient.WsClient do
  @moduledoc false
  require Logger
  alias Plug.Crypto.MessageEncryptor
  @timeout 60000

  # 1 => no encrypt packet
  @encrypt_none 1
  # 2 => just encrypt first packet
  @encrypt_once 2
  # 3 => encrypt all packet
  @encrypt_all 3

  @sign_secret "90de3456asxdfrtg"

  def init(opts) do
    {urls, opts} = Map.pop(opts, :servers)
    uri = urls |> Enum.random() |> URI.parse()

    {encrypt_type, key} =
      case Map.get(opts, :encrypt, false) do
        false -> {@encrypt_none, nil}
        {:once, key} when is_binary(key) -> {@encrypt_once, key}
        {:all, key} when is_binary(key) -> {@encrypt_all, key}
      end

    %{uri: uri, encrypt_type: encrypt_type, key: key}
  end

  def send_target(%{conn_pid: conn_pid, stream_ref: stream_ref, key: nil}, binary) do
    :gun.ws_send(conn_pid, stream_ref, {:binary, binary})
  end

  def send_target(%{conn_pid: conn_pid, stream_ref: stream_ref, key: key}, binary) do
    binary = MessageEncryptor.encrypt(binary, key, @sign_secret)
    :gun.ws_send(conn_pid, stream_ref, {:binary, binary})
  end

  def send(
        %{conn_pid: conn_pid, stream_ref: stream_ref, encrypt_type: @encrypt_all, key: key},
        binary
      ) do
    binary = MessageEncryptor.encrypt(binary, key, @sign_secret)
    :gun.ws_send(conn_pid, stream_ref, {:binary, binary})
  end

  def send(%{conn_pid: conn_pid, stream_ref: stream_ref}, binary) do
    :gun.ws_send(conn_pid, stream_ref, {:binary, binary})
  end

  def connect_remote(%{uri: uri} = data) do
    port =
      case uri.scheme do
        "ws" -> uri.port || 80
        "wss" -> uri.port || 443
      end

    url = String.to_charlist(uri.host)

    with {:ok, conn_pid} <- :gun.open(url, port, %{retry_timeout: 2, supervise: false}),
         {:ok, _} <- :gun.await_up(conn_pid, 10_000) do
      stream_ref = :gun.ws_upgrade(conn_pid, uri.path)
      data = Map.merge(data, %{conn_pid: conn_pid, stream_ref: stream_ref})
      {:next_state, :wait_upgrade, data, @timeout}
    else
      error ->
        Logger.error("connect_remote error: #{inspect(error)}")
        {:stop, :normal}
    end
  end

  def handle_response(%{encrypt_type: @encrypt_all, key: key}, response) do
    MessageEncryptor.decrypt(response, key, @sign_secret)
  end

  def handle_response(_data, response), do: response
end
