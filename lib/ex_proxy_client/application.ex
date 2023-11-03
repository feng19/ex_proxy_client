defmodule ExProxyClient.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application
  require Logger
  @app :ex_proxy_client

  @impl true
  def start(_type, _args) do
    children = Application.get_env(@app, :endpoints, []) |> Enum.map(&child_spec/1)
    opts = [strategy: :one_for_one, name: ExProxyClient.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp type2module(:socks5), do: ExProxyClient.Socks5
  defp type2module(:http), do: ExProxyClient.Http

  defp child_spec(endpoint_config) do
    {type, config} = endpoint_config |> Map.new() |> Map.pop!(:type)
    module = type2module(type)
    Logger.info("start #{inspect(module)}, config: #{inspect(config)}")

    {ip, config} = Map.pop(config, :ip, {127, 0, 0, 1})
    {port, config} = Map.pop!(config, :port)

    port =
      case port do
        port when is_binary(port) -> String.to_integer(port)
        port when is_integer(port) -> port
      end

    trans_opts = %{socket_opts: [ip: ip, port: port]}
    ref = module |> Module.split() |> List.last() |> String.downcase() |> Kernel.<>("-#{port}")
    :ranch.child_spec(ref, :ranch_tcp, trans_opts, module, config)
  end
end
