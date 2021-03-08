defmodule ExProxyClient.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application
  require Logger
  @app :ex_proxy_client

  @impl true
  def start(_type, _args) do
    children = [
      # child_spec(ExProxyClient.Http),
      # child_spec(ExProxyClient.Socks4),
      child_spec(ExProxyClient.Socks5)
    ]

    opts = [strategy: :one_for_one, name: ExProxyClient.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp child_spec(module) do
    config = Application.fetch_env!(@app, module) |> Map.new()
    Logger.info("start #{inspect(module)}, config: #{inspect(config)}")

    port =
      case config.port do
        port when is_binary(port) -> String.to_integer(port)
        port when is_integer(port) -> port
      end

    trans_opts = %{socket_opts: [port: port]}
    ref = module |> Module.split() |> List.last() |> String.downcase()
    :ranch.child_spec(ref, :ranch_tcp, trans_opts, module, config)
  end
end
