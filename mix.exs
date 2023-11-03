defmodule ExProxyClient.MixProject do
  use Mix.Project

  def project do
    [
      app: :ex_proxy_client,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      escript: escript(),
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {ExProxyClient.Application, []}
    ]
  end

  def escript do
    [
      main_module: ExProxyClient,
      emu_args: "+K true -detached -name ex_proxy_client@127.0.0.1"
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:plug_crypto, "~> 2.0"},
      {:ranch, "~> 2.0"},
      {:gen_state_machine, "~> 3.0"},
      {:gun, "~> 2.0"}
    ]
  end
end
