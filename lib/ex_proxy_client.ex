defmodule ExProxyClient do
  def main(_) do
    start()

    receive do
      :stop -> :stop
    end
  end

  def start, do: Application.ensure_started(:ex_proxy_client)
end
