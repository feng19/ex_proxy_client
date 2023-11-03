.PHONY: run stop find_process build

run:
	iex -S mix

stop:
	@elixir --name ex_proxy_client_killer@127.0.0.1 --eval ":rpc.call(:'ex_proxy_client@127.0.0.1', ExProxyClient, :stop, [])"

find_process:
	ps -ef | grep ex_proxy_client

build:
	mix escript.build
