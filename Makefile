.PHONY: run build

run:
	iex -S mix

build:
	mix escript.build