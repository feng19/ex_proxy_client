defmodule ExProxyClientTest do
  use ExUnit.Case
  doctest ExProxyClient

  test "greets the world" do
    assert ExProxyClient.hello() == :world
  end
end
