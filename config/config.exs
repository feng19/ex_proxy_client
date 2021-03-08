import Config

config :ex_proxy_client, ExProxyClient.Socks5,
  port: 9090,
  url: "ws://exps.gigalixirapp.com/ws",
  #url: "ws://127.0.0.1:4000/ws",
  encrypt: {:once, "90yT56qlvXmCdrrAnQsdb16HNm7lP6ySqi5tySHIr3o8C+Fr4B8URl5XH0NVssVI"}
