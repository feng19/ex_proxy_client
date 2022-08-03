import Config

encrypt_key =
  System.get_env(
    "EX_PROXY_ENCRYPT_KEY",
    "90yT56qlvXmCdrrAnQsdb16HNm7lP6ySqi5tySHIr3o8C+Fr4B8URl5XH0NVssVI"
  )

config :ex_proxy_client, ExProxyClient.Socks5,
  port: 9050,
  url: "ws://127.0.0.1:4000/ws",
  encrypt: {:once, encrypt_key}

config :ex_proxy_client, ExProxyClient.Http,
  port: 9060,
  url: "ws://127.0.0.1:4000/ws",
  encrypt: {:once, encrypt_key}
