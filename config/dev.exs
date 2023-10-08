import Config

encrypt_key =
  System.get_env(
    "EXPS_ENCRYPT_KEY",
    "90yT56qlvXmCdrrAnQsdb16HNm7lP6yS"
  )

config :ex_proxy_client, ExProxyClient.Socks5,
  port: 9050,
  urls: ["ws://127.0.0.1:4000/ws"],
  encrypt: {:once, encrypt_key}

config :ex_proxy_client, ExProxyClient.Http,
  port: 9060,
  urls: ["ws://127.0.0.1:4000/ws"],
  encrypt: {:once, encrypt_key}
