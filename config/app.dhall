let Env = < Test : {} | Prod : {} >

let showEnv = λ(env : Env) →
  merge { Test = λ(x : {}) →  "test", Prod = λ(x : {}) →  "live" } env

let makeForexConfig = λ(env : Env) →
  { host = "https://free.currconv.com"
  , apiKey = "${env:FOREX_API_KEY as Text}"
  , apiPath = "/api/v7"
  , apiUsage = "/others/usage"
  }

let makeConfig = λ(env : Env) →
  { forex = makeForexConfig env
  }

in makeConfig ( Env.Prod {=} )

