let Env = < Test : {} | Prod : {} >

let forexEnv = λ(env : Env) →
  merge { Test = λ(x : {}) →  "free", Prod = λ(x : {}) →  "api" } env

let redisEnv = λ(env : Env) →
  merge { Test = λ(x : {}) →  "127.0.0.1", Prod = λ(x : {}) →  "127.0.0.1" } env

let makeForexConfig = λ(env : Env) →
  { apiHost = "https://${forexEnv env}.currconv.com"
  , apiKey = "${env:FOREX_API_KEY as Text}"
  , apiPath = "/api/v7"
  , apiUsage = "/others/usage"
  , keyExpiration = 60 * 60 -- 1 hour in seconds
  , requestsPerHour = 100
  }

let makeRedisConfig = λ(env : Env) →
  { redisHost = "${redisEnv env}"
  , redisPort = 6379
  }

let makeConfig = λ(env : Env) →
  { forex = makeForexConfig env
  , redis = makeRedisConfig env
  }

in makeConfig ( Env.Test {=} )
