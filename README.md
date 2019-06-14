exchange-rates
==============

[![CircleCI](https://circleci.com/gh/gvolpe/exchange-rates.svg?style=svg)](https://circleci.com/gh/gvolpe/exchange-rates)

Querying a [rate-limited foreign currency exchange API](https://free.currencyconverterapi.com/) using [wreq](http://hackage.haskell.org/package/wreq), [hedis](https://github.com/informatikr/hedis) and [transient](http://hackage.haskell.org/package/transient).

Rates are cached in Redis for 20 minutes (configurable) to avoid hitting the web serivce too often.

### Rate Limits of the Free API

- Conversion Pairs per Request: 2
- Number of Requests per Hour: 100
- Date Range in History: 8 Days
- Allowed Back in History: 1 Year(s)

### Run it locally

You'll need a Redis instance. Easiest way is to run it with `docker`:

```
docker run -p 6379:6379 redis:5.0.0
```

And then run the app with `cabal new-run`, you should see something like:

```
AppConfig {forex = ForexConfig {host = "https://free.currconv.com/api/v7", apiKey = [SECRET]}, redis = RedisConfig {redisHost = "127.0.0.1", redisPort = 6379}}

ApiUsage {timestamp = "2019-06-14T19:25:43.748Z", usage = 6}
Calling web service for: USD -> ARS
Exchange {value = 43.963104}
Calling web service for: EUR -> PLN
Exchange {value = 4.256994}
Cache hit: USD -> ARS
Exchange {value = 43.963104}
Calling web service for: EUR -> GBP
Exchange {value = 0.890276}
Cache hit: EUR -> PLN
Exchange {value = 4.256994}
ApiUsage {timestamp = "2019-06-14T19:25:46.301Z", usage = 9}
```
