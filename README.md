# redis-glob

[![GitHub CI](https://github.com/adetokunbo/redis-glob/actions/workflows/ci.yml/badge.svg)](https://github.com/adetokunbo/redis-glob/actions)
[![Stackage Nightly](http://stackage.org/package/redis-glob/badge/nightly)](http://stackage.org/nightly/package/redis-glob)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/redis-glob/blob/master/LICENSE)

`redis-glob` checks that glob expressions for use with Redis are valid.
Redis commands like [KEYS] use glob expressions to match keys in redis.

If the glob expression is invalid, the command returns an empty result, which
unfortunately is the same result as if the expression was valid but had no
results.

The `validate` function is used to confirm that an expression is valid. It
parses the expression in the same way as the actual [redis glob], returning
`Nothing` for invalid expressions.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Redis.Glob
import qualified Data.ByteString.Lazy.Char8 as CL


isOK :: CL.ByteString -> IO ()
isOk b = CL.putStrLn $ "Is " <> b <> " ok? :" <> maybe "n" (const "y") (validate b)


main :: IO()
main = do
  isOK "h?llo"  -- "Is h?llo ok? y"
  isOK "h[a-b]llo" -- Is h[a-b]llo ok? y"
  isOK "h[a-]llo" -- Is h[a-]llo ok? n"
```

[1]: https://hackage.haskell.org/package/wai
[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/redis-glob.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=redis-glob>
[hackage-badge]:      <https://img.shields.io/hackage/v/redis-glob.svg>
[hackage]:            <https://hackage.haskell.org/package/redis-glob>
[KEYS]:               <https://redis.io/commands/keys>
[redis glob]:         <https://github.com/redis/redis/blob/203b12e41ff7981f0fae5b23819f072d61594813/src/util.c#L54>
