module Main where

--import Happstack.Server.Env (nullConf, simpleHTTP, toResponse, ok)
import Happstack.Server (nullConf, simpleHTTP, toResponse, ok)

main :: IO ()
main = simpleHTTP nullConf $ ok "Hello, World!"

















