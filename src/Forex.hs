{-# LANGUAGE OverloadedStrings #-}

module Forex
  ( showApiUsage
  , showForex
  )
where

import qualified Config                        as C
import           Control.Lens
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Network.Wreq

showForex :: C.ForexConfig -> IO ()
showForex c = makeReq ops url
 where
  makeReq ops url = do
    r <- getWith ops url
    print $ r ^. responseBody
  url = unpack (C.host c <> C.apiPath c <> "/convert")
  ops = defaults & param "q" .~ ["USD_PLN"] & param "compact" .~ ["ultra"] & param "apiKey" .~ [C.apiKey c]

showApiUsage :: C.ForexConfig -> IO ()
showApiUsage c = makeReq ops url
 where
  makeReq ops url = do
    r <- getWith ops url
    print $ r ^. responseBody
  url = unpack (C.host c <> C.apiUsage c)
  ops = defaults & param "apiKey" .~ [C.apiKey c]

