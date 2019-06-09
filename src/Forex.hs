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
showForex c = makeReq path
 where
  makeReq p = do
    r <- get $ unpack (C.host c <> C.apiPath c <> p <> C.apiKey c)
    print $ r ^. responseBody
  path = "/convert?q=USD_PLN,PLN_USD&compact=ultra&apiKey=" :: Text

showApiUsage :: C.ForexConfig -> IO ()
showApiUsage c = makeReq path
 where
  makeReq p = do
    r <- get $ unpack (C.host c <> C.apiUsage c <> p <> C.apiKey c)
    print $ r ^. responseBody
  path = "?apiKey=" :: Text

