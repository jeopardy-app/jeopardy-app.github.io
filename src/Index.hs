{-# LANGUAGE
    OverloadedStrings
  #-}

module Index where

import Shpadoinkle.Run (Env, entrypoint)
import Shpadoinkle.Html
import Shpadoinkle.Backend.Static (renderStatic)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)

indexHtml :: Env -> Html m a
indexHtml env =
  html_
  [ head_
    [ link' [rel "stylesheet", href "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css"]
    , meta' [charset "UTF-8"]
    , script' [src $ entrypoint env]
    ]
  , body'_
  ]

indexHtmlUtf8 :: Env -> ByteString
indexHtmlUtf8 = encodeUtf8 . fromStrict . renderStatic . indexHtml
