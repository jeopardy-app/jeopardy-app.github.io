module Index where

import Shpadoinkle (Env, entrypoint)
import Shpadonikle.Html
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)

indexHtml :: Env -> Html m a
indexHtml env =
  html_
  [ head_
    [ link' [rel "stylesheet", href "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css"]
    , meta_ [charset "UTF-8"]
    , script_ [src $ entrypoint env]
    ]
  , body_'
  ]

indexHtmlUtf8 :: Env -> ByteString
indexHtmlUtf8 = encodeUtf8 . fromStrict . renderStatic . indexHtml
