{-# LANGUAGE OverloadedStrings #-}


module Main where

import Index (indexHtmlUtf8)

import           Shpadoinkle                      (Html, JSM)
import           Shpadoinkle.Backend.Snabbdom     (runSnabbdom, stage)
import           Shpadoinkle.Html
import           Shpadoinkle.Run                  (Env (..), liveWithIndex, simple)
import           Language.Javascript.JSaddle.Warp (runWithIndex)


view :: () -> Html m ()
view _ = div_ [ "hello world" ]


app :: JSM ()
app = simple runSnabbdom () view stage


dev :: IO ()
dev = liveWithIndex (indexHtmlUtf8 Dev) 8080 app


main :: IO ()
main = do
  putStrLn "\nhi, my name is jeopardy"
  putStrLn "happy point of view on https://localhost:8080\n"
  runWithIndex (indexHtmlUtf8 Prod) 8080 app
