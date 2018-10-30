{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Lazy (toStrict)
import Web.Spock
import Web.Spock.Config

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as R

type AppM = SpockM AppDb AppSession AppState ()
type AppAction ctx a = SpockActionCtx ctx AppDb AppSession AppState a

type AppDb = ()
type AppState = ()
type AppSession = ()

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 3000 (spock spockCfg app)

app :: AppM
app = get root $ do
  {- The 'html' function does two things for us.
  -  1. It sets the content type on the HTTP response
  -  using the function 'setHeaderUnsafe'. Spock-lucid
  -  uses the following when it does this:
  -
  -  setHeader "Content-Type" "text/html; charset=utf-8"
  -
  -  2. The second thing 'html' does is convert our Text
  -  string to a strict ByteString and from there to a
  -  lazy ByteString. If you can render your template in
  -  a lazy ByteString, you can cut down on the conversions
  -  by using the function 'lazyBytes' with the ByteString
  -  to return the HTTP response with the now set header
  -  and body.
  -
  -  In short, instead of Text.Blaze.Html.Render.Text we could
  -  use the Utf8 variant and provide the following code instead:
  -
  -  setHeader "Content-Type" "text/html; charset=utf-8"
  -  lazyBytes $ renderHtml hello
  -
  -  For further study see here: https://hackage.haskell.org/package/Spock-core-0.13.0.0/docs/src/Web.Spock.Internal.CoreAction.html#html
  -
  -  Also please remember that the Spock internal API is subject to
  -  change, so please consult current documentation for the versions
  -  you are using.
  -}
  html . toStrict $ R.renderHtml hello

hello :: H.Html
hello = H.docTypeHtml $ do
  H.head $ do
    H.title "Hello from Blaze and Spock"
  H.body $ do
    H.p "Hello world! Powered by Blaze and Spock."
