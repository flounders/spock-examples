{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Original imports from the tutorial.
import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T

-- Imports for extended example.
import Database.SQLite.Simple
import Lucid

-- In case you don't know setting up type aliases like this will save you a
-- lot of work should you refactor later. If you need to use a lot of actions,
-- it helps save typing too.
type ExampleM = SpockM ExampleDb ExampleSession ExampleState ()
type ExampleAction ctx a = SpockActionCtx ctx ExampleDb ExampleSession ExampleState a

type ExampleDb = Connection
data ExampleState = ExampleState (IORef Int)
data ExampleSession = EmptySession


main :: IO ()
main = do
  open "test.db" >>= (\x -> execute_ x createGuestTable >> close x)
  ref <- newIORef 0
  spockCfg <- defaultSpockCfg EmptySession (PCConn $
    ConnBuilder (open "test.db") close (PoolCfg 1 1 0.5)) (ExampleState ref)
  runSpock 8080 (spock spockCfg app)

app :: ExampleM
app = do
  get root $ do
    (ExampleState ref) <- getState
    visitNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
    html . toStrict . renderText $ pageTemplate (do
      p_ "Welcome to more Spock Examples."
      p_ . toHtml $ "You are visit number " `T.append` (T.pack . show $ visitNumber) `T.append` "!"
      p_ "Please put your name in the guestbook."
      p_ $ a_ [href_ "guestbook"] "Guestbook") "ExampleApp"
  get "guestbook" $ do
    guests <- runQuery (\x -> query_ x "SELECT * FROM guest" :: IO [Only Text])
    html . toStrict . renderText $ pageTemplate (do
      h1_ "Guest List"
      guestsTemplate guests
      h1_ "Guest Sign In"
      form_ [action_ "guestbook", method_ "post"] $ do
        label_ "Name: "
        input_ [type_ "text", name_ "name"]
        input_ [type_ "submit"]) "ExampleApp Guestbook"
  post "guestbook" $ do
    ps <- params
    runQuery (\x -> execute x "INSERT INTO guest VALUES(?)" (Only . snd . Prelude.head $ filter (\x -> "name" == fst x) ps))
    redirect "guestbook"

guestsTemplate :: Monad m => [Only Text] -> HtmlT m ()
guestsTemplate xs = do
  table_ $ do
    tr_ $ do
      th_ "Guest"
    sequence_ $ map f xs
  where f :: Monad m => Only Text -> HtmlT m ()
        f (Only x) = do
          tr_ $ do
            td_ $ toHtml x


createGuestTable :: Query
createGuestTable = "CREATE TABLE IF NOT EXISTS guest (name TEXT)"

pageTemplate :: Monad m => HtmlT m a -> Text -> HtmlT m a
pageTemplate x title = do
  doctype_
  html_ $ do
    head_ $ do
      title_ $ toHtml title
    body_ $ do
      x
