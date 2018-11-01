{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (append)
import Web.Spock
import Web.Spock.Config

main :: IO ()
main = do
  config <- defaultSpockCfg () PCNoDatabase ()
  runSpock 3000 (spock config app)

app = do
  let simpleForm = "<html><head><title>Simple Post Request Example</title></head>" `append`
                   "<body><form action=\"/\" method=\"POST\">" `append`
                   "<label for=\"name\">Name: </label>" `append`
                   "<input type=\"text\" name=\"name\" id=\"name\" required>" `append`
                   "<button type=\"submit\" name=\"submit\" value=\"submit-true\">Submit</button>" `append`
                   "</form></body></html>"
  get root $ html simpleForm
  post root $ do
    name <- param' "name"
    text name
