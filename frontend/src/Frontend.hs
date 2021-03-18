{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Frontend where

import Control.Monad
import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Obelisk.Route.Frontend

import Reflex.Dom.Core

import Common.Api
import Common.Route
import Common.Login

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "TreeHouse"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: "https://unpkg.com/tailwindcss@%5E2/dist/tailwind.min.css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do

      subRoute_ $ \case

        FrontendRoute_Main -> do
          el "h1" $ text "Welcome to Treehouse!"
          routeLink (FrontendRoute_Login :/ ()) $ elClass "button" "text-blue hover:text-gray-100" $ text "Log in!"
          
        FrontendRoute_Login -> el "div" $ do
          el "h1" $ text "Log into your account"
          elClass "div" "flex flex-col mx-auto w-1/2" $ do
            elClass "div" "flex flex-row justify-end my-2" $ do
              el "h5" $ text "Enter Username:"
              username <- _inputElement_value <$> (inputElement $ def
                                                   & initialAttributes .~ "class" =: "w-64 ml-2 border-2 p-1 rounded-sm border-black")
              dynText username

            elClass "div" "flex flex-row justify-end my-2" $ do
              el "h5" $ text "Enter Password:"
              password <- _inputElement_value <$> (inputElement $ def
                                                   & initialAttributes .~ "class" =: "w-64 ml-2 border-2 p-1 rounded-sm border-black" <> "type" =: "password")
              dynText password

            elClass "div" "flex flex-col items-end" $ do
              (e, _) <- elClass' "button" "px-4 py-2 bg-blue-300 rounded text-white hover:bg-green-300 transition-all duration-200 w-64 hover:shadow-lg" $ text "Log in"
              let clicked = domEvent Click e
              count <- foldDyn (+) (0 :: Int) $ fmap (const 1) clicked
              display count
              routeLink (FrontendRoute_Main :/ ()) $ elClass "button" "text-blue hover:text-gray-100" $ text "Back to Home page"

      return ()
  }
