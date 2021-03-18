{-# LANGUAGE TemplateHaskell #-}

module Common.Login where

import Data.Text
import Data.Aeson.TH

type Username = Text
type Password = Text

data Login = Login Username Password

deriveJSON defaultOptions ''Login
