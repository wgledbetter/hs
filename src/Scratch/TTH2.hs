{-# LANGUAGE TemplateHaskell #-}

module Scratch.TTH2 where

import Language.Haskell.TH.Syntax
import Scratch.TTH1 (templatedLookup, templatedTable)

templatedLookupTable :: Int -> Maybe String
templatedLookupTable idx = $$(templatedLookup [||idx||] templatedTable)
