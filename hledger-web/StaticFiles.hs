{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-| 

This module exports routes for all the files in the static directory at
compile time, allowing compile-time verification that referenced files
exist. However, any files added during run-time can't be accessed this
way; use their FilePath or URL to access them.

This is a separate module to satisfy template haskell requirements.

-}
module StaticFiles where

import Yesod.Helpers.Static

import Settings (staticdir)

$(staticFiles staticdir)
