{-# LANGUAGE TemplateHaskellQuotes #-}

module TH where

import RIO
import Language.Haskell.TH
import Control.Lens.TH (makeLensesWith, classyRules_, DefName (TopName), lensField, lensClass)

type Field = VarBangType

makeRegularClassy :: Name -> DecsQ
makeRegularClassy = makeLensesWith $
  classyRules_ & lensField .~ (\_ _ n -> [TopName . mkName . lensSuffix $ nameBase n])
               & lensClass .~ (\(nameBase -> n) -> Just ( mkName (classPrefix n)
                                                        , mkName $ '_':lensSuffix n))

lensSuffix :: String -> String
lensSuffix = (<> "L")

classPrefix :: String -> String
classPrefix = ("Has" <>)
