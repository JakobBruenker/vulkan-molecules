{-# LANGUAGE BlockArguments
           , LambdaCase
           , TupleSections
           , TemplateHaskellQuotes
           , ImportQualifiedPost
#-}

module TH where

import RIO
import RIO.Char qualified as C
import RIO.Lens
import Language.Haskell.TH

type Field = VarBangType

makeRioClassy :: Name -> DecsQ
makeRioClassy tyConName = do
  fields <- reify tyConName >>= \case
    TyConI (DataD _ _ [] _ [RecC _ fields] _) -> pure fields
    TyConI (NewtypeD _ _ [] _ (RecC _ fields) _) -> pure fields
    _ -> fail "Unsupported declaration"
  (orphanFields, parentedFields) <-
    partitionEithers . map (\(x, my) -> maybe (Left x) (Right . (x,)) my) <$>
    mapM attachClass fields
  superInsts <- concat <$> mapM mkSuperInsts parentedFields
  insts <- mapM mkInstance fields
  let result = [thisClass fields, thisInstance]
            <> map mkClass orphanFields
            <> insts
            <> superInsts
  pure result
  where
    tyCon :: Type
    tyCon = ConT tyConName

    attachClass :: Field -> Q (Field, Maybe Name)
    attachClass field = findClass >>= \x -> pure (field, x)
      where
        findClass = lookupTypeName . nameBase $ className field

    thisInstance :: Dec
    thisInstance =
      InstanceD Nothing [] (AppT (ConT thisClassName) tyCon)
        [FunD thisLensName [Clause [] (NormalB (VarE $ mkName "id")) []]]

    thisLensName :: Name
    thisLensName = mapName (lensSuffix . (ix 0 %~ C.toLower)) tyConName

    thisLens :: Dec
    thisLens = SigD thisLensName (envLens' tyCon)

    thisClass :: [Field] -> Dec
    thisClass fields = ClassD ctxt thisClassName envParam [] [thisLens]
      where
        ctxt :: Cxt
        ctxt = map ((\cls -> AppT (ConT cls) (VarT env)) . className) fields

    env :: Name
    env = mkName "env"

    envParam :: [TyVarBndr ()]
    envParam = [PlainTV env ()]

    thisClassName :: Name
    thisClassName = mapName classPrefix tyConName

    classPrefix :: String -> String
    classPrefix = ("Has" <>)

    lensSuffix :: String -> String
    lensSuffix = (<> "L")

    fieldLens :: Field -> Name
    fieldLens (fieldName, _, _) = mapName lensSuffix fieldName

    mapName :: (String -> String) -> Name -> Name
    mapName f = mkName . f . nameBase

    envLens' :: Type -> Type
    envLens' = AppT (AppT (ConT $ mkName "Lens'") (VarT env))

    mkClass :: Field -> Dec
    mkClass field =
      ClassD [] (className field) envParam [] [mkMethod field]

    className :: Field -> Name
    className = mapName (classPrefix . (ix 0 %~ C.toUpper)) . view _1

    mkMethod :: Field -> Dec
    mkMethod (name, _, fieldType) =
      SigD (mapName lensSuffix name) (envLens' fieldType)

    mkInstance :: Field -> Q Dec
    mkInstance field = InstanceD Nothing [] (AppT (ConT $ className field) tyCon) . pure <$>
      mkImpl field
      where
        mkImpl :: Field -> Q Dec
        mkImpl (fieldName, _, _) = do
          b <- body
          pure $ FunD (mapName lensSuffix fieldName) [Clause [] b []]
          where
            body :: Q Body
            body = do
              x <- newName "x"
              y <- newName "y"
              NormalB <$> [| lens $(varE fieldName) \ $(varP x) $(varP y) ->
                $(recUpdE (varE x) [pure (fieldName, VarE y)]) |]

    mkSuperInsts :: (Field, Name) -> DecsQ
    mkSuperInsts (field, cls) = do
      reify cls >>= \case
        ClassI (ClassD ctxt _ _ _ _) _ -> concat <$> mapM superInstsForPred ctxt
        _ -> fail $ "Expected " <> show cls <> " to be a class, but it's not"
      where
        mkSuperInstsRec :: Name -> DecsQ
        mkSuperInstsRec name = reify name >>= \case
          ClassI (ClassD ctxt _ _ _ decs) _ -> do
            inst <- superInstHead . concat <$> mapM mkSuperImpl decs
            (inst :) . concat <$> mapM superInstsForPred ctxt
          _ -> fail $
            "Couldn't make instance for " <> show name <> " - it's not a class"
          where
            superInstHead :: [Dec] -> Dec
            superInstHead = InstanceD Nothing [] (AppT (ConT name) (ConT tyConName))

        superInstsForPred :: Pred -> DecsQ
        superInstsForPred = \case
          AppT (ConT superCls) (VarT _) -> mkSuperInstsRec superCls
          constraint ->
            fail $ "Unsupported superclass constraint " <> show (ppr constraint)

        mkSuperImpl :: Dec -> DecsQ
        mkSuperImpl (SigD methName _) =
          [d| $(varP methName) = $(varE $ fieldLens field) . $(varE methName) |]
        mkSuperImpl _ = pure []
