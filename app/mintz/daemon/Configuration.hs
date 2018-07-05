{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

{-
-- Need Data.YAML to enable specifying nullability?

import GHC.Generics
import Data.Scientific
import Data.HashMap
import qualified Data.Text as T
import Data.Aeson
import Data.Yaml
import Language.Haskell.TH

declareSettings :: FilePath
                -> Q [Dec]
declareSettings f = runIO $ do
    root <- decodeFileThrow @Object f

fromObject :: Name -- ^ Name of genrated type.
           -> Object -- ^ An item of yaml mapping type.
           -> Q [Dec]
fromObject name obj = do

fromObjectItem :: Value
               -> Q (Type, [Dec])
fromObjectItem Null = (,) <$> [t| Maybe String |] <*> return []
fromObjectItem (String _) = (,) <$> [t| String |] <*> 

typeOfObject :: Name
             -> String
             -> Object
             -> Q Dec
typeOfObject pn n obj = do
    let recs = map toRecord (toList obj)
    con <- recC (mkName n) recs
    dataD (cxt []) (mkName $ show pn ++ "'" ++ n) [] Nothing [con] [derivClause Nothing (varT ''Generic)]
    where
        toRecord :: (T.Text, Value) -> (Name, Bang, Type)
-}