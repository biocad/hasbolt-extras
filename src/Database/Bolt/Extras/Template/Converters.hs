{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Bolt.Extras.Template.Converters
 (
    makeNodeLike
  , makeURelationLike
  ) where

import           Control.Lens                        (view, _1)
import           Data.Map.Strict                     (fromList, member, (!))
import           Data.Text                           (Text, pack, unpack)
import           Database.Bolt.Extras.Template.Types (FromValue (..),
                                                      Labels (..), Node (..),
                                                      NodeLike (..),
                                                      Properties (..),
                                                      ToValue (..),
                                                      URelationLike (..),
                                                      URelationship (..))
import           Database.Bolt.Extras.Utils          (currentLoc, dummyId)
import           Instances.TH.Lift                   ()
import           Language.Haskell.TH

-- | Describes a @bijective@ class, i.e. class that has two functions: @phi :: a -> SomeType@ and @phiInv :: SomeType -> a@.
-- Requires class name, @SomeType@ name and names of the class functions (@phi@ and @phiInv@).
--
data BiClassInfo = BiClassInfo { className    :: Name
                               , dataName     :: Name
                               , classToFun   :: Name
                               , classFromFun :: Name
                               }

-- | Example of @bijective@ class is 'NodeLike'.
-- Describes conversions into and from 'Node'.
-- That is, this class provides a bridge between Neo4j world and Haskell world.
--
nodeLikeClass :: BiClassInfo
nodeLikeClass = BiClassInfo { className     = ''NodeLike
                            , dataName      = 'Node
                            , classToFun    = 'toNode
                            , classFromFun  = 'fromNode
                            }

-- | Another example of @bijective@ class is 'URelationLike'.
-- Describes conversions into and from 'URelationship'.
--
uRelationLikeClass :: BiClassInfo
uRelationLikeClass = BiClassInfo { className    = ''URelationLike
                                 , dataName     = 'URelationship
                                 , classToFun   = 'toURelation
                                 , classFromFun = 'fromURelation
                                 }

-- | Make an instance of 'NodeLike' class.
-- Only data types with one constructor are currently supported.
-- Each field is transformed into 'Text' key and its value is transformed into a 'Value'.
-- For example, we have a structure
--
-- > data Foo = Bar { baz  :: Double
-- >                , quux :: Text
-- >                , quuz :: Int
-- >                }
--
-- You can make it instance of NodeClass by writing
-- > makeNodeLike ''Foo
--
-- Then you may create example and convert it into from from Node:
--
-- > ghci>:set -XOverloadedStrings
-- > ghci> let foo = Bar 42.0 "Loren ipsum" 7
-- > ghci> toNode foo
-- > Node {nodeIdentity = -1, labels = ["Foo"], nodeProps = fromList [("baz",F 42.0),("quux",T "Loren ipsum"),("quuz",I 7)]}
-- > ghci> fromNode . toNode $ foo :: Foo
-- > Bar {baz = 42.0, quux = "Loren ipsum", quuz = 7}
--
makeNodeLike :: Name -> Q [Dec]
makeNodeLike = makeBiClassInstance nodeLikeClass


-- | Make an instance of 'URelationLike' class.
-- Transformations are the same as in 'NodeLike' instance declaration with the only one difference:
-- 'URelationship' holds only one label (or type), but 'Node' holds list of labels.
--
makeURelationLike :: Name -> Q [Dec]
makeURelationLike = makeBiClassInstance uRelationLikeClass


-- | Declare an instance of `bijective` class using TemplateHaskell.
-- It works as follows:
-- Say we have a type with field records, e.g.
--
-- > data VariableDomainScoring = VDS { specie   :: Text
-- >                                  , vgen     :: Double
-- >                                  , fr       :: Double
-- >                                  , sim      :: Double
-- >                                  , germline :: Text
-- >                                  }
--
-- As an example, transformation into Node is described below.
--
-- > data Node = Node { nodeIdentity :: Int             -- ^Neo4j node identifier
-- >                  , labels       :: [Text]          -- ^Set of node labels (types)
-- >                  , nodeProps    :: Map Text Value  -- ^Dict of node properties
-- >                  }
-- >  deriving (Show, Eq)
--
-- @nodeIdentity@ will be set to a dummy value (-1). There is no way of obtaining object ID before uploading it into database.
-- @labels@ will be set to type name, i.e. @VariableDomainScoring@. This is due to our convention: object label into Neo4j is the same as its type name in Haskell.
-- @nodeProps@ will be set to a Map: keys are field record names, values are data in the corresponding fields.
--
-- Therefore, applying toNode on a @VariableDomainScoring@ will give the following:
-- > Node { nodeIdentity = -1
-- >      , labels = ["VariableDomainScoring"]
-- >      , nodeProps = fromList [("specie", T "text value"), ("vgen", F %float_value), ("fr", F %float_value), ("sim", F %float_value), ("germline", T "text value")]
-- >     }
--
makeBiClassInstance :: BiClassInfo -> Name -> Q [Dec]
makeBiClassInstance BiClassInfo {..} typeCon = do
  -- reify function gives Info about Name such as constructor name and its fields. See: https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH.html#t:Info
  TyConI declaration <- reify typeCon

  -- get type declaration parameters: type name and fields. Supports data and newtype only. These will be used in properties Map formation.
  let (tyName, constr) = getTypeCons declaration

  -- nameBase gives object name without package prefix. `label` is the type name here.
  let label = nameBase tyName

  -- collects the names of all fields in the type.
  let dataFields = concatMap (snd . getConsFields) constr

  -- gets data constructor name
  let (consName, _) = head $ fmap getConsFields constr

  -- Just a fresh variable. It will be used in labmda abstractions in makeToClause and makeFromClause functions.
  fresh <- newName "x"

  -- constructs `bijective` class functions (phi and phiInv – toClause and fromClause correspondingly here).
  toClause   <- makeToClause label dataName fresh dataFields
  fromClause <- makeFromClause label consName fresh dataFields

  -- function declarations themselves.
  let bodyDecl = [FunD classToFun [toClause], FunD classFromFun [fromClause]]

  -- Instance declaration itself.
  pure [InstanceD Nothing [] (AppT (ConT className) (ConT typeCon)) bodyDecl]



-- | Extract information about type: constructor name and field record names.
--
getConsFields :: Con -> (Name, [Name])
getConsFields (RecC cName decs)           = (cName, fmap (view _1) decs)
getConsFields (ForallC _ _ cons)          = getConsFields cons
getConsFields (RecGadtC (cName:_) decs _) = (cName, fmap (view _1) decs)
getConsFields (NormalC cName _)           = (cName, [])
getConsFields _                           = error $ $currentLoc ++ "unsupported data declaration."


-- | Parse a type declaration and retrieve its name and its constructors.
--
getTypeCons :: Dec -> (Name, [Con])
getTypeCons (DataD    _ typeName _ _ constructors _) = (typeName, constructors)
getTypeCons (NewtypeD _ typeName _ _ constructor  _) = (typeName, [constructor])
getTypeCons otherDecl = error $ $currentLoc ++ "unsupported declaration: " ++ show otherDecl ++ "\nShould be either 'data' or 'newtype'."

-- | Describes the body of conversion to target type function.
--
makeToClause :: String -> Name -> Name -> [Name] -> Q Clause
makeToClause label dataCons varName dataFields | null dataFields = pure $ Clause [WildP] (NormalB result) []
                                               | otherwise       = pure $ Clause [VarP varName] (NormalB result) []
  where
    -- apply field record to a data.
    getValue :: Name -> Exp
    getValue name = AppE (VarE name) (VarE varName)

    -- List of values which a data holds.
    -- The same in terms of Haskell :: valuesExp = fmap (\field -> toValue (field x))
    -- `x` is a bounded in pattern match variable (e.g. toNode x = ...). If toNode :: a -> Node, then x :: a, i.e. x is data which we want to convert into Node.
    -- `field` is a field record function.
    valuesExp :: [Exp]
    valuesExp = fmap (\fld -> AppE (VarE 'toValue) (getValue fld)) dataFields

    -- Retrieve all field record names from the convertible type.
    fieldNames :: [String]
    fieldNames = fmap nameBase dataFields

    -- List of pairs :: [(key, value)]
    -- `key` is field record name.
    -- `value` is the data that corresponding field holds.
    pairs :: [Exp]
    pairs = zipWith (\fld val -> TupE [strToTextE fld, val]) fieldNames valuesExp

    -- Map representation:
    -- mapE = fromList pairs
    -- in terms of Haskell.
    mapE :: Exp
    mapE = AppE (VarE 'fromList) (ListE pairs)

    -- A bit of crutches.
    -- The difference between Node and URelationship is in the number of labels they hold.
    -- strToTextE returns Text packed in Exp so `id` is applied to it when constructing URelationship.
    -- Node takes list of labels so the label must be packed into list using ListE . (:[])
    fieldFun :: Exp -> Exp
    fieldFun | nameBase dataCons == "URelationship" = id
             | nameBase dataCons == "Node"          = ListE . (:[])
             | otherwise                = error $ $currentLoc ++ "unsupported data type."

    -- In terms of Haskell:
    -- dataCons (fromIntegral dummyId) (fieldFun label) mapE
    -- Constructs data with three fields.
    result :: Exp
    result = AppE (AppE (AppE (ConE dataCons) (LitE . IntegerL . fromIntegral $ dummyId)) (fieldFun $ strToTextE label)) mapE


-- | Describes the body of conversion from target type function.
--
makeFromClause :: String -> Name -> Name -> [Name] -> Q Clause
makeFromClause label conName varName dataFields = do
  -- fieldNames :: [Text]
  -- field records of the target type.
  let fieldNames = fmap (pack . nameBase) dataFields

  -- dataLabel :: Text
  -- Label a.k.a type name
  let dataLabel = pack label

  -- Field record names packed in Exp
  -- \x -> [|x|] :: a -> Q Exp
  -- Therefore, fieldNamesE :: [Exp]
  fieldNamesE <- mapM (\x -> [|x|]) fieldNames

  -- varExp :: Q Exp
  -- Pattern match variable packed in Exp. It will be used in QuasiQuotation below.
  let varExp = pure (VarE varName)

  -- Guard checks that all necessary fields are present in the container.
  guardSuccess <- NormalG <$> [|checkLabels $(varExp) [dataLabel] && checkProps $(varExp) fieldNames|]

  -- `otherwise` case.
  guardFail <- NormalG <$> [|otherwise|]

  -- Unpack error message.
  failExp <- [|unpackError $(varExp) (unpack dataLabel)|]

  -- Kind of this function realization in terms of Haskell:
  -- fromNode :: Node -> a
  -- fromNode varName | checkLabels varName [dataLabel] && checkProps varName fieldNames = ConName { foo = bar, baz = quux ...}
  --                  | otherwise = unpackError varName (unpack dataLabel)
  let successExp = RecConE conName (zipWith (\f d -> (d, AppE (AppE (VarE 'getProp) (VarE varName)) f)) fieldNamesE dataFields)
  let successCase = (guardSuccess, successExp)
  let failCase = (guardFail, failExp)

  pure $ Clause [VarP varName] (GuardedB [successCase, failCase]) []


strToTextE :: String -> Exp
strToTextE str = AppE (VarE 'pack) (LitE . StringL $ str)

checkProps :: Properties t => t -> [Text] -> Bool
checkProps container = all (`member` getProps container)

checkLabels :: Labels t => t -> [Text] -> Bool
checkLabels container = all (`elem` getLabels container)

getProp :: (Properties t, FromValue a) => t -> Text -> a
getProp container field = fromValue (getProps container ! field)

unpackError :: Show c => c -> String -> a
unpackError container label = error $ $currentLoc ++ " could not unpack " ++ label ++ " from " ++ show container
