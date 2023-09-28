{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Bolt.Extras.Template.Internal.Converters
 (
    makeNodeLike
  , makeNodeLikeWith
  , makeURelationLike
  , makeURelationLikeWith
  ) where

import           Data.Map.Strict            (fromList, member, notMember, (!))
import           Data.Text                  (Text, pack, unpack)
import           Database.Bolt (Node (..), URelationship (..), Value (..), IsValue(..), RecordValue(..))
import           Database.Bolt.Extras       (Labels (..),
                                             NodeLike (..),
                                             Properties (..),
                                             URelationLike (..))
import           Database.Bolt.Extras.Utils (currentLoc, dummyId)
import           Instances.TH.Lift          ()
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           GHC.Stack                  (HasCallStack)

-- Starting with template-haskell-2.16.0.0, 'TupE' constructor accepts @Maybe Exp@, to support
-- TupleSections. We use this alias for compatibility with both old and new versions.
tupE' :: [Exp] -> Exp
#if MIN_VERSION_template_haskell(2, 16, 0)
tupE' = TupE . map Just
#else
tupE' = TupE
#endif


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
-- For example, we have a structure and define an instance:
--
-- >>> :{
-- data Foo = Bar
--   { baz  :: Double
--   , quux :: Text
--   , quuz :: Maybe Int
--   } deriving (Show)
-- makeNodeLike ''Foo
-- :}
--
-- Then you may create example and convert it to and from Node:
--
-- >>> let foo = Bar 42.0 "Loren ipsum" (Just 7)
-- >>> toNode foo
-- Node {nodeIdentity = -1, labels = ["Foo"], nodeProps = fromList [("baz",F 42.0),("quux",T "Loren ipsum"),("quuz",I 7)]}
-- >>> fromNode . toNode $ foo :: Foo
-- Bar {baz = 42.0, quux = "Loren ipsum", quuz = Just 7}
--
-- 'Maybe' fields are handled correctly:
--
-- >>> let bar = Bar 42.0 "Hello world" Nothing
-- >>> toNode bar
-- Node {nodeIdentity = -1, labels = ["Foo"], nodeProps = fromList [("baz",F 42.0),("quux",T "Hello world"),("quuz",N ())]}
-- >>> :{
-- let barNode = Node
--       { nodeIdentity = -1
--       , labels = ["Foo"]
--       , nodeProps = fromList [("baz", F 42.0), ("quux", T "Hello world")] -- No "quuz" here
--       }
-- :}
--
-- >>> fromNode barNode :: Foo
-- Bar {baz = 42.0, quux = "Hello world", quuz = Nothing}
makeNodeLike :: HasCallStack => Name -> Q [Dec]
makeNodeLike name = makeBiClassInstance nodeLikeClass name id

-- | The same as 'makeNodeLike', but applies a function to all field names before storing them
-- in Neo4j, like @aeson@ does.
--
-- This can be used with @fieldLabelModifier@ from 'Data.Aeson.Types.Options' in @aeson@:
--
-- > makeNodeLikeWith ''Foo $ fieldLabelModifier $ aesonPrefix camelCase
--
makeNodeLikeWith :: HasCallStack => Name -> (String -> String) -> Q [Dec]
makeNodeLikeWith = makeBiClassInstance nodeLikeClass

-- | Make an instance of 'URelationLike' class.
-- Transformations are the same as in 'NodeLike' instance declaration with the only one difference:
-- 'URelationship' holds only one label (or type), but 'Node' holds list of labels.
--
makeURelationLike :: HasCallStack => Name -> Q [Dec]
makeURelationLike name = makeBiClassInstance uRelationLikeClass name id

-- | As 'makeNodeLikeWith'.
makeURelationLikeWith :: HasCallStack => Name -> (String -> String) -> Q [Dec]
makeURelationLikeWith = makeBiClassInstance uRelationLikeClass

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
makeBiClassInstance :: HasCallStack => BiClassInfo -> Name -> (String -> String) -> Q [Dec]
makeBiClassInstance BiClassInfo {..} typeCon fieldLabelModifier = do
  -- reify function gives Info about Name such as constructor name and its fields. See: https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH.html#t:Info
  TyConI declaration <- reify typeCon

  -- get type declaration parameters: type name and fields. Supports data and newtype only. These will be used in properties Map formation.
  let (tyName, constr) = getTypeCons declaration

  -- nameBase gives object name without package prefix. `label` is the type name here.
  let label = nameBase tyName

  -- collects names and types of all fields in the type.
  let (dataFields, fieldTypes) = unzip $ concatMap (snd . getConsFields) constr

  -- gets data constructor name
  let (consName, _) = head $ fmap getConsFields constr

  -- Just a fresh variable. It will be used in labmda abstractions in makeFromClause function.
  fresh <- newName "x"

  -- constructs `bijective` class functions (phi and phiInv – toClause and fromClause correspondingly here).
  toClause   <- makeToClause label dataName consName dataFields fieldLabelModifier
  fromClause <- makeFromClause label consName fresh dataFields fieldTypes fieldLabelModifier

  -- function declarations themselves.
  let bodyDecl = [FunD classToFun [toClause], FunD classFromFun [fromClause]]

  -- Instance declaration itself.
  pure [InstanceD Nothing [] (AppT (ConT className) (ConT typeCon)) bodyDecl]

-- | Extract information about type: constructor name and field record names with corresponding types.
--
getConsFields :: HasCallStack => Con -> (Name, [(Name, Type)])
getConsFields (RecC cName decs)           = (cName, fmap (\(fname, _, ftype) -> (fname, ftype)) decs)
getConsFields (ForallC _ _ cons)          = getConsFields cons
getConsFields (RecGadtC (cName:_) decs _) = (cName, fmap (\(fname, _, ftype) -> (fname, ftype)) decs)
getConsFields (NormalC cName _)           = (cName, [])
getConsFields _                           = error $ $currentLoc ++ "unsupported data declaration."


-- | Parse a type declaration and retrieve its name and its constructors.
--
getTypeCons :: HasCallStack => Dec -> (Name, [Con])
getTypeCons (DataD    _ typeName _ _ constructors _) = (typeName, constructors)
getTypeCons (NewtypeD _ typeName _ _ constructor  _) = (typeName, [constructor])
getTypeCons otherDecl = error $ $currentLoc ++ "unsupported declaration: " ++ show otherDecl ++ "\nShould be either 'data' or 'newtype'."

-- | Describes the body of conversion to target type function.
--
makeToClause :: String -> Name -> Name -> [Name] -> (String -> String) -> Q Clause
makeToClause label dataCons consName dataFields fieldLabelModifier
  | null dataFields = pure $ Clause [WildP] (NormalB $ result []) []
  | otherwise       = do
    fieldVars <- sequenceQ $ newName "_field" <$ dataFields -- var for each field
    pure $ Clause [recPat fieldVars] (NormalB $ result fieldVars) []
  where
    -- construct record pattern: (Rec {f1 = v1, ... })
    recPat :: [Name] -> Pat
    recPat fieldVars = ParensP $ RecP consName $ zip dataFields $ VarP <$> fieldVars

    -- List of values which a data holds.
    -- The same in terms of Haskell :: valuesExp = fmap (\field -> toValue fieldVar)
    valuesExp :: [Name] -> [Exp]
    valuesExp = fmap (AppE (VarE 'toValue) . VarE)

    -- Retrieve all field record names from the convertible type.
    fieldNames :: [String]
    fieldNames = fmap nameBase dataFields

    -- List of pairs :: [(key, value)]
    -- `key` is field record name.
    -- `value` is the data that corresponding field holds.
    pairs :: [Name] -> [Exp]
    pairs = zipWith (\fld val -> tupE' [strToTextE $ fieldLabelModifier fld, val]) fieldNames . valuesExp

    -- Map representation:
    -- mapE = fromList pairs
    -- in terms of Haskell.
    mapE :: [Name] -> Exp
    mapE vars = AppE (VarE 'fromList) (ListE $ pairs vars)

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
    result :: [Name] -> Exp
    result = AppE (AppE (AppE (ConE dataCons) (LitE . IntegerL . fromIntegral $ dummyId)) (fieldFun $ strToTextE label)) . mapE


-- | Describes the body of conversion from target type function.
--
makeFromClause :: String -> Name -> Name -> [Name] -> [Type] -> (String -> String) -> Q Clause
makeFromClause label conName varName dataFields fieldTypes fieldLabelModifier = do
  -- Contains 'True' in each position where 'Maybe a' type occured and 'False' everywhere else.
  let maybeFields = fmap isMaybe fieldTypes

  -- fieldNames :: [Text]
  -- field records of the target type.
  let fieldNames = fmap (pack . fieldLabelModifier . nameBase) dataFields

  -- maybeLabels :: [(Text, Bool)]
  -- field records of the target type and 'isMaybe' check results.
  let maybeNames = zip fieldNames maybeFields

  -- dataLabel :: Text
  -- Label a.k.a type name
  let dataLabel = pack label

  -- Field record names packed in Exp
  -- \x -> [|x|] :: a -> Q Exp
  -- Therefore, fieldNamesE :: [Exp]
  fieldNamesE <- mapM (\x -> [|x|]) fieldNames

  -- maybeNamesE :: [Exp]
  -- Contains Exp representation of (Text, Bool) – field name and isMaybe check result on it.
  let maybeNamesE = zipWith (\n m -> tupE' [n, ConE $ if m then trueName else falseName]) fieldNamesE maybeFields

  -- varExp :: Q Exp
  -- Pattern match variable packed in Exp. It will be used in QuasiQuotation below.
  let varExp = pure (VarE varName)

  -- Guard checks that all necessary fields are present in the container.
  guardSuccess <- NormalG <$> [|checkLabels $(varExp) [dataLabel] && checkProps $(varExp) maybeNames|]

  -- `otherwise` case.
  guardFail <- NormalG <$> [|otherwise|]

  -- Unpack error message.
  failExp <- [|unpackError $(varExp) (unpack dataLabel)|]

  -- Kind of this function realization in terms of Haskell:
  -- fromNode :: Node -> a
  -- fromNode varName | checkLabels varName [dataLabel] && checkProps varName fieldNames = ConName (getProp varName "fieldName1") (getProp varName "fieldName2") ...
  --                  | otherwise = unpackError varName (unpack dataLabel)
  let successExp = foldl (\a f -> AppE a $ AppE (AppE (VarE 'getProp) (VarE varName)) f) (ConE conName) maybeNamesE
  let successCase = (guardSuccess, successExp)
  let failCase = (guardFail, failExp)

  pure $ Clause [VarP varName] (GuardedB [successCase, failCase]) []


-- | Check whether given type is 'Maybe _'
-- It pattern matches type T applied to any argument and checks if T is ''Maybe
isMaybe :: Type -> Bool
isMaybe (AppT (ConT t) _) = t == ''Maybe
isMaybe _                 = False

strToTextE :: String -> Exp
strToTextE str = AppE (VarE 'pack) (LitE . StringL $ str)

checkProps :: Properties t => t -> [(Text, Bool)] -> Bool
checkProps container = all (\(fieldName, fieldMaybe) -> fieldMaybe || fieldName `member` getProps container)

checkLabels :: Labels t => t -> [Text] -> Bool
checkLabels container = all (`elem` getLabels container)

getProp :: (HasCallStack, Properties t, RecordValue a) => t -> (Text, Bool) -> a
getProp container (fieldName, fieldMaybe) | fieldMaybe && fieldName `notMember` getProps container = exactE $ N ()
                                          | otherwise = exactE (getProps container ! fieldName)
  where
    exactE v = case exactEither v of
      Right res -> res
      Left err -> error
        $ "Could not unpack "
        <> unpack fieldName <> ": " <> show err
        <> ", value: " <> show v

unpackError :: HasCallStack => Show c => c -> String -> a
unpackError container label = error $ $currentLoc ++ " could not unpack " ++ label ++ " from " ++ show container

{- $setup
>>> :set -XTemplateHaskell
>>> :set -XOverloadedStrings
>>> import Data.Text (Text)
-}
