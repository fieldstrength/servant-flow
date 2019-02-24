module Servant.Flow


    ( -- ** Classes and basic types
      FlowType
    , Flow (..)
    , genericFlowType
    , renderFlowType
    , generateType
    , FlowObjectKey

    -- ** FlowTypeInfo for primative types
    , primBoolean, primNumber, primString, primAny, primAnyObject, primVoid

    -- ** Names and functions for converting FlowTypeInfo
    , nameless, withName, named, dropAllNames, getTypeName, dropTypeName, renameType

    -- ** Code generation
    , CodeGenOptions (..)
    , defaultCodeGenOptions
    , renderClientFunction
    , generateClientFunction
    , getEndpoints
    , generateFlowClient
    , generateTypeDefinitions

    -- ** Aeson rexports
    , Options (..)
    , defaultOptions
    , SumEncoding (..)
    , defaultTaggedObject

    -- ** Servant HasForeign
    , LangFlow

    , doit
    , Rec (..), X (..)

) where

import           Data.Aeson            (Options (..), SumEncoding (..), defaultOptions,
                                        defaultTaggedObject)
import           Data.Proxy
import           Data.Text             (Text, unpack)
import           Servant.Flow.CodeGen
import           Servant.Flow.Internal
import           Servant.Foreign

import GHC.Generics
import Data.Foldable


data LangFlow

instance Flow a => HasForeignType LangFlow FlowTypeInfo a where
    typeFor _ _ = flowTypeInfo

getEndpoints :: ( HasForeign LangFlow FlowTypeInfo api
                , GenerateList FlowTypeInfo (Foreign FlowTypeInfo api))
               => Proxy api -> [Req FlowTypeInfo]
getEndpoints = listFromAPI (Proxy @LangFlow) (Proxy @FlowTypeInfo)

generateFlowClient :: ( HasForeign LangFlow FlowTypeInfo api
                      , GenerateList FlowTypeInfo (Foreign FlowTypeInfo api))
                   => Proxy api -> CodeGenOptions -> Text
generateFlowClient apiProxy opts
    = execCodeGen opts
    . renderFullClientWithDefs
    $ getEndpoints apiProxy

generateTypeDefinitions :: ( HasForeign LangFlow FlowTypeInfo api
                           , GenerateList FlowTypeInfo (Foreign FlowTypeInfo api))
                        => Proxy api -> CodeGenOptions -> Text
generateTypeDefinitions apiProxy opts
    = execCodeGen opts
    . renderTypeDefs
    $ getEndpoints apiProxy

generateClientFunction :: CodeGenOptions -> Text
generateClientFunction opts = execCodeGen opts renderClientFunction


--genFlowType :: FlowTypeRef -> CodeGen ()
generateType :: forall a. Flow a => Proxy a -> Text
generateType _ = execCodeGen defaultCodeGenOptions . genFlowType . toReferenced . flowTypeInfo $ Proxy @a


data RR = RR { rint :: Int, rbool :: Bool} deriving (Generic, Flow)

data X
    = X Int Bool
    | Y
        { yb  :: Bool
        , yb2 :: Bool}
        deriving (Generic, Flow)

data Rec = Rec
    { recb :: Bool
    , recr :: Maybe Rec
    } deriving (Generic, Flow)

data U = U deriving (Generic, Flow)

doit :: IO ()
doit = traverse_ (putStrLn . unpack)
    [ generateType $ Proxy @X
    , ""
    , generateType $ Proxy @U
    , ""
    --, generateType $ Proxy @Rec
    , ""
    ]
