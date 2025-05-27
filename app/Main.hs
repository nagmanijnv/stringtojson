module Main where

import Data.Aeson
import Data.Char
import qualified Data.HashMap.Strict as DHS
import qualified Data.List.Extra as DLE
import Data.Maybe
import Data.Scientific
import qualified Data.Set as DS
import Data.Text as DT
import Data.Vector as DV
import GHC.Generics
import Integer.Integer
import qualified Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Servant
import Text.Read hiding (String)

type Record = DHS.HashMap Text Value

data ErrorCode =
  BooleanError
  | DateFormatError
  | LengthError
  | ValueNotArrayError
  | NumberFormatError
  | ABLengthError
  | InvalidABCode
  | NoError
  deriving (Eq, Ord, Show)

data ErrorResp = ErrorResp {
  status :: Bool,
  message :: String,
  errorMessage :: [String]
} deriving (Show, Generic, FromJSON, ToJSON )

data Payload = Payload {
  encodedString :: String
} deriving (Show, Generic, FromJSON, ToJSON )

type StringToJsonAPI = "stringtojson" :> ReqBody '[JSON] Payload :> Post '[JSON] Value


main :: IO ()
main = do
  putStrLn "server started at port: 8081"
  run 8081 app

stringToJsonAPI :: Proxy StringToJsonAPI
stringToJsonAPI = Proxy

server :: Server StringToJsonAPI
server = hoistServer stringToJsonAPI id requestHandler

app :: Application
app = serve stringToJsonAPI server

-- handler for the request
requestHandler :: Payload -> Handler Value
requestHandler Payload {encodedString} = do
  let
    hashmap :: Record = DHS.empty
    vp = Prelude.filter (/= "") $ collectKeyValuePair encodedString 
    final = Prelude.foldl (
        \acc val -> do
          let ab = getAB $ Prelude.take 2 val
          if fst ab then
            handleIsArrayAndType (snd ab) (Prelude.drop 2 val) acc
          else do
            let 
              (resp, err) = acc
            (resp, DS.insert ABLengthError err)
      ) (hashmap, DS.empty :: DS.Set ErrorCode) vp
    errorset = snd final
  if DS.null errorset
    then pure $ toJSON $ fst final
  else throwError $ ServerError {
      errHTTPCode = 400,
      errReasonPhrase = "body string format error",
      errBody = encode $ makeErrorResp errorset,
      errHeaders = [(H.hContentType, "application/json")]
    }

makeErrorResp :: DS.Set ErrorCode -> ErrorResp
makeErrorResp errs = ErrorResp {
  status = False,
  message = "parser failed, something wrong with the input",
  errorMessage = fmap errorCodeToMsg (DS.toList errs)
}

errorCodeToMsg :: ErrorCode -> String
errorCodeToMsg code =
  case code of 
    BooleanError -> "boolean values have some issue"
    DateFormatError -> "date format has some issue"
    LengthError -> "key value combination has some issue"
    ValueNotArrayError -> "some specified values are not array"
    NumberFormatError -> "number format error"
    ABLengthError -> "ab length is not as per expected"
    InvalidABCode -> "ab code is not valid"
    NoError -> ""

collectKeyValuePair :: String -> [String]
collectKeyValuePair = DLE.split (=='#')

getAB :: String -> (Bool, (Int, Int))
getAB val = do
  let 
    number = readMaybe val :: Maybe Int
  maybe (False, (10, 10)) (\num -> (True, (num `div` 10, num `mod` 10))) number

handleIsArrayAndType :: (Int, Int) -> String -> (Record, DS.Set ErrorCode) -> (Record, DS.Set ErrorCode)
handleIsArrayAndType tup str final = 
  case tup of
    (0, 0) -> keyValueToHashmap tup splittedList final
    (1, 0) -> keyValuesToHashmap tup splittedList final
    (0, 1) -> keyValueToHashmap tup splittedList final
    (1, 1) -> keyValuesToHashmap tup splittedList final
    (0, 2) -> keyValueToHashmap tup splittedList final
    (1, 2) -> keyValuesToHashmap tup splittedList final
    (0, 3) -> keyValueToHashmap tup splittedList final
    (1, 3) -> keyValuesToHashmap tup splittedList final
    _      -> do
      let
        (record, err) = final
      (record, DS.insert InvalidABCode err)
  where
    splittedList = DLE.split (=='|') str

-- handling normal/single values
keyValueToHashmap :: (Int, Int) -> [String] -> (Record, DS.Set ErrorCode) -> (Record, DS.Set ErrorCode)
keyValueToHashmap (_, b) str res =
  if Prelude.length str == 2
  then do
    let
      record = fst res
      err = snd res
      key = str!!0
      val = str!!1
    if b == 3 then
      if checkBoolValid val then 
        if Prelude.and $ fmap (\item -> Prelude.elem item booleanTrue) val then (DHS.insert (DT.pack key) (Bool True) record, err)
        else (DHS.insert (DT.pack key) (Bool False) record, err)
      else (record, DS.insert BooleanError err)
    else if b == 0 then
      if checkDateValidity val then
        (DHS.insert (DT.pack key) (String $ DT.pack val) record, err)
      else (record, DS.insert DateFormatError err)
    else if b == 1 then do
      let 
        numparsed = numberParser val
      if fst numparsed  then do
        let num = fromJust $ snd numparsed
        (DHS.insert (DT.pack key) (Data.Aeson.Number num) record, err)
      else (record, DS.insert NumberFormatError err)
    else (DHS.insert (DT.pack key) (String $ DT.pack val) record, err)
  else (fst res, DS.insert LengthError $ snd res)

-- handling array type values
keyValuesToHashmap :: (Int, Int) -> [String] -> (Record, DS.Set ErrorCode) -> (Record, DS.Set ErrorCode)
keyValuesToHashmap (_, b) str res =
  if Prelude.length str == 2
  then do
    let
      record = fst res
      err = snd res
      key = str!!0
      val = str!!1
    if Prelude.length (DLE.split (==',') val) > 1 then
      if b == 3 then
        if checkArrayBoolValid val then
          (
            DHS.insert
              (DT.pack key)
              (Array $ DV.fromList $ fmap (
                  \items ->
                    if Prelude.and $ fmap (
                        \item -> Prelude.elem item booleanTrue
                      ) items 
                    then Bool True
                    else Bool False
                  ) $ DLE.split (==',') val)
              record
            , 
            err
          )
        else (record, DS.insert BooleanError err)
      else if b == 0 then
        if checkArrayDateValidity val then
          (
            DHS.insert 
              (DT.pack key) 
              (Array $ DV.fromList $
                fmap (String . DT.pack) $ DLE.split (==',') val) 
              record
            ,
            err
          )
        else (record, DS.insert DateFormatError err)
      else if b == 1 then
        if checkArrayNumberValid val then do
          let numparsedlist = fmap (Data.Aeson.Number . fromJust . snd . numberParser) $ DLE.split (==',') val
          (DHS.insert (DT.pack key) (Array $ DV.fromList $ numparsedlist) record, err)            
        else
          (record, DS.insert NumberFormatError err)
      else
        ( DHS.insert
            (DT.pack key) 
            (Array $ DV.fromList $
              fmap (String . DT.pack) $ DLE.split (==',') val
            )
            record
        , err
        )
    else (record, DS.insert ValueNotArrayError err)
  else (fst res, DS.insert LengthError $ snd res)

-- check for date validity
checkArrayDateValidity :: String -> Bool
checkArrayDateValidity str = do
  let
    datelist = DLE.split (==',') str
  Prelude.and $ fmap checkDateValidity datelist

checkDateValidity :: String -> Bool
checkDateValidity str = do
  let
    yymmdd = DLE.split (=='-') str
  if Prelude.length yymmdd == 3 then do
    let
      year = yymmdd!!0
      month = yymmdd!!1
      day = yymmdd!!2
    checkYearValid year && checkMonthValid month && checkDayValid day
  else False
  
checkYearValid :: String -> Bool
checkYearValid str = Prelude.length str == 4 && (Prelude.and $ fmap (isDigit) str)

checkMonthValid :: String -> Bool
checkMonthValid str = Prelude.length str == 2 && (Prelude.and $ fmap (isDigit) str)

checkDayValid :: String -> Bool
checkDayValid str = Prelude.length str == 2 && (Prelude.and $ fmap (isDigit) str)

-- check for boolean string validity
booleanTrue :: [Char]
booleanTrue = ['y', 'Y', 't', 'T']

booleanFalse :: [Char]
booleanFalse = ['n', 'N', 'f', 'F']

checkArrayBoolValid :: String -> Bool
checkArrayBoolValid str = do
  let
    boollist = DLE.split (==',') str
  Prelude.and $ fmap checkBoolValid boollist

checkBoolValid :: String -> Bool
checkBoolValid str = Prelude.length str == 1 && (Prelude.and $ fmap (\item -> Prelude.or [Prelude.elem item booleanTrue, Prelude.elem item booleanFalse]) str)

-- check for number validity
checkArrayNumberValid :: String -> Bool
checkArrayNumberValid str = do
  let
    numberlist = DLE.split (==',') str
  Prelude.and $ fmap (fst . numberParser) numberlist

numberParser :: String -> (Bool, Maybe Scientific)
numberParser str =
  maybe (False, Nothing) (
    \dval -> do
      if dval == ((toEnum . fromEnum) dval :: Double) then (True, (Just $ (flip scientific 0 . fromInt . fromEnum) dval) :: Maybe Scientific)
      else (True, Just $ fromFloatDigits dval)
    ) 
    (readMaybe str :: Maybe Double)