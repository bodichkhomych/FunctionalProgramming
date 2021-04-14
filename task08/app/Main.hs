module Main where

import           Data.Maybe
import           System.IO

import qualified Text.XML.HaXml            as HX
import           Text.XML.HaXml.Types
import           Text.XML.HaXml.Util
import           Text.XML.HaXml.XmlContent

data CallPrice = CallPrice{
    inner          :: Float,
    otherOperators :: Float,
    landline       :: Float
} deriving (Show)

data Parameters = Parameters{
    connectionCost :: Float,
    favoriteNumber :: Int
} deriving (Show)

data Tariff = Tariff{
    name         :: String,
    operatorName :: String,
    payroll      :: Float,
    callPrices   :: CallPrice,
    smsPrice     :: Float,
    parameters   :: Parameters
} deriving (Show)

newtype Tariffs = Tariffs{
    t :: [Tariff]
} deriving (Show)

instance HTypeable Tariffs where
  toHType (Tariffs ss) = Defined "Tariffs" [] [Constr "tariffs" [] [toHType ss]]

instance HTypeable Tariff where
  toHType st =
    let Tariff n o p c s par = st
     in Defined "Tariff" [] [Constr "tariff" [] [toHType n, toHType o, toHType p, toHType c, toHType s, toHType par]]

instance HTypeable CallPrice where
  toHType (CallPrice i o l) = Defined "CallPrice" [] [Constr "call-price" [] [toHType i, toHType o, toHType l]]

instance HTypeable Parameters where
  toHType (Parameters c f) = Defined "Parameters" [] [Constr "parameters" [] [toHType c, toHType f]]

instance XmlContent Tariff where
  parseContents = do
    e <- element ["tariff"]
    interior
      e
      (Tariff <$> name <*> operatorName <*> payroll <*> parseContents <*> smsPrice <*> parseContents)
    where
      name = inElement "name" text
      operatorName = inElement "operator-name" text
      payroll = read <$> inElement "payroll" text
      smsPrice = read <$> inElement "sms-price" text

instance XmlContent Tariffs where
  parseContents = inElement "tariffs" (Tariffs <$> parseContents)

instance XmlContent CallPrice where
  parseContents = inElement "call-prices" (CallPrice <$> inner <*> otherOperators <*> landline)
    where
      inner = read <$> inElement "inner" text
      otherOperators = read <$> inElement "other-operators" text
      landline = read <$> inElement "landline" text

instance XmlContent Parameters where
  parseContents = inElement "parameters" (Parameters <$> connectionCost <*> favoriteNumber)
    where
      connectionCost = read <$> inElement "connection-cost" text
      favoriteNumber = read <$> inElement "favorite-number" text

main :: IO ()
main = do
  stds <- fReadXml "xml/input_tariff.xml" :: IO Tariffs
  handle <- openFile "xml/input_tariff.xml" ReadMode
  handleDTD <- openFile "xml/verify_tariff.dtd" ReadMode
  str <- hGetContents handle
  strDTD <- hGetContents handleDTD
  let (Just dtd) = HX.dtdParse "xml/verify_tariff.dtd" strDTD
  let (Document _ _ root _) = HX.xmlParse "xml/input_tariff.xml" str
  let errors = HX.validate dtd root
  print errors
  print stds