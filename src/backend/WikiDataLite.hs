module WikiDataLite (readWikiDescs) where

import Logic(Comparison(Eq))
import Description

import Prelude hiding (readFile)

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy (ByteString,readFile)
import Data.Vector (Vector,toList)
import Data.Csv (decode,HasHeader(NoHeader))

citiesFile, countriesFile :: FilePath
citiesFile    = "data/city-list.csv"
countriesFile = "data/country-list.csv"

readWikiDescs :: IO [Desc]
readWikiDescs = do
  countries <- (decode NoHeader <$> readFile countriesFile) :: IO (Either String (Vector (Vector ByteString)))
  cities    <- (decode NoHeader <$> readFile citiesFile)    :: IO (Either String (Vector (Vector ByteString)))
  -- Convert from Either String (Vector (Vector ByteString)) to [[String]]
  let countries'      = map (map unpack) $ map toList $ toList $ fromRight countries
  let cities'         = map (map unpack) $ map toList $ toList $ fromRight cities
  let countriesHeader = head countries'
  let citiesHeader    = head cities'
  let countries''     = map (\x -> appendConcept x "country") $ map (convertToDesc countriesHeader) $ drop 1 countries'
  let cities''        = map (\x -> appendConcept x "city")    $ map (convertToDesc citiesHeader)    $ drop 1 cities'
  return $ concatDescs $ countries'' ++ cities''

convertToDesc :: [String] -> [String] -> Desc
convertToDesc header dat = Object (head dat) [Attr prop Eq val | (prop,val) <- zip (drop 1 header) (drop 1 dat)]

appendConcept :: Desc -> String -> Desc
appendConcept (Object n ps) s = Object n $ Concept s : ps

fromRight :: Either a b -> b
fromRight (Right x) = x
