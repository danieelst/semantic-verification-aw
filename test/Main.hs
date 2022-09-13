module Main where

import Logic
import Description
import Verification
import CountriesGrammar
import CountriesSemantics
import WikiDataLite

import Data.List(maximumBy)
import Data.Ord(comparing)

import PGF

main :: IO ()
main = do
  dat <- readWikiDescs
  js  <- sequence $ map sanityCheck dat
  let js' = concat js
  putStrLn $ (show $ length $ filter (==Valid)   js') ++ " properties are OK"
  putStrLn $ (show $ length $ filter (==Unknown) js') ++ " properties could not be checked"
  putStrLn $ (show $ length $ filter (==Invalid) js') ++ " properties are not OK"

-- Simply take a description from the data model and check if it is valid against itself
sanityCheck :: Desc -> IO [Judgement]
sanityCheck (Object n ps) = do
    let statements = map (\x -> propertyToText x n) ps
    sequence $ map (\s -> checkText s (Object n ps)) statements
  where
    propertyToText :: Property -> String -> String
    propertyToText (Attr attr _ val) name = "the " ++ attr ++ " of " ++ name ++ " is " ++ val ++ " ."
    propertyToText (Concept n)       name = name ++ " is a " ++ n ++ " ."

checkText :: String -> Desc -> IO Judgement
checkText statement desc = do
    pgf <- readPGF "data/Countries.pgf"
    let Just eng = readLanguage "CountriesEng"
    let Just cat = readType "Doc"

    let pr  = map fg $ parse pgf eng cat statement -- [Expr]
    if null pr
      then do
        putStrLn $ "\"" ++ statement ++ "\"" ++ " = ?"
        return Unknown
      else do
        let pr' = map (iDoc (pgf,eng)) pr                             -- [Prop]
        let dss = map toDescriptions pr'                              -- [[Desc]]
        let js  = map (\ds -> map (\x -> verifyDesc x [desc]) ds) dss -- [[Judgment]]
        let vs  = map judgmentVals js                                 -- [Integer]
        let j   = cmb $ fst $ maximumBy (comparing snd) $ zip js vs   -- Judgment
        putStrLn $ "\"" ++ statement ++ "\"" ++ " = " ++ show j
        return j
  where
    judgmentVals :: [Judgement] -> Integer
    judgmentVals (j:js) = case j of
      Valid   -> 2 + judgmentVals js
      Unknown -> 1 + judgmentVals js
      Invalid -> 0 + judgmentVals js
    judgmentVals [] = 0
    cmb :: [Judgement] -> Judgement
    cmb (j:js) = case j of
      Valid   -> cmb js
      Unknown -> if cmb js /= Invalid then Unknown else Invalid
      Invalid -> Invalid
    cmb [] = Valid
