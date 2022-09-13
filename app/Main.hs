module Main where

import Logic
import Description
import Verification
import CountriesGrammar
import CountriesSemantics
import WikiDataLite

import Data.Char(isAlpha)
import Data.List(nub,isSuffixOf)
import System.FilePath(isValid)

import PGF
import GfUtils

main :: IO ()
main = do
  gr <- readPGF "data/Countries.pgf"
  let Just eng = readLanguage "CountriesEng"
  let Just cat = readType "Doc"
  lang <- selectLanguage
  let Just l   = readLanguage $ "Countries" ++ lang
  putStrLn "\nInput text or path to a text file...\n"
  s <- getLine
  content <- readFileOrNot s
  let ps = parse gr l cat content
  case ps of
    p:_ -> analyzeParses ps gr eng
    _ -> putStrLn "parse error"

analyzeParses :: [Tree] -> PGF -> Language -> IO ()
analyzeParses (p:ps) pgf lang = do
    dat <- readWikiDescs

    let p'           = replaceMetaElements p
    let tree         = fg p'
    let formulas     = iDoc (pgf,lang) tree
    let contexts     = buildContexts formulas

    putStrLn ""
    putStrLn $ take 80 $ repeat '-'
    putStrLn "Parsing...\n"
    putStrLn $ showExpr [] p
    putStrLn "\nReplacing pronouns...\n"
    putStrLn $ showExpr [] p'
    putStrLn "\nConverting into grammar...\n"
    putStrLn $ show tree
    putStrLn "\nInterpreting...\n"
    printProp formulas
    putStrLn "\nNaïve context-building..."
    putStrLn $ show (length contexts) ++ " context(s) generated"
    sequence [verifyContext context num dat | (context,num) <- zip contexts [1..]]

    analyzeParses ps pgf lang
analyzeParses [] _ _ = return ()

verifyContext :: Prop -> Int -> [Desc] -> IO ()
verifyContext context num dat = do
  let descs = toDescriptions context
  let judgs = map (\x -> verifyDesc x dat) descs
  let len   = foldr1 max [length (show d) | d <- descs]
  putStrLn $ take 40 $ repeat '-'
  putStrLn $ "Context #" ++ show num ++ ":\n"
  printPropLn context
  putStrLn "Converting into descriptions...\n"
  putStrLn $ showDescs $ descs
  putStrLn "\nVerifying descriptions...\n"
  putStrLn $ concat [show d ++ (take (len - (length $ show d)) (repeat ' ')) ++ " => " ++ show j ++ "\n" | (d,j) <- zip descs judgs]

selectLanguage :: IO String
selectLanguage = do
  putStrLn "\nSelect language (Eng/Swe):\n"
  s <- getLine
  if s == "Eng" || s == "Swe"
    then return s
    else selectLanguage

readFileOrNot :: String -> IO String
readFileOrNot s | isValid s && ".txt" `isSuffixOf` s = fmap replaceNewLine $ readFile s
                | otherwise = return s
  where
    replaceNewLine :: String -> String
    replaceNewLine s = map (\c -> if c == '\n' then ' ' else c) s
