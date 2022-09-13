module Verification where

import Logic(Value(..),Comparison(..),Ind)
import Data.List(nub)
import Data.Maybe(Maybe(..),fromJust)
import Text.Read(readMaybe)
import Description(Desc(..),Property(..),removeEmptyDescs,Name)

data Judgement = Valid | Invalid | Unknown
  deriving Eq

instance Show Judgement where
  show Valid   = "✓"
  show Invalid = "X"
  show Unknown = "?"

-- verifyDesc d1 d2 holds if:
--   * name(d1) == name(d2)
--   * properties(d1) ⊆ properties(d2)
verifyDesc :: Desc -> [Desc] -> Judgement
verifyDesc (Object n ps)  dat = verifyObject (Object n ps)  dat
verifyDesc (Cond ds1 ds2) dat = verifyCond   (Cond ds1 ds2) dat
verifyDesc (All f)        dat = verifyAll    (All f)        dat

verifyAll :: Desc -> [Desc] -> Judgement
verifyAll d dat = verifyAll' d dat dat
  where
    verifyAll' :: Desc -> [Desc] -> [Desc] -> Judgement
    verifyAll' (All f) (Object n ps : ds) dat = cmbAll $ [verifyDesc d dat | d <- f n] ++ [verifyAll' (All f) ds dat]
    verifyAll' (All f) []                 dat = Valid
    cmbAll :: [Judgement] -> Judgement
    cmbAll (j:js) = case j of
      Valid   -> cmbAll js
      Invalid -> Invalid
      Unknown -> if cmbAll js /= Invalid then Unknown else Invalid
    cmbAll []     = Valid

verifyCond :: Desc -> [Desc] -> Judgement
verifyCond (Cond ds1 ds2) dat = case cmbCond (map (\d -> verifyDesc d dat) ds1) of
    Valid     -> cmbCond $ map (\d -> verifyDesc d dat) ds2
    otherwise -> Valid
  where
    cmbCond :: [Judgement] -> Judgement
    cmbCond (j:js) = case j of
      Valid   -> cmbCond js
      Invalid -> Invalid
      Unknown -> if cmbCond js /= Invalid then Unknown else Invalid
    cmbCond []     = Valid

verifyObject :: Desc -> [Desc] -> Judgement
verifyObject (Object n ps) dat = cmbObj $ map (\x -> verifyProperty x n dat) ps
  where
    cmbObj :: [Judgement] -> Judgement
    cmbObj (j:js) = case j of
      Valid   -> cmbObj js
      Invalid -> Invalid
      Unknown -> if cmbObj js /= Invalid then Unknown else Invalid
    cmbObj []     = Valid

verifyProperty :: Property -> Name -> [Desc] -> Judgement
verifyProperty (Constr attr cmp v) n dat | attr /= getAttrInVal v             = Invalid
                                         | not ((getIndInVal v) `elem` names) = Unknown
                                         | not (n `elem` names)               = Unknown
                                         | otherwise                          = cmb [cmpAttrs left right | left <- lefts, right <- rights]
  where
    lefts :: [Property]
    lefts = [Attr attr cmp (getValInAttr attr') | attr' <- getEqAttr (getAttr (getIndInVal v) attr dat)]  -- e.g. population(China) = "1.4 billion" => population(x) <= "1.4 billion"
    rights :: [Property]
    rights = getAttr n attr dat                                                                 -- e.g. population(x) = "12345"
    names :: [Ind]
    names = [n | (Object n _) <- dat]
    getIndInVal :: Value -> Ind
    getIndInVal  (Func1P _ x) = x
    getAttrInVal :: Value -> Name
    getAttrInVal (Func1P f _) = f
    getValInAttr :: Property -> Ind
    getValInAttr (Attr _ _ x) = x
    getAttr :: Name -> Name -> [Desc] -> [Property]
    getAttr name attr dat = concat $ [[Attr attr' cmp x | (Attr attr' cmp x) <- ps, attr==attr'] | (Object name' ps) <- dat, name==name']
    getEqAttr :: [Property] -> [Property]
    getEqAttr ((Attr attr Eq val):ps) = (Attr attr Eq val) : getEqAttr ps
    getEqAttr (_                 :ps) = getEqAttr ps
    getEqAttr []                      = []
verifyProperty p                   n dat = cmb [propInProperties p ps | (Object n' ps) <- dat, n==n']

cmb :: [Judgement] -> Judgement
cmb (j:js) = case j of
  Valid   -> Valid
  Invalid -> if cmb js /= Valid then Invalid else Valid
  Unknown -> cmb js
cmb []     = Unknown

-- propInProperties p ps holds if:
--   * p ∈ ps
propInProperties :: Property -> [Property] -> Judgement
propInProperties (Concept name)    (p:ps) = case p of
  Concept name' -> if name == name' then Valid else propInProperties (Concept name) ps
  _             -> propInProperties (Concept name) ps
propInProperties (Attr name cmp x) (p:ps) = case p of
  Attr name' cmp' x' -> if name == name'
    then case cmpAttrs (Attr name cmp x) (Attr name' cmp' x') of
                          Valid   -> Valid
                          Invalid -> if propInProperties (Attr name cmp x) ps /= Valid then Invalid else Valid
                          Unknown ->    propInProperties (Attr name cmp x) ps
    else propInProperties (Attr name cmp x) ps
  _                  -> propInProperties (Attr name cmp x) ps
propInProperties _                 []     = Unknown

-- Right side holds the truth to verify against
-- Left side is the claim
-- Pattern matching doesn't get better than this!
-- Apologies to all future code archeologists!
-- One could make the argument that this is not very pretty not good!
cmpAttrs :: Property -> Property -> Judgement
cmpAttrs attr1 attr2 | isAttr attr1 && isAttr attr2 = cmpAttrs' attr1 attr2
                     | otherwise                    = Invalid
  where
    isAttr :: Property -> Bool
    isAttr (Attr _ _ _) = True
    isAttr _            = False
    convert :: String -> Maybe Double
    convert s = readMaybe s :: Maybe Double
    convertAndCompare :: Ind -> (Double -> Double -> Bool) -> Ind -> Bool
    convertAndCompare x cmp y | convert x == Nothing || convert y == Nothing = False
                              | otherwise                                    = cmp (fromJust $ convert x) (fromJust $ convert y)
    aprx :: Double -> (Double,Double)
    aprx i = (i * 0.7, i * 1.3)
    check :: Bool -> Judgement
    check b | b         = Valid
            | otherwise = Invalid
    cmpAttrs' :: Property -> Property -> Judgement
    cmpAttrs' (Attr n1 Gt   v1) (Attr n2 Gt   v2) = check $ n1 == n2 && v1 == v2
    cmpAttrs' (Attr n1 Lt   v1) (Attr n2 Lt   v2) = check $ n1 == n2 && v1 == v2
    cmpAttrs' (Attr n1 GtEq v1) (Attr n2 GtEq v2) = check $ n1 == n2 && v1 == v2
    cmpAttrs' (Attr n1 LtEq v1) (Attr n2 LtEq v2) = check $ n1 == n2 && v1 == v2
    cmpAttrs' (Attr n1 Aprx v1) (Attr n2 Aprx v2) = check $ n1 == n2 && v1 == v2
    cmpAttrs' _                 (Attr _  Gt   _)  = Unknown
    cmpAttrs' _                 (Attr _  Lt   _)  = Unknown
    cmpAttrs' _                 (Attr _  GtEq _)  = Unknown
    cmpAttrs' _                 (Attr _  LtEq _)  = Unknown
    cmpAttrs' (Attr n1 Eq   v1) (Attr n2 Eq   v2) = check $ n1 == n2 && v1 == v2
    cmpAttrs' (Attr n1 Gt   v1) (Attr n2 Eq   v2) = check $ n1 == n2 && convertAndCompare v1 (<) v2
    cmpAttrs' (Attr n1 Lt   v1) (Attr n2 Eq   v2) = check $ n1 == n2 && convertAndCompare v1 (>) v2
    cmpAttrs' (Attr n1 Aprx v1) (Attr n2 Eq   v2) = let (lower,upper) = aprx (fromJust $ convert v1) in check $ n1 == n2 && convertAndCompare v2 (>) (show lower) && convertAndCompare v2 (<) (show upper)
    cmpAttrs' (Attr n1 Gt   v1) (Attr n2 Aprx v2) = let (lower,_) = aprx (fromJust $ convert v2) in check $ n1 == n2 && convertAndCompare v1 (<) (show lower)
    cmpAttrs' (Attr n1 Lt   v1) (Attr n2 Aprx v2) = let (_,upper) = aprx (fromJust $ convert v2) in check $ n1 == n2 && convertAndCompare v1 (>) (show upper)
    cmpAttrs' (Attr n1 Eq   v1) (Attr n2 Aprx v2) = Unknown
    cmpAttrs' (Attr n GtEq  v)  a                 = cmpAttrs' (Attr n Gt v) a `jor` cmpAttrs' (Attr n Eq v) a
    cmpAttrs' (Attr n LtEq  v)  a                 = cmpAttrs' (Attr n Lt v) a `jor` cmpAttrs' (Attr n Eq v) a
    jor :: Judgement -> Judgement -> Judgement
    jor Valid _   = Valid
    jor _ Valid   = Valid
    jor Invalid _ = Invalid
    jor _ Invalid = Invalid
    jor _ _       = Unknown
