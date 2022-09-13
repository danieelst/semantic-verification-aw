{-
  Indvidual => object with properties
-}

module Description where

import Data.Maybe
import Data.List(nub)
import qualified Logic as L

type Name = String

-- Add comparison attr to another attribute
data Property = Concept Name
              | Attr Name L.Comparison L.Ind
              | Constr Name L.Comparison L.Value
  deriving Eq

instance Show Property where
  show (Concept name)  = name
  show (Attr name cmp x) = name ++ show cmp ++ x
  show (Constr name cmp val) = name ++ show cmp ++ show val

data Desc = Object L.Ind [Property]
          | Cond [Desc] [Desc]
          | All  (L.Ind -> [Desc])
instance Show Desc where
  show desc = case desc of
      Object ind ps      -> ind ++ "{" ++ (drop 1 $ init $ show ps) ++ "}"
      Cond ds1 ds2       -> show ds1 ++ "=>" ++ show ds2
      All  f             -> "All(" ++ "x₀" ++ ")"  ++ show (f "x₀")
instance Eq Desc where
  Object i1 ps1      == Object i2 ps2         = i1 == i2 && and [p `elem` ps2 | p<-ps1]
  Cond ds1 ds2       == Cond ds1' ds2'        = ds1 == ds1' && ds2 == ds2'
  All f1             == All f2                = (f1 L.placeholder) == (f2 L.placeholder)
  _                  == _                     = False

showDescs :: [Desc] -> String
showDescs descs = drop 1 $ init $ show descs

-- Remove empty descriptions and duplicates here instead
toDescriptions :: L.Prop -> [Desc]
toDescriptions p = toDesc p $ map (\x -> Object x []) $ collectIndividuals p
  where
    toDesc :: L.Prop -> [Desc] -> [Desc]
    toDesc p ds = concatDescs $ nub $ removeEmptyDescs $ buildDescs p ds
    buildDescs :: L.Prop -> [Desc] -> [Desc]
    buildDescs (L.Pred1P n x)    ds = map (addProp (Concept n) x) ds
    buildDescs (L.Conj p1 p2)    ds = let ds' = buildDescs p1 ds in buildDescs p2 ds'
    buildDescs (L.Impl p1 p2)    ds = ds ++ [Cond (toDesc p1 $ buildEmptyObjects p1) (toDesc p2 $ buildEmptyObjects p2)]
    buildDescs (L.ForAll f)      ds = ds ++ [All $ \x -> toDesc (f x) $ buildEmptyObjects $ f x]
    buildDescs (L.Rel v1 cmp v2) ds = propFromValues v1 v2 cmp ds
    propFromValues :: L.Value -> L.Value -> L.Comparison -> [Desc] -> [Desc]
    propFromValues (L.Val s)        (L.Func1P n x)   cmp ds = propFromValues (L.Func1P n x) (L.Val s) cmp ds -- Just flip around to prevent code duplication
    propFromValues (L.Func1P n x)   (L.Val s)        cmp ds = map (addProp (Attr n cmp s) x) ds
    propFromValues (L.Func1P n1 x1) (L.Func1P n2 x2) cmp ds = map (addProp (Constr n1 cmp (L.Func1P n2 x2)) x1) ds --ds -- should probably generate descriptions later
    propFromValues (L.Val s1)       (L.Val s2)       cmp ds = ds -- generates no descriptions
    addProp :: Property -> L.Ind -> Desc -> Desc
    addProp prop ind' (Object ind props)
      | ind == ind' = Object ind $ props ++ [prop]
      | otherwise   = Object ind props
    addProp prop ind' (All f) = All f
    buildEmptyObjects :: L.Prop -> [Desc]
    buildEmptyObjects p = map (\y -> Object y []) $ collectIndividuals p
    collectIndividuals :: L.Prop -> [L.Ind]
    collectIndividuals p = nub $ collect p
      where
        collect :: L.Prop -> [L.Ind]
        collect (L.Pred1P _ x)   = [x]
        collect (L.Conj p1 p2)   = collect p1 ++ collect p2
        collect (L.Impl p1 p2)   = collect p1 ++ collect p2
        collect (L.ForAll f)     = [x | x<- (collect $ f L.placeholder), x /= L.placeholder]
        collect (L.Rel v1 _ v2)  = collectIndividualsV v1 ++ collectIndividualsV v2
        collectIndividualsV :: L.Value -> [L.Ind]
        collectIndividualsV (L.Val s)      = [s]
        collectIndividualsV (L.Func1P _ x) = [x]

removeEmptyDescs :: [Desc] -> [Desc]
removeEmptyDescs (Object n ps : ds) | null ps   = removeEmptyDescs ds
                                    | otherwise = Object n ps : removeEmptyDescs ds
removeEmptyDescs (Cond ds1 ds2 : ds) | null ds2'              = removeEmptyDescs ds
                                     | null ds1'              = ds2' ++ removeEmptyDescs ds
                                     | otherwise              = Cond ds1' ds2' : removeEmptyDescs ds
  where
    ds1' = removeEmptyDescs ds1
    ds2' = removeEmptyDescs ds2
removeEmptyDescs (All f : ds) | null $ f L.placeholder = removeEmptyDescs ds
                              | otherwise              = All (\x -> removeEmptyDescs $ f x) : removeEmptyDescs ds
removeEmptyDescs []     = []

concatDescs :: [Desc] -> [Desc]
concatDescs (d:ds) = case d of
    Object n ps  -> Object n (concatProps $ ps ++ takeProps ds n) : (concatDescs $ removeDescs ds n)
    Cond ds1 ds2 -> Cond (concatDescs ds1) (concatDescs ds2) : concatDescs ds
    All f        -> (All $ \x -> concatDescs $ f x) : concatDescs ds
  where
    takeProps :: [Desc] -> L.Ind -> [Property]
    takeProps (d:ds) n = case d of
      Object n' ps -> if n == n' then ps ++ takeProps ds n else takeProps ds n
      _            -> takeProps ds n
    takeProps [] _ = []
    removeDescs :: [Desc] -> L.Ind -> [Desc]
    removeDescs (d:ds) n = case d of
      Object n' _ -> if n == n' then removeDescs ds n else d : removeDescs ds n
      _           -> d : removeDescs ds n
    removeDescs [] _ = []
concatDescs [] = []

concatProps :: [Property] -> [Property]
concatProps (p:ps) = p : (concatProps $ removeAllOccurrences ps p)
  where
    removeAllOccurrences :: [Property] -> Property -> [Property]
    removeAllOccurrences (p:ps) p' | p == p'   = removeAllOccurrences ps p'
                                   | otherwise = p : removeAllOccurrences ps p'
    removeAllOccurrences []     _  = []
concatProps [] = []
