module Logic where

import System.IO
import Data.List(nub,isPrefixOf)
import Data.Text(replace,pack,unpack)

data Comparison = Eq | Gt | Lt | GtEq | LtEq | Aprx
  deriving Eq
instance Show Comparison where
  show Eq    = "="
  show Gt    = ">"
  show Lt    = "<"
  show GtEq  = "≥"
  show LtEq  = "≤"
  show Aprx  = "≈"

data Prop = Pred1P Name Ind             -- P(x)
          | Conj Prop Prop              -- P & Q
          | Impl Prop Prop              -- P -> Q
          | ForAll (Ind -> Prop)        -- ∀x[...x...]
          | Rel Value Comparison Value  -- X <> Y

data Value = Val String                 -- "abc"
           | Func1P Name Ind            -- F(x)
  deriving Eq

instance Show Value where
  show (Val s)      = s
  show (Func1P n x) = n ++ pth x

type Name = String
type Pred = String
type Ind  = String

-- A predefined individual for unknown individuals.
unknown :: Ind
unknown = "<?>"
-- A predefined individual for placeholder individuals.
-- Used for non-evaluating computations on quantifiers.
placeholder :: Ind
placeholder = "<P>"

-- A sentence will create a proposition from left to right
-- When an unknown individual is found, create new propositions
-- based on the alternatives
buildContexts :: Prop -> [Prop]
buildContexts p = fst $ build p []
  where
    build :: Prop -> [Ind] -> ([Prop],[Ind])
    build (Pred1P n x)     is | x == unknown                 = ([Pred1P n x' | x' <- is],              is)
                              | otherwise                    = ([Pred1P n x],                          nub $ x:is)
    build (Conj p1 p2)    is = let (p1s,is') = build p1 is in
      let (p2s, is'') = build p2 is' in ([Conj x y | x<-p1s, y<-p2s], is'')
    build (Impl p1 p2)    is = let (p1s,is') = build p1 is in
      let (p2s, is'') = build p2 is' in ([Impl x y | x<-p1s, y<-p2s], is'')
    build (ForAll f)      is = buildQ ForAll f is
    build (Rel v1 cmp v2) is = let (v1s,is') = buildV v1 is in
      let (v2s, is'') = buildV v2 is' in ([Rel x cmp y | x<-v1s, y<-v2s], is'')

    buildQ :: ((Ind -> Prop) -> Prop) -> (Ind -> Prop) -> [Ind] -> ([Prop],[Ind])
    buildQ q f is = let p = f placeholder in
      let (ps',is') = build p is in
        (map q $ map propToQuantified ps', [i | i<-is', i /= placeholder])

    propToQuantified :: Prop -> (Ind -> Prop)
    propToQuantified (Pred1P n y)    = \x -> Pred1P n $ head $ replaceArguments [y] [x]
    propToQuantified (Conj p1 p2)    = \x -> Conj (replacePlaceholder p1 x) (replacePlaceholder p2 x)
    propToQuantified (Impl p1 p2)    = \x -> Impl (replacePlaceholder p1 x) (replacePlaceholder p2 x)
    propToQuantified (ForAll f)      = \x -> ForAll $ \y -> replacePlaceholder (f y) x
    propToQuantified (Rel v1 cmp v2) = \x -> Rel (replacePlaceholderInValue v1 x) cmp (replacePlaceholderInValue v2 x)

    replacePlaceholder :: Prop -> Ind -> Prop
    replacePlaceholder (Pred1P n y)     x = Pred1P n $ head $ replaceArguments [y] [x]
    replacePlaceholder (Conj p1 p2)    x = Conj (replacePlaceholder p1 x) (replacePlaceholder p2 x)
    replacePlaceholder (Impl p1 p2)    x = Impl (replacePlaceholder p1 x) (replacePlaceholder p2 x)
    replacePlaceholder (ForAll f)      x = ForAll $ \y -> replacePlaceholder (f y) x
    replacePlaceholder (Rel v1 cmp v2) x = Rel (replacePlaceholderInValue v1 x) cmp (replacePlaceholderInValue v2 x)

    replacePlaceholderInValue :: Value -> Ind -> Value
    replacePlaceholderInValue (Func1P n y) x = Func1P n $ head $ replaceArguments [y] [x]
    replacePlaceholderInValue (Val v)      x = Val $ head $ replaceArguments [v] [x]

    buildV :: Value -> [Ind] -> ([Value],[Ind])
    buildV (Func1P n x) is | x == unknown = ([Func1P n x' | x' <- is], is)
                           | otherwise    = ([Func1P n x],             nub $ x:is)
    buildV (Val s) is = if s == unknown then ([Val x | x <- is],is) else ([Val s], is ++ [s])

    -- replaceArguments l1 l2 replaces all placeholder occurences in l1 with elements from l2
    replaceArguments :: [Ind] -> [Ind] -> [Ind]
    replaceArguments (x:xs) (x':xs') | x == placeholder              = x' : replaceArguments xs xs'
                                     | placeholder `isSubstringOf` x = unpack (replace (pack placeholder) (pack x') (pack x)) : replaceArguments xs xs'
                                     | otherwise                     = x : replaceArguments xs (x':xs')
    replaceArguments [] xs' = []
    replaceArguments xs []  = xs

    isSubstringOf :: String -> String -> Bool
    isSubstringOf s1 (s:s2) = s1 `isPrefixOf` (s:s2) || s1 `isSubstringOf` s2
    isSubstringOf _  []     = False

-- Given a proposition; output the corresponding string on stdout
printProp :: Prop -> IO ()
printProp prop = hSetEncoding stdout utf8 >> putStrLn (trimPth $ toS 0 prop)
  where
    toS :: Int -> Prop -> String
    toS _ (Pred1P p x)    = p ++ pth x
    toS x (Conj p1 p2)    = pth $ toS x p1 ++ " ∧ " ++ toS x p2
    toS x (Impl p1 p2)    = pth $ toS x p1 ++ " ⇒ " ++ toS x p2
    toS x (ForAll f)      = "∀" ++ var x ++ "[" ++ toS (x+1) (f (var x)) ++ "]"
    toS x (Rel v1 cmp v2) = show v1 ++ " " ++ show cmp ++ " " ++ show v2
    var :: Int -> String
    var i = "x" ++ getSubscript i
    getSubscript :: Int -> String
    getSubscript i | i < 0     = ""
                   | i < 10    = (subs !! i) : []
                   | otherwise = getSubscript (i `div` 10) ++ getSubscript (i `mod` 10)
    subs :: [Char]
    subs = "₀₁₂₃₄₅₆₇₈₉"

printPropLn :: Prop -> IO ()
printPropLn p = printProp p >> putStrLn ""

pth :: String -> String
pth s = "(" ++ s ++ ")"

trimPth :: String -> String
trimPth (c:cs) = if c == '(' then (init cs) else (c:cs)
