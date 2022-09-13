module CountriesSemantics where

import Logic
import CountriesGrammar

import PGF
import Data.Maybe(fromJust)

import Data.Text(pack,unpack,replace)

iDoc :: (PGF,Language) -> GDoc -> Prop
iDoc gf (GAddSentenceDoc doc s) = Conj (iDoc gf doc) (iS gf s)
iDoc gf (GOneSentenceDoc s)     = iS gf s

-- A sentence contains a statement, i.e. iS gf a fact, and thus iS gf a proposition
iS :: (PGF,Language) -> GSentence -> Prop
iS gf (GConjSentence s1 s2) = Conj (iS gf s1) (iS gf s2)
iS gf (GFactSentence fact)  = iFact gf fact

-- A fact iS gf a proposition
iFact :: (PGF,Language) -> GFact -> Prop
iFact gf (GAttributeFact attr obj val)      = iObject gf obj $ \x -> ((iValue gf val) $ iAttribute gf attr x)
iFact gf (GKindFact obj kind)               = iObject gf obj $ iKind gf kind
iFact gf (GPropertyFact obj prop)           = iObject gf obj $ iProperty gf prop
iFact gf (GMaxObjectAttributeFact obj attr) = ForAll $ \x -> iObject gf obj $ \o -> Rel ((iAttribute gf attr) x) LtEq ((iAttribute gf attr) o) -- ∀x [a(x) <= a(o)]
iFact gf (GMinObjectAttributeFact obj attr) = ForAll $ \x -> iObject gf obj $ \o -> Rel ((iAttribute gf attr) x) GtEq ((iAttribute gf attr) o) -- ∀x [a(x) >= a(o)]
iFact gf (GMaxObjectKindAttributeFact obj kind attr) = ForAll $ \x -> Impl ((iKind gf kind) x) (iObject gf obj $ \o -> Conj (Rel ((iAttribute gf attr) x) LtEq ((iAttribute gf attr) o)) ((iKind gf kind) o))
iFact gf (GMinObjectKindAttributeFact obj kind attr) = ForAll $ \x -> Impl ((iKind gf kind) x) (iObject gf obj $ \o -> Conj (Rel ((iAttribute gf attr) x) GtEq ((iAttribute gf attr) o)) ((iKind gf kind) o))

-- Object corresponds to NP in the grammar
iObject :: (PGF,Language) -> GObject -> ((Ind -> Prop) -> Prop)
iObject gf (GConjObject list) = case map (iObject gf) (iListObject list) of -- interpret all objects in the liS gft
                               l -> \f -> case sequence l f of              -- apply f to all object
                                 (x:xs) -> foldr Conj x xs                  -- conjoin them
  where
    iListObject :: GListObject -> [GObject]
    iListObject (GListObject list) = list
iObject gf (GNameObject name)    = iNameNP gf name
iObject gf (GPronounObject name) = iNameNP gf name

iValue :: (PGF,Language) -> GValue -> (Value -> Prop)
iValue gf (GNameValue name)            = \v -> Rel v Eq (Val $ iNamePN gf name)
iValue gf (GNumericKindValue num kind) = \v -> Conj ((iNumeric num) v) ((iKind gf kind) (iNumericInd num))
iValue _  (GNumericValue num)          = iNumeric num

-- Get the individual of the value
iValueGetInd :: (PGF,Language) -> GValue -> Ind
iValueGetInd gf (GNameValue name)        = iNamePN gf name
iValueGetInd _ (GNumericKindValue num _) = iNumericInd num
iValueGetInd _ (GNumericValue num)       = iNumericInd num

-- Numerics are relations to values
iNumeric :: GNumeric -> (Value -> Prop)
iNumeric n = case n of
    GOverNumeric  num -> (\v -> Rel v Gt   $ iNumeric' num)
    GUnderNumeric num -> (\v -> Rel v Lt   $ iNumeric' num)
    GAboutNumeric num -> (\v -> Rel v Aprx $ iNumeric' num)
    num               -> (\v -> Rel v Eq   $ iNumeric' num)
  where
    iNumeric' :: GNumeric -> Value
    iNumeric' (GIntNumeric         int) = Val $ show $ iInt int
    iNumeric' (GIntMillionNumeric  int) = Val $ show $ 10^6 * iInt int
    iNumeric' (GIntBillionNumeric  int) = Val $ show $ 10^9 * iInt int
    iNumeric' _                         = error "Multiple comparators (over,under,about) is combined in the numeric!"

-- Numerics are also individuals
iNumericInd :: GNumeric -> Ind
iNumericInd (GIntNumeric         int) = show $ iInt int
iNumericInd (GIntMillionNumeric  int) = show $ 10^6  * iInt int
iNumericInd (GIntBillionNumeric  int) = show $ 10^9  * iInt int
iNumericInd (GOverNumeric        num) = iNumericInd num
iNumericInd (GUnderNumeric       num) = iNumericInd num
iNumericInd (GAboutNumeric       num) = iNumericInd num

-- Attribute corresponds to CN in the grammar, but it is treated as a function
iAttribute :: (PGF,Language) -> GAttribute -> (Ind -> Value)
iAttribute gf (LexAttribute s) = \x -> Func1P (lin gf $ LexAttribute $ cleanLexical s) x

-- Kind corresponds to CN in the grammar
iKind :: (PGF,Language) -> GKind -> (Ind -> Prop)
iKind gf (GPropertyKind prop kind) = \x -> Conj ((iProperty gf prop) x) ((iKind gf kind) x)
iKind gf (LexKind s)               = \x -> Pred1P (lin gf $ LexKind $ cleanLexical s) x

-- Property corresponds to AP in the grammar
iProperty :: (PGF,Language) -> GProperty -> (Ind -> Prop)
iProperty gf (GDemonymProperty cdname) = iCDNameCN gf cdname

-- Name corresponds to NP in the grammar
iNameNP :: (PGF,Language) -> GName -> ((Ind -> Prop) -> Prop)
iNameNP gf (GMkName  name)         = iCNameNP  gf name
iNameNP gf (GMkContinentName name) = iCDNameNP gf name

-- Name can also be treated as PN
iNamePN :: (PGF,Language) -> GName -> Ind
iNamePN gf (GMkName  name)         = iCNamePN  gf name
iNamePN gf (GMkContinentName name) = iCDNamePN gf name

-- Common names, treated as NP
iCNameNP :: (PGF,Language) -> GCName -> ((Ind -> Prop) -> Prop)
iCNameNP gf s = \f -> f $ iCNamePN gf s

-- Continent name/demonym, treated as NP
iCDNameNP :: (PGF,Language) -> GCDName -> ((Ind -> Prop) -> Prop)
iCDNameNP gf s = \f -> f $ iCDNamePN gf s

-- Continent name/demonym, treated as CN
iCDNameCN :: (PGF,Language) -> GCDName -> (Ind -> Prop)
iCDNameCN gf (LexCDName s) = \x -> Pred1P (lin gf $ LexCDName $ cleanLexical s) x

-- Common names, treated as PN
iCNamePN :: (PGF,Language) -> GCName -> Ind
iCNamePN gf (LexCName s) = lin gf $ LexCName $ cleanLexical s

-- Continent name/demonym, treated as PN
-- Ugly solution but at least it works
iCDNamePN :: (PGF,Language) -> GCDName -> Ind
iCDNamePN gf (LexCDName s) = [if c=='_' then ' ' else c | c <- (take ((length s) - (length "_CDName")) s)]

iInt :: GInt -> Integer
iInt (GInt int) = toInteger int

-----------------------------------------------------------
-- Just a little shorthand
lin :: Gf a => (PGF,Language) -> a -> String
lin (pgf,lang) g = case linearize pgf lang $ gf g of
  "" -> unknown
  s  -> s

-- Not really sure why this cleaning is needed, might be some problem with linearizing in PGF
cleanLexical :: String -> String
cleanLexical s | head s == '\'' && last s == '\'' = cleanLexical $ init $ tail s
               | otherwise                        = unpack $ replace (pack "\\'") (pack "'") $ pack s
