module CountriesGrammar where

import PGF hiding (Tree)

----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

newtype GString = GString String deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double deriving Show

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GAttribute =
   LexAttribute String
  deriving Show

data GCDName =
   LexCDName String
  deriving Show

data GCName =
   LexCName String
  deriving Show

data GDoc =
   GAddSentenceDoc GDoc GSentence
 | GOneSentenceDoc GSentence
  deriving Show

data GFact =
   GAttributeFact GAttribute GObject GValue
 | GKindFact GObject GKind
 | GMaxObjectAttributeFact GObject GAttribute
 | GMaxObjectKindAttributeFact GObject GKind GAttribute
 | GMinObjectAttributeFact GObject GAttribute
 | GMinObjectKindAttributeFact GObject GKind GAttribute
 | GPropertyFact GObject GProperty
  deriving Show

data GKind =
   GPropertyKind GProperty GKind
 | LexKind String
  deriving Show

newtype GListObject = GListObject [GObject] deriving Show

data GName =
   GMkContinentName GCDName
 | GMkName GCName
  deriving Show

data GNumeric =
   GAboutNumeric GNumeric
 | GIntBillionNumeric GInt
 | GIntMillionNumeric GInt
 | GIntNumeric GInt
 | GOverNumeric GNumeric
 | GUnderNumeric GNumeric
  deriving Show

data GObject =
   GConjObject GListObject
 | GNameObject GName
 | GPronounObject GName
  deriving Show

data GProperty = GDemonymProperty GCDName
  deriving Show

data GSentence =
   GConjSentence GSentence GSentence
 | GFactSentence GFact
  deriving Show

data GValue =
   GNameValue GName
 | GNumericKindValue GNumeric GKind
 | GNumericValue GNumeric
  deriving Show


instance Gf GAttribute where
  gf (LexAttribute x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexAttribute (showCId i)
      _ -> error ("no Attribute " ++ show t)

instance Gf GCDName where
  gf (LexCDName x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexCDName (showCId i)
      _ -> error ("no CDName " ++ show t)

instance Gf GCName where
  gf (LexCName x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexCName (showCId i)
      _ -> error ("no CName " ++ show t)

instance Gf GDoc where
  gf (GAddSentenceDoc x1 x2) = mkApp (mkCId "AddSentenceDoc") [gf x1, gf x2]
  gf (GOneSentenceDoc x1) = mkApp (mkCId "OneSentenceDoc") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AddSentenceDoc" -> GAddSentenceDoc (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "OneSentenceDoc" -> GOneSentenceDoc (fg x1)


      _ -> error ("no Doc " ++ show t)

instance Gf GFact where
  gf (GAttributeFact x1 x2 x3) = mkApp (mkCId "AttributeFact") [gf x1, gf x2, gf x3]
  gf (GKindFact x1 x2) = mkApp (mkCId "KindFact") [gf x1, gf x2]
  gf (GMaxObjectAttributeFact x1 x2) = mkApp (mkCId "MaxObjectAttributeFact") [gf x1, gf x2]
  gf (GMaxObjectKindAttributeFact x1 x2 x3) = mkApp (mkCId "MaxObjectKindAttributeFact") [gf x1, gf x2, gf x3]
  gf (GMinObjectAttributeFact x1 x2) = mkApp (mkCId "MinObjectAttributeFact") [gf x1, gf x2]
  gf (GMinObjectKindAttributeFact x1 x2 x3) = mkApp (mkCId "MinObjectKindAttributeFact") [gf x1, gf x2, gf x3]
  gf (GPropertyFact x1 x2) = mkApp (mkCId "PropertyFact") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "AttributeFact" -> GAttributeFact (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "KindFact" -> GKindFact (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "MaxObjectAttributeFact" -> GMaxObjectAttributeFact (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "MaxObjectKindAttributeFact" -> GMaxObjectKindAttributeFact (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "MinObjectAttributeFact" -> GMinObjectAttributeFact (fg x1) (fg x2)
      Just (i,[x1,x2,x3]) | i == mkCId "MinObjectKindAttributeFact" -> GMinObjectKindAttributeFact (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "PropertyFact" -> GPropertyFact (fg x1) (fg x2)


      _ -> error ("no Fact " ++ show t)

instance Gf GKind where
  gf (GPropertyKind x1 x2) = mkApp (mkCId "PropertyKind") [gf x1, gf x2]
  gf (LexKind x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "PropertyKind" -> GPropertyKind (fg x1) (fg x2)

      Just (i,[]) -> LexKind (showCId i)
      _ -> error ("no Kind " ++ show t)

instance Gf GListObject where
  gf (GListObject [x1,x2]) = mkApp (mkCId "BaseObject") [gf x1, gf x2]
  gf (GListObject (x:xs)) = mkApp (mkCId "ConsObject") [gf x, gf (GListObject xs)]
  fg t =
    GListObject (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseObject" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsObject" -> fg x1 : fgs x2


      _ -> error ("no ListObject " ++ show t)

instance Gf GName where
  gf (GMkContinentName x1) = mkApp (mkCId "MkContinentName") [gf x1]
  gf (GMkName x1) = mkApp (mkCId "MkName") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "MkContinentName" -> GMkContinentName (fg x1)
      Just (i,[x1]) | i == mkCId "MkName" -> GMkName (fg x1)


      _ -> error ("no Name " ++ show t)

instance Gf GNumeric where
  gf (GAboutNumeric x1) = mkApp (mkCId "AboutNumeric") [gf x1]
  gf (GIntBillionNumeric x1) = mkApp (mkCId "IntBillionNumeric") [gf x1]
  gf (GIntMillionNumeric x1) = mkApp (mkCId "IntMillionNumeric") [gf x1]
  gf (GIntNumeric x1) = mkApp (mkCId "IntNumeric") [gf x1]
  gf (GOverNumeric x1) = mkApp (mkCId "OverNumeric") [gf x1]
  gf (GUnderNumeric x1) = mkApp (mkCId "UnderNumeric") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "AboutNumeric" -> GAboutNumeric (fg x1)
      Just (i,[x1]) | i == mkCId "IntBillionNumeric" -> GIntBillionNumeric (fg x1)
      Just (i,[x1]) | i == mkCId "IntMillionNumeric" -> GIntMillionNumeric (fg x1)
      Just (i,[x1]) | i == mkCId "IntNumeric" -> GIntNumeric (fg x1)
      Just (i,[x1]) | i == mkCId "OverNumeric" -> GOverNumeric (fg x1)
      Just (i,[x1]) | i == mkCId "UnderNumeric" -> GUnderNumeric (fg x1)


      _ -> error ("no Numeric " ++ show t)

instance Gf GObject where
  gf (GConjObject x1) = mkApp (mkCId "ConjObject") [gf x1]
  gf (GNameObject x1) = mkApp (mkCId "NameObject") [gf x1]
  gf (GPronounObject x1) = mkApp (mkCId "PronounObject") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "ConjObject" -> GConjObject (fg x1)
      Just (i,[x1]) | i == mkCId "NameObject" -> GNameObject (fg x1)
      Just (i,[x1]) | i == mkCId "PronounObject" -> GPronounObject (fg x1)


      _ -> error ("no Object " ++ show t)

instance Gf GProperty where
  gf (GDemonymProperty x1) = mkApp (mkCId "DemonymProperty") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "DemonymProperty" -> GDemonymProperty (fg x1)


      _ -> error ("no Property " ++ show t)

instance Gf GSentence where
  gf (GConjSentence x1 x2) = mkApp (mkCId "ConjSentence") [gf x1, gf x2]
  gf (GFactSentence x1) = mkApp (mkCId "FactSentence") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "ConjSentence" -> GConjSentence (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "FactSentence" -> GFactSentence (fg x1)


      _ -> error ("no Sentence " ++ show t)

instance Gf GValue where
  gf (GNameValue x1) = mkApp (mkCId "NameValue") [gf x1]
  gf (GNumericKindValue x1 x2) = mkApp (mkCId "NumericKindValue") [gf x1, gf x2]
  gf (GNumericValue x1) = mkApp (mkCId "NumericValue") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "NameValue" -> GNameValue (fg x1)
      Just (i,[x1,x2]) | i == mkCId "NumericKindValue" -> GNumericKindValue (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "NumericValue" -> GNumericValue (fg x1)


      _ -> error ("no Value " ++ show t)
