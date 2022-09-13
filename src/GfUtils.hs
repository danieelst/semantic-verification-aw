module GfUtils where

import Logic(unknown)
import PGF

-- Replace meta elements with the unknown individual
replaceMetaElements :: Tree -> Tree
replaceMetaElements t = caseOfMeta $ caseOfAbs $ caseOfApp t
  where
    caseOfApp :: Expr -> Expr
    caseOfApp e = case unApp e of
      Just (cid,exprs) -> mkApp cid $ map replaceMetaElements exprs
      Nothing          -> e
    caseOfAbs :: Expr -> Expr
    caseOfAbs e = case unAbs e of
      Just (bindt,cid,expr) -> mkAbs bindt cid $ replaceMetaElements expr
      Nothing               -> e
    caseOfMeta :: Expr -> Expr
    caseOfMeta e = case unMeta e of
      Just _  -> mkApp (mkCId unknown) []
      Nothing -> e
