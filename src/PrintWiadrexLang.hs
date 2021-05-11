{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintWiadrexLang.
--   Generated by the BNF converter.

module PrintWiadrexLang where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, dropWhile, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Syntax.AbsWiadrexLang as AbsWiadrexLang

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i = \case
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    [";"]        -> showChar ';'
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i     = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt     _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsWiadrexLang.Ident where
  prt _ (AbsWiadrexLang.Ident i) = doc $ showString i

instance Print (AbsWiadrexLang.Program' a) where
  prt i = \case
    AbsWiadrexLang.PProgram _ inits -> prPrec i 0 (concatD [prt 0 inits])

instance Print [AbsWiadrexLang.Init' a] where
  prt = prtList

instance Print (AbsWiadrexLang.Arg' a) where
  prt i = \case
    AbsWiadrexLang.PArg _ id_ type_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString ":"), prt 0 type_])
    AbsWiadrexLang.PArgVar _ id_ type_ -> prPrec i 0 (concatD [doc (showString "var"), prt 0 id_, doc (showString ":"), prt 0 type_])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsWiadrexLang.Arg' a] where
  prt = prtList

instance Print (AbsWiadrexLang.Init' a) where
  prt i = \case
    AbsWiadrexLang.IFnDef _ id_ args type_ block -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), doc (showString ":"), prt 0 type_, prt 0 block])
    AbsWiadrexLang.IInit _ id_ type_ expr -> prPrec i 0 (concatD [doc (showString "var"), prt 0 id_, doc (showString ":"), prt 0 type_, doc (showString "="), prt 0 expr, doc (showString ";")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsWiadrexLang.Block' a) where
  prt i = \case
    AbsWiadrexLang.SBlock _ stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print (AbsWiadrexLang.Stmt' a) where
  prt i = \case
    AbsWiadrexLang.SEmpty _ -> prPrec i 0 (concatD [doc (showString ";")])
    AbsWiadrexLang.SBStmt _ block -> prPrec i 0 (concatD [prt 0 block])
    AbsWiadrexLang.SInit _ init -> prPrec i 0 (concatD [prt 0 init])
    AbsWiadrexLang.SAss _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsWiadrexLang.SIncr _ id_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString "++"), doc (showString ";")])
    AbsWiadrexLang.SDecr _ id_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString "--"), doc (showString ";")])
    AbsWiadrexLang.SRet _ expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    AbsWiadrexLang.SRetVoid _ -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    AbsWiadrexLang.SCond _ expr block -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsWiadrexLang.SCondElse _ expr block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block1, doc (showString "else"), prt 0 block2])
    AbsWiadrexLang.SWhile _ expr block -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsWiadrexLang.SExp _ expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsWiadrexLang.Stmt' a] where
  prt = prtList

instance Print (AbsWiadrexLang.Type' a) where
  prt i = \case
    AbsWiadrexLang.TInt _ -> prPrec i 0 (concatD [doc (showString "Int")])
    AbsWiadrexLang.TString _ -> prPrec i 0 (concatD [doc (showString "String")])
    AbsWiadrexLang.TBool _ -> prPrec i 0 (concatD [doc (showString "Bool")])
    AbsWiadrexLang.TVoid _ -> prPrec i 0 (concatD [doc (showString "Void")])
    AbsWiadrexLang.TFun _ types type_ -> prPrec i 0 (concatD [doc (showString "("), prt 0 types, doc (showString ")"), doc (showString "->"), prt 0 type_])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsWiadrexLang.Type' a] where
  prt = prtList

instance Print (AbsWiadrexLang.Expr' a) where
  prt i = \case
    AbsWiadrexLang.EVar _ id_ -> prPrec i 6 (concatD [prt 0 id_])
    AbsWiadrexLang.ELitInt _ n -> prPrec i 6 (concatD [prt 0 n])
    AbsWiadrexLang.ELitTrue _ -> prPrec i 6 (concatD [doc (showString "True")])
    AbsWiadrexLang.ELitFalse _ -> prPrec i 6 (concatD [doc (showString "False")])
    AbsWiadrexLang.EApp _ id_ exprs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])
    AbsWiadrexLang.EString _ str -> prPrec i 6 (concatD [prt 0 str])
    AbsWiadrexLang.ENeg _ expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsWiadrexLang.ENot _ expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsWiadrexLang.EMul _ expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsWiadrexLang.EAdd _ expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsWiadrexLang.ERel _ expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    AbsWiadrexLang.EAnd _ expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsWiadrexLang.EOr _ expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])
    AbsWiadrexLang.ELambda _ args type_ block -> prPrec i 0 (concatD [doc (showString "lambda"), doc (showString "("), prt 0 args, doc (showString ")"), doc (showString ":"), prt 0 type_, doc (showString "=>"), prt 0 block])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsWiadrexLang.Expr' a] where
  prt = prtList

instance Print (AbsWiadrexLang.AddOp' a) where
  prt i = \case
    AbsWiadrexLang.OPlus _ -> prPrec i 0 (concatD [doc (showString "+")])
    AbsWiadrexLang.OMinus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (AbsWiadrexLang.MulOp' a) where
  prt i = \case
    AbsWiadrexLang.OTimes _ -> prPrec i 0 (concatD [doc (showString "*")])
    AbsWiadrexLang.ODiv _ -> prPrec i 0 (concatD [doc (showString "/")])
    AbsWiadrexLang.OMod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (AbsWiadrexLang.RelOp' a) where
  prt i = \case
    AbsWiadrexLang.OLth _ -> prPrec i 0 (concatD [doc (showString "<")])
    AbsWiadrexLang.OLE _ -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsWiadrexLang.OGth _ -> prPrec i 0 (concatD [doc (showString ">")])
    AbsWiadrexLang.OGE _ -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsWiadrexLang.OEq _ -> prPrec i 0 (concatD [doc (showString "==")])
    AbsWiadrexLang.ONe _ -> prPrec i 0 (concatD [doc (showString "!=")])

