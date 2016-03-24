module AST where

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Text.PrettyPrint.HughesPJ


data Accessor = Dot String | Bracket Exp

type Var = String
type Name = String

data BinOp = Mult
           | Div
           | Mod
           | Add 
           | Sub
           | GEq
           | LEq
           | Gt
           | Lt
           | Eq
           | NEq
           | Or
           | And

data UnOp = Typeof
          | ToNum
          | Neg
          | Not

data STyp = Double | Single

type RegExp = String -- PLACEHOLDER

data Lit = LString STyp String
         | LNum Float
         | LObj [(Name,Exp)]
         | LBool Bool
         | LArray [Exp]
         | LRegExp RegExp

data Exp = EBinOP Exp BinOp Exp
         | EUnOp UnOp Exp
         | ETernIf Exp Exp Exp
         | ENewExp Exp [Exp]
         | EDelete Exp Accessor
         | EFunCall Exp [Exp]
         | EVar Var
         | ENan
         | EInf
         | EUndefined
         | EAccess Exp Accessor

data Stmt = SReturn (Maybe Exp)
          | SBreak (Maybe Var)
          | SThrow Exp
          | SFor (Maybe Exp) (Maybe Exp) (Maybe Exp) [Stmt]
          | SForIn Var Exp [Stmt]
          | SVar Name (Maybe Exp)
          | SIf Exp [Stmt] [(Maybe Exp,[Stmt])]
          | SWhile Exp [Stmt]
          | SDo [Stmt] Exp
          | STry [Stmt] Name [Stmt]

