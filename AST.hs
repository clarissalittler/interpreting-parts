module AST where

data Accessor = Dot String | Bracket Exp

type Var = String

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

data Exp = EBinOP Exp BinOp Exp
         | EUnOp UnOp Exp
         | EStringLit String
         | EObjLit [(String,Exp)]
         | ENum Float
         | ETernIf Exp Exp Exp
         | ENewExp Exp [Exp]
         | EDelete Exp
         | EFunCall Exp [Exp]
         | EVar Var
         | EBool Bool
         | ENan
         | EInf
         | EUndefined

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
          

