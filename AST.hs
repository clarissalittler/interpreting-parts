module AST where

import Text.PrettyPrint.HughesPJ

data Accessor = Dot String | Bracket Exp | Prototype

type Var = String
type Name = String

ppBinOp :: BinOp -> Doc
ppBinOp Mult = text "*"
ppBinOp Div = text "/"
ppBinOp Mod = text "%"
ppBinOp Add = text "+"
ppBinOp Sub = text "-"
ppBinOp GEq = text ">="
ppBinOp LEq = text "<="
ppBinOp Gt = text ">"
ppBinOp Lt = text "<"
ppBinOp Eq = text "==="
ppBinOp NEq = text "!=="
ppBinOp Or = text "||"
ppBinOp And = text "&&"

instance Show BinOp where
    show = render . ppBinOp

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

ppUnOp :: UnOp -> Doc -> Doc
ppUnOp Typeof d = text "typeof" <+> d
ppUnOp ToNum d = text "+" <> d
ppUnOp Neg d = text "-" <> d
ppUnOp Not d = text "!" <> d
ppUnOp Inc d = text "++" <> d
ppUnOp Dec d = text "--" <> d
ppUnOp ThenInc d = d <> text "++"
ppUnOp ThenDec d = d <> text "--"

data UnOp = Typeof
          | ToNum
          | Neg
          | Not
          | Inc -- ++x
          | Dec -- --x
          | ThenInc -- x++
          | ThenDec -- x--

data STyp = Double | Single

type RegExp = String -- PLACEHOLDER
type Block = [Stmt]

commaSep = sep . punctuate comma

ppLit :: Lit -> Doc
ppLit (LString Double s) = doubleQuotes $ text s
ppLit (LString Single s) = quotes $ text s
ppLit (LBool True) = text "true"
ppLit (LBool False) = text "false"
ppLit (LArray es) = brackets $ commaSep $ map ppExp es
ppLit (LNum n) = float n

instance Show Lit where
    show = render . ppLit

data Lit = LString STyp String
         | LNum Float
         | LObj [(Name,Exp)]
         | LBool Bool
         | LArray [Exp]

ppExp :: Exp -> Doc
ppExp (EBinOp e1 op e2) = ppExp e1 <+> ppBinOp op <+> ppExp e2
ppExp (EUnOp u e) = ppUnOp u (ppExp e)
ppExp (ETernIf e1 e2 e3) = ppExp e1 <+> text "?" <+> ppExp e2 <+> text ":" <+> ppExp e3
ppExp (ENewExp f es) = text "new" <+> ppExp f <> parens (commaSep (map ppExp es))
ppExp (EDelete e a) = undefined
ppExp (EFunCall f es) = ppExp f <> parens (commaSep (map ppExp es))
ppExp (EVar v) = text v
ppExp ENan = text "NaN"
ppExp EInf = text "Infinity"
ppExp EUndefined = text "undefined"
ppExp (EAccess e a) = undefined
ppExp (EFunDecl v vs ss) = undefined
ppExp (EAssign v e) = text v <+> text "=" <+> ppExp e
ppExp (EAssignDec v e) = text v <+> text "-=" <+> ppExp e
ppExp (EAssignInc v e) = text v <+> text "+=" <+> ppExp e
ppExp EThis = text "this"

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
         | EFunDecl Var [Var] Block
         | EAssign Var Exp
         | EAssignDec Var Exp
         | EAssignInc Var Exp
         | EThis

data Stmt = SReturn (Maybe Exp)
          | SBreak (Maybe Var)
          | SThrow Exp
          | SFor (Maybe Exp) (Maybe Exp) (Maybe Exp) Block
          | SForIn Var Exp [Stmt]
          | SVar Name (Maybe Exp)
          | SIf Exp Block [(Maybe Exp, Block)]
          | SWhile Exp Block
          | SDo Block Exp
          | STry Block Name Block
          | SSwitch Exp [Clause] (Maybe Block)
          | SExp Exp

