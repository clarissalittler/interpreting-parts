module Interp where

import AST
import Control.Monad.State

type Heap = [ Val ]

type Env a = [(String,a)]

type Addr = Int
          
data Val = VObj [(String, Addr)]
         | VClosure 
           Env Addr -- lexically scoped environment
           [Var]
           [Stmt] -- body of function
         | VNum Float
         | VString String
         | VBool Bool
         | VUndefined
         | VNan
         | VInf

data InterpState = IS { varenv :: Env Addr,
                        heap :: Heap}

type Interp = StateT InterpState IO

lookupVar :: Var -> Interp Val
lookupVar v = undefined

truthy :: Val -> Bool
truthy = undefined

funcall :: Val -> [Val] -> Interp Val
funcall = undefined

allocate :: Val -> Interp Addr
allocate = undefined

interpExp :: Exp -> Interp Val
interpExp ENan = return VNan
interpExp EInf = return EInf
interpExp (EBinOp e1 op e2) = do
  v1 <- interpExp e1
  v2 <- interpExp e2
  return $ (makeBinOp op) v1 v2 -- not right for And, actually. Might not be lazy enough
interpExp (EUnOp op e) = do
  v <- interpExp e
  return $ (makeUnOp op) v
interpExp (ETernIf e1 e2 e3) = do
  v1 <- interpExp e1
  if truthy v1 then interpExp e2 else interpExp e3
interpExp (EVar v) = lookupVar v
interpExp (ENewExp e es) = undefined
interpExp (EDelete eo acc) = undefined
interpExp (EAccess e a) = undefined

interpStmt :: Stmt -> Interp ()
interpStmt (SIf e trues ifelses) = undefined
interpStmt (SWhile e ss) = do
  v <- interpExp e
  if truthy v 
  then do 
    mapM_ interpStmt ss
    interpStmt (SWhile e ss)
  else return ()
interpStmt (SDo ss e) = do
  mapM_ interpStmt ss
  v <- interpExp e
  if truthy v
  then do
    interpStmt (SDo ss e)
  else return ()
interpStmt (SFor minit mtest minc ss) = undefined
interpStmt (SForIn v e ss) = undefined
interpStmt (STry tries n catches) = undefined
