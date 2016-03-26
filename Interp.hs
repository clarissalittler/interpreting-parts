module Interp where

import AST
import Control.Monad.State

type Heap = [ Val ]

type Env a = [(String,a)]

type Addr = Int
          
data Val = VObj Addr -- self address 
          [(String, Addr)] -- fields 
          Addr -- prototype, since everyone has one
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

data InterpState = IS { varenv :: Env Addr, -- variables -> heap objects
                        heap :: Heap, -- just a list of values, addresses are indexes
                        thisCtxt :: [Addr]} -- context for using this, addresses of objects

type Interp = StateT InterpState IO

lookupVar :: Var -> Interp Val
lookupVar v = do
  (venv,hp) <- get
  case lookup v venv of
    Nothing -> error "unknown variable"
    Just i -> hp !! i -- TODO: gracefully handle failure, or even better ensure that it won't happen

truthy :: Val -> Bool
truthy VInf = True
truthy VNan = False
truthy VUndefined = False
truthy (VBool b) = b
truthy (VNum f) = f /= 0
truthy (VString s) = s /= ""
truthy (VClosure _ _ _) = True
truthy (VObj _ _ _) = True

funcall :: Val -> [Val] -> Interp Val
funcall = undefined

allocate :: Val -> Interp Addr
allocate = undefined

interpBlock :: Block -> Interp ()
interpBlock = undefined

popThis :: Interp Val
popThis = do
  (venv,hp,(t : thss)) <- get
  put (venv,hp,thss)
  return $ hp !! t

putThis :: Addr -> Interp ()
putThis a = modify (\(v,h,ts) -> (v,h,a : ts))

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
interpExp (EAccess e a) = undefined -- remember that this needs to access the prototype tree
interpExp EThis = popThis

interpStmt :: Stmt -> Interp ()
interpStmt (SReturn 
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
