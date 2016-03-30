module Interp where

import AST
import Control.Monad.State
import Control.Monad.Reader

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

type Interp = StateT InterpState (ReaderT (Bool,Bool) IO)

data Returnable a = RUndefined | RReturned a | RThrown a

lookupVar :: Var -> Interp Val
lookupVar v = do
  (venv,hp) <- get
  case lookup v venv of
    Nothing -> error "unknown variable"
    Just i -> hp !! i -- TODO: gracefully handle failure, or even better ensure that it won't happen

class Truthy a where -- what elements of a type count as "truthy"
    truthy :: a -> Bool

instance Truthy Bool where
    truthy = id

instance Truthy Int where
    truthy = (/=0)

instance Truthy String where
    truthy = (/= "")

instance Truthy Val where
    truthy VInf = True
    truthy VNan = False
    truthy VUndefined = False
    truthy (VBool b) = truthy b
    truthy (VNum f) = truthy f
    truthy (VString s) = truthy s
    truthy (VClosure _) = True
    truthy (VObj _ _ _) = True

{- This class might be overkill, but there ya go -}

funcall :: Val -> [Val] -> Interp Val
funcall = undefined

allocate :: Val -> Interp Addr
allocate = undefined

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

interpBlock :: [Stmt] -> Interp Val
interpBlock [] = return RUndefined
interpBlock (s :: ss) = do
  v <- interpStmt s
  case v of
    RUndefined -> interpBlock fun thr ss
    _ -> return v

returnable :: Interp Bool
returnable = fst `fmap` (lift ask)

throwable :: Interp Bool
throwable = snd `fmap` (list ask)

exitEarly :: Returnable Val -> Interp (Returnable Val) -> Interp (Returnable Val)
exitEarly rv m = case rv of
                   RUndefined -> m
                   _ -> return rv

-- the first boolean argument is whether the statement is in a "returnable" context
-- the second boolean argument is whether the statement is in a "throwable" context
interpStmt :: Stmt -> Interp (Returnable Val)
interpStmt (SReturn e) = do
  r <- returnable
  if r 
  then 
      case e of
        Nothing -> return $ RReturned VUndefined
        Just e' -> return $ RReturned `fmap` interpExp e'
  else error "Returning from a non-function context"
interpStmt (SIf e trues ifelses) = do
       v <- interpExp e
       if truthy v
       then interpBlock trues
       else elseInterp ifelses
    where elseInterp [] = RUndefined
          elseInterp (Just e, b) :: elses = do
                                     v <- interpExp e
                                     if truthy v
                                     then interpBlock b
                                     else elseIntepr elses
          elseInterp ((Nothing, b) :: _) = interpBlock b
interpStmt (SWhile e ss) = do
  v <- interpExp e
  if truthy v 
  then do 
    rv <- interpBlock ss
    exitEarly rv $ interpStmt (SWhile e ss)
  else return RUndefined
interpStmt (SDo ss e) = do
  r <- interpBlock ss
  exitEarly r $ do 
    v <- interpExp e
    if truthy v
    then do
      interpStmt (SDo ss e)
    else return RUndefined
interpStmt (SFor minit mtest minc ss) = undefined
interpStmt (SForIn v e ss) = undefined
interpStmt (STry tries n catches) = undefined
