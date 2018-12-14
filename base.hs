module SCbase where

import Control.Monad
import Data.List
import Data.Maybe

-- Language definition
type Name = String
type Def  = (Name, Expr)

data Prog = Prog Expr [Def]         -- Prog := E_0 where F is {f_1 = E_1  ... f_k = E_k}
            deriving (Show, Eq)
data Expr = Var Name                -- E := x 
          | Func Name               --    | f \in F
          | Cons Name [Expr]        --    | Cons E_1 ... E_n -- named constructor
          | App Expr Expr           --    | E_1 E_2 
          | Lmb Name Expr           --    | \x. E 
          | Let Name Expr Expr      --    | let x = E_1 in E_2 
          | Case Expr [(Pat, Expr)] --    | Case E_0 of p_1 => E_1
            deriving (Show, Eq)     --                | p_2 => E_2
                                    --                ...
                                    --                | p_n => E_n
data Pat  = Uncons Name [Name]      -- p := Cons x_1 ... x_n
            deriving (Show, Eq)

-- Supercompilation-related data types and functions
data State       = St Expr Transition        -- tree node with possible transitions
                   deriving (Show, Eq) 
data Transition  = Uncond State              -- unconditional transition
                 | Dep (Name, State) State   -- unconditional too but requires some dependency first
                 | Cond State [(Pat, State)] -- transition involving some pattern matching
                 | Leaf                      -- current state is final and its expression should be returned
                   deriving (Show, Eq) 
data ProgTree    = PT [Def] State            -- program tree with F and initial state

buildPT :: Prog -> ProgTree -- builds a program tree from given program
buildPT (Prog e0 fs) = PT fs $ buildPTInner e0 
    where buildPTInner :: Expr -> State 
          buildPTInner e = St e $ buildTransition e

          buildTransition :: Expr -> Transition
          buildTransition e 
                | (App e1 e2)   <- e,
                  (Lmb v e')    <- e1                 = Dep (v, buildPTInner e2) $ buildPTInner e'
                | (Let v e1 e2) <- e                  = Dep (v, buildPTInner e1) $ buildPTInner e2
                | (Func f)      <- e, 
                  (Just e')     <- lookup f fs        = Uncond $ buildPTInner e'
                | (App e1 e2)   <- e, 
                  Leaf          <- buildTransition e1 = fmapTransition (App e1) $ buildTransition e2
                | (App e1 e2)   <- e                  = fmapTransition (flip App e2) $ buildTransition e1
                | (Case e0 css) <- e                  = Cond (buildPTInner e0) $ map (fmap buildPTInner) css
                | otherwise                           = Leaf

          fmapState :: (Expr -> Expr) -> State -> State
          fmapState f (St e tr) = St (f e) $ fmapTransition f tr

          fmapTransition :: (Expr -> Expr) -> Transition -> Transition
          fmapTransition f (Uncond st)      = Uncond $ fmapState f st
          fmapTransition f (Dep d st)       = Dep d $ fmapState f st
          fmapTransition f (Cond st0 conds) = Cond st0 $ map (fmap $ fmapState f) conds
          fmapTransition _ Leaf             = Leaf

evalPT :: [Def] -> ProgTree -> Maybe Expr -- compute result using program tree and initial context
evalPT vs (PT fs st) = evalState vs st
    where evalState :: [Def] -> State -> Maybe Expr
          evalState vs (St e tr)
            | (Uncond st)       <- tr               = evalState vs st
            | (Dep (v, stv) st) <- tr, 
              (Just ev)         <- evalState vs stv = evalState ((v, ev):vs) st
            | (Cond stc css)    <- tr, 
              (Just ec)         <- evalState vs stc = evalCond vs ec css
            | Leaf              <- tr               = Just e
          
          evalCond :: [Def] -> Expr -> [(Pat, State)] -> Maybe Expr
          evalCond _ _ [] = Nothing
          evalCond vs e ((p, st):css) 
            | (Just vs') <- matchPat e p = evalState (vs' ++ vs) st
            | otherwise                  = evalCond vs e css

          matchPat :: Expr -> Pat -> Maybe [Def]
          matchPat (Cons ename es) (Uncons pname vs)
            | (length vs) == (length es), 
              ename == pname              = Just $ zip vs es
            | otherwise                   = Nothing
