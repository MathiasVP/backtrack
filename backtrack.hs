{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Backtrack where

import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Trans
import qualified Data.List as List
import Control.Lens
import Language.Haskell.TH.Syntax
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Cont
import Control.Monad.State.Class
import Control.Lens.Getter
import Control.Lens.Tuple
import Control.Monad.Cont.Class
import Control.Lens.Setter
import GHC.Base
import GHC.Err

doWhileM :: Monad m => m Bool -> m a -> m a
doWhileM test body = run
  where run = do
          x <- body
          b <- test
          if b then return x else run

(#?) :: Q Exp -> Q Exp
(#?) q = do
  n <- newName "x"
  k <- newName "k"
  r <- newName "r"
  gets <- [| Control.Monad.State.Class.gets |]
  view <- [| Control.Lens.Getter.view |]
  _1 <- [| Control.Lens.Tuple._1 |]
  _2 <- [| Control.Lens.Tuple._2 |]
  modify <- [| Control.Monad.State.Class.modify |]
  set <- [| (Control.Lens.Setter..~) |]
  returnname <- [| return |]
  callccname <- [| Control.Monad.Cont.Class.callCC |]
  evalstatet <- [| Control.Monad.Trans.State.Lazy.evalStateT |]
  runcontt <- [| Control.Monad.Trans.Cont.runContT |]
  undef <- [| GHC.Err.undefined |]
  lift <- [| Control.Monad.Trans.lift |]
  q >>= \case
    CaseE exp ms ->
      let
        enrichMatch (c, Match p body decs) =
          case body of
            GuardedB [(guard, exp)] ->
              let guard' = case guard of
                    NormalG exp -> PatG [checkcount, NoBindS exp]
                    PatG stmts -> PatG (checkcount : stmts)
                  exp' = (DoE [setn c, NoBindS (AppE exp (AppE (VarE k) undef))])
              in Match p (GuardedB [(guard', exp')]) decs
            NormalB exp ->
              let exp' = (DoE [setn c, NoBindS (AppE exp (AppE (VarE k) undef))])
              in Match p (GuardedB [(PatG [checkcount], exp')]) decs
          where checkcount = NoBindS (InfixE (Just (VarE n))
                                                (VarE '(<))
                                               (Just (LitE (IntegerL c))))
        cas = CaseE exp (List.map enrichMatch (List.zip [1..] ms))
        getn = BindS (VarP n) (AppE gets (AppE view _1))
        setn c = NoBindS (AppE modify (InfixE (Just _1) set (Just (LitE (IntegerL c)))))
        setterminated =
          NoBindS (AppE modify
                    (InfixE (Just _2) set (Just (ConE 'True))))
        doreturn = NoBindS (AppE returnname (UnboundVarE r))
        callcc =
          AppE callccname
            (LamE [VarP k] (DoE [getn, BindS (VarP r) cas, setterminated, doreturn]))
      in
        return $
          AppE (AppE evalstatet
               (AppE (AppE runcontt
                       (AppE (AppE (VarE 'doWhileM)
                               (AppE gets
                                 (AppE view
                                   _2))) callcc))
                 returnname)) (TupE [LitE (IntegerL 0), ConE 'False])
