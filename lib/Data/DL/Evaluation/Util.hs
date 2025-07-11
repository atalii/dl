-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.
{-# LANGUAGE LambdaCase #-}

module Data.DL.Evaluation.Util
  ( bindRule,
    gatherFrees,
    GroundAntecedent,
    GroundConsequent,
    GroundRule (..),
  )
where

import Data.DL.Evaluation
import Data.DL.Parser
import Data.List
import Data.Maybe

type GroundAntecedent = Sentence BoundVar

type GroundConsequent = GroundFact

data GroundRule = GroundRule GroundAntecedent GroundConsequent

gatherFrees :: Rule -> [FreeVar]
gatherFrees (Implication a c) = nub $ gatherFrees' a ++ gatherFrees' (Fact c)
  where
    gatherFrees' :: Fact -> [FreeVar]
    gatherFrees' (Conjunct lhs rhs) = nub $ gatherFrees' lhs ++ gatherFrees' rhs
    gatherFrees' (Fact (Claim _ vars)) =
      mapMaybe
        ( \case
            Free v -> Just v
            _ -> Nothing
        )
        vars

bindRule :: Rule -> [(FreeVar, BoundVar)] -> Either EvalError GroundRule
bindRule (Implication a c) subs = do
  a' <- bind a subs
  c' <- bind (Fact c) subs
  return $
    GroundRule
      a'
      ( case c' of
          (Fact claim) -> claim
          _ -> error "bind is broken"
      )

bind :: Fact -> [(FreeVar, BoundVar)] -> Either EvalError GroundAntecedent
bind f s = reduce $ runSubstitutions s f

-- | Reduce to ground, failing on any free variables.
reduce :: Fact -> Either EvalError GroundAntecedent
reduce = traverse $ \case
  (Free v) -> Left $ SubstitutionFailure v
  (Bound v) -> Right v

-- | Run the given fact through the given substitutions.
runSubstitutions :: [(FreeVar, BoundVar)] -> Fact -> Fact
runSubstitutions subs = fmap substituteVar
  where
    substituteVar (Bound var) = Bound var
    substituteVar (Free var) = case lookup var subs of
      Just b -> Bound b
      Nothing -> Free var
