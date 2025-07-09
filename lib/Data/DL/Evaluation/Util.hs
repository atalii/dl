-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.

module Data.DL.Evaluation.Util (bind, GroundAntecedent) where

import Data.DL.Evaluation
import Data.DL.Parser

type GroundAntecedent = Sentence BoundVar

bind :: Fact -> [(FreeVar, BoundVar)] -> Either EvalError GroundAntecedent
bind f s = reduce $ runSubstitutions f s

-- | Reduce to ground, failing on any free variables.
reduce :: Fact -> Either EvalError GroundAntecedent
reduce (Fact (Claim pred (Free subject))) = Left $ SubstitutionFailure subject
reduce (Fact (Claim pred (Bound subject))) = Right $ Fact $ Claim pred subject
reduce (Conjunct lhs rhs) = do
  lhs' <- reduce lhs
  rhs' <- reduce rhs
  return $ Conjunct lhs' rhs'

-- | Run the given fact through the given substitutions.
runSubstitutions :: Fact -> [(FreeVar, BoundVar)] -> Fact
runSubstitutions (Fact (Claim pred (Free subject))) subs = case lookup subject subs of
  Nothing -> Fact $ Claim pred (Free subject)
  Just bound -> Fact $ Claim pred (Bound bound)
runSubstitutions (Fact (Claim pred (Bound subject))) subs = Fact $ Claim pred $ Bound subject
-- \^ TODO: I don't know if this should fail.
runSubstitutions (Conjunct lhs rhs) subs = Conjunct (runSubstitutions lhs subs) (runSubstitutions rhs subs)
