-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.

module Data.DL.Evaluation.Util (bind, GroundAntecedent) where

import Data.DL.Evaluation
import Data.DL.Parser

type GroundAntecedent = Sentence BoundVar

-- | Run the given fact through the given substitutions.
bind :: Fact -> [(FreeVar, BoundVar)] -> Either EvalError GroundAntecedent
bind (Fact (Claim pred (Free subject))) subs = case lookup subject subs of
  Nothing -> Left $ SubstitutionFailure subject
  Just bound -> return $ Fact $ Claim pred bound
bind (Fact (Claim pred (Bound subject))) subs = return $ Fact $ Claim pred subject
-- \^ TODO: I don't know if this should fail.
bind (Conjunct lhs rhs) subs = do
  lhs' <- bind lhs subs
  rhs' <- bind rhs subs
  return $ Conjunct lhs' rhs'
