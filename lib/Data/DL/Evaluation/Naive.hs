-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.

module Data.DL.Evaluation.Naive (naive) where

import Data.DL.Evaluation
import Data.DL.Parser
import Data.DL.Universe
import Data.DL.Util
import Data.Either
import Data.Either.Extra
import Data.List
import Data.Maybe

type GroundAntecedent = GroundFact

type GroundConsequent = GroundFact

data GroundRule = GroundRule GroundAntecedent GroundConsequent

-- | Run the naive evaluation strategy by repeatedly making as many inferences
-- as possible until we reach a fixed point.
naive :: Universe -> Either [PosTagged EvalError] [GroundFact]
naive u = getFacts <$> findFixedPoint u

findFixedPoint :: Universe -> Either [PosTagged EvalError] Universe
findFixedPoint u = do
  u' <- infer u
  if u == u' then return u else findFixedPoint u'

infer :: Universe -> Either [PosTagged EvalError] Universe
infer (Universe facts rules vars) =
  addFactsToUniverse <$> joinEithers (map (mapLeft singleton . apply) rules)
  where
    addFactsToUniverse newFacts = Universe (nub $ facts ++ newFacts) rules vars

    apply :: Rule -> Either (PosTagged EvalError) [GroundFact]
    apply rule = mapMaybe affirm <$> bindVarsIn rule

    bindVarsIn :: Rule -> Either (PosTagged EvalError) [GroundRule]
    bindVarsIn
      ( Implication
          (Fact (Free aSub) aPred)
          (Fact (Free cSub@(Tag errTag _)) cPred)
        ) =
        if cSub == aSub
          then
            Right $
              map
                (\bound -> GroundRule (Fact bound aPred) (Fact bound cPred))
                vars
          else Left $ Tag errTag $ SubstitutionFailure cSub
    bindVarsIn (Implication (Fact (Free aSub) _) _) = Right []

    affirm (GroundRule antecedent consequent) =
      if antecedent `elem` facts then Just consequent else Nothing
