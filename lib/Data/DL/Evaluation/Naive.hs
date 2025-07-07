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

type GroundAntecedent = Sentence BoundVar

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
    apply rule = concatMap decomposeFacts . mapMaybe affirm <$> bindVarsIn rule

    bindVarsIn :: Rule -> Either (PosTagged EvalError) [GroundRule]
    bindVarsIn rule = catMaybes <$> mapM (substitute rule) vars

    substitute :: Rule -> BoundVar -> Either (PosTagged EvalError) (Maybe GroundRule)
    substitute
      ( Implication
          (Fact (Claim aPred (Free aSub)))
          (Claim cPred (Free cSub))
        )
      targetBound
        | cSub == aSub =
            return $
              Just $
                GroundRule
                  (Fact $ Claim aPred targetBound)
                  (Claim cPred targetBound)
    substitute
      (Implication (Fact _) (Claim _ (Free var@(Tag tag _))))
      _ = Left $ Tag tag $ SubstitutionFailure var
    substitute
      (Implication (Conjunct lhs rhs) consequent@(Claim cPred _))
      var = do
        lhs' <- substitute (Implication lhs consequent) var
        rhs' <- substitute (Implication rhs consequent) var
        return $ case (lhs', rhs') of
          (Just (GroundRule lhs'' _), Just (GroundRule rhs'' _)) ->
            Just $ GroundRule (Conjunct lhs'' rhs'') (Claim cPred var)
          _ -> Nothing
    substitute _ _ = return Nothing

    affirm (GroundRule (Fact antecedent) consequent) =
      if antecedent `elem` facts then Just consequent else Nothing
    affirm (GroundRule (Conjunct lhs rhs) consequent) =
      affirm (GroundRule lhs consequent) <* affirm (GroundRule rhs consequent)

    decomposeFacts :: GroundConsequent -> [GroundFact]
    decomposeFacts claim = [claim]
