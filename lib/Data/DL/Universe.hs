-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.

module Data.DL.Universe
  ( Universe (..),
    makeUniverse,
    getFacts,
  )
where

import Data.DL.Parser
import Data.List.Extra

-- | A universe (very roughly analagous to a "universe of discourse" in most
-- logical settings) is a set of facts and rules that continuously evolve as
-- inferences are made. Whereas a Document is a product of parsing, a Universe
-- is a product of deduction.
data Universe = Universe [GroundFact] [Rule] [BoundVar]
  deriving (Eq, Show)

instance Semigroup Universe where
  (<>) (Universe lf lr lv) (Universe rf rr rv) =
    Universe (lf ++ rf) (lr ++ rr) (lv ++ rv)

instance Monoid Universe where
  mempty = Universe [] [] []

makeUniverse :: Document -> Universe
makeUniverse (Document clauses) = mconcatMap toUniverse clauses
  where
    toUniverse (Simple fact@(Claim _ subject)) = Universe [fact] [] [subject]
    toUniverse (Rule rule) = Universe [] [rule] []

getFacts (Universe facts _ _) = facts
