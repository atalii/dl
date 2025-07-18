-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.

module Data.DL.Evaluation (EvalError (..)) where

import Data.DL.Parser

newtype EvalError = SubstitutionFailure FreeVar

instance Show EvalError where
  show (SubstitutionFailure (Tag _ name)) =
    "no valid substitutions for unbound variable: " <> show name <> "."
