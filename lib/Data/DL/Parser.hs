{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.
{- We use `head` for working with results from the parser, which are, by
 - the definition of the grammar, non-empty. -}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Data.DL.Parser
  ( Antecedent,
    Consequent,
    Clause (..),
    Document (..),
    Fact,
    Variable (..),
    Sentence (..),
    Predicate,
    BoundVar,
    FreeVar,
    runDocumentParser,
  )
where

import Control.Monad
import Data.Char
import Text.Parsec
import Text.Parsec.Text

newtype Document = Document [Clause]
  deriving (Show)

type BoundVar = String

type FreeVar = String

data Variable = Bound BoundVar | Free FreeVar
  deriving (Eq, Show)

type Predicate = String

data Clause = Simple Fact | Rule Consequent Antecedent
  deriving (Show)

type Consequent = Fact

type Antecedent = Fact

--  Store a predicate on a variable of some kind. Evaluation can discriminate
--  between sentences on bounded variables, sentences on free variables, or, as
--  the parser, work on 'facts,' i.e., sentences of any kind of variable.
data Sentence v where
  Fact :: (Eq v) => v -> Predicate -> Sentence v

deriving instance (Eq v) => Eq (Sentence v)

type Fact = Sentence Variable

instance (Show v) => Show (Sentence v) where
  show (Fact subject predicate) =
    "Fact: " <> show subject <> " is-a " <> predicate

documentParser :: Parser Document
documentParser = fmap Document $ many (clauseParser <* spaces) <* spaces

clauseParser :: Parser Clause
clauseParser = try ruleParser <|> fmap Simple (factParser <* char '.')

ruleParser :: Parser Clause
ruleParser = do
  fact <- factParser
  spaces
  arrowParser
  spaces
  Rule fact <$> factParser <* char '.'

factParser :: Parser Fact
factParser = do
  predicate <- many1 letter
  _ <- char '('
  subject <- many1 letter
  _ <- char ')'
  return $ Fact (makeVariable subject) predicate

arrowParser :: Parser ()
arrowParser = void $ string ":-" <|> string "<-"

runDocumentParser :: FilePath -> IO (Either ParseError Document)
runDocumentParser = parseFromFile documentParser

makeVariable :: String -> Variable
makeVariable s = if isUpper (head s) then Free s else Bound s
