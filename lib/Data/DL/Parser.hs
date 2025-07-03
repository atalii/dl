-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

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

data Variable = Bound SourcePos BoundVar | Free SourcePos FreeVar
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
  subject <- variableParser
  _ <- char ')'
  return $ Fact subject predicate

arrowParser :: Parser ()
arrowParser = void $ string ":-" <|> string "<-"

variableParser :: Parser Variable
variableParser = getParserState >>= var . statePos
  where
    var pos = Free pos <$> free <|> Bound pos <$> bound
    free = liftA2 (:) upper $ many letter
    bound = liftA2 (:) lower $ many letter

runDocumentParser :: FilePath -> IO (Either ParseError Document)
runDocumentParser = parseFromFile documentParser
