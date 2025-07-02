-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.
--
module Data.DL.Parser
  ( Antecedent (..),
    Clause (..),
    Document (..),
    Fact (..),
    runDocumentParser,
  )
where

import Control.Monad
import Text.Parsec
import Text.Parsec.Text

newtype Document = Document [Clause]
  deriving (Show)

type Subject = String

type Predicate = String

data Clause = Simple Fact | Rule Fact Antecedent
  deriving (Show)

newtype Antecedent = Antecedent Fact
  deriving (Show)

data Fact = Fact Subject Predicate

instance Show Fact where
  show (Fact subject predicate) = "Fact: " <> subject <> " is-a " <> predicate

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
  Rule fact . Antecedent <$> factParser <* char '.'

factParser :: Parser Fact
factParser = do
  predicate <- many1 letter
  _ <- char '('
  subject <- many1 letter
  _ <- char ')'
  return $ Fact subject predicate

arrowParser :: Parser ()
arrowParser = void $ string ":-"

runDocumentParser :: FilePath -> IO (Either ParseError Document)
runDocumentParser = parseFromFile documentParser
