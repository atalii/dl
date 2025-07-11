-- This Source Code Form is subject to the terms of the Mozilla Public License,
-- v. 2.0. If a copy of the MPL was not distributed with this file, You can
-- obtain one at https://mozilla.org/MPL/2.0/.
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}

module Data.DL.Parser
  ( Antecedent,
    Consequent,
    Clause (..),
    Document (..),
    Fact,
    Claim (..),
    Rule (..),
    Variable (..),
    Sentence (..),
    GroundFact,
    Predicate,
    BoundVar (..),
    FreeVar (..),
    PosTagged (..),
    runDocumentParser,
  )
where

import Control.Monad
import Data.Char
import Text.Parsec
import Text.Parsec.Text

data PosTagged a = Tag SourcePos a

newtype Document = Document [Clause]
  deriving (Show)

type BoundVar = PosTagged String

type FreeVar = PosTagged String

data Variable = Bound BoundVar | Free FreeVar
  deriving (Eq, Show)

type Predicate = String

data Rule = Implication Antecedent Consequent
  deriving (Eq, Show)

data Clause = Simple GroundFact | Rule Rule
  deriving (Show)

type Consequent = Claim Variable

type Antecedent = Fact

data Sentence v = Fact (Claim v) | Conjunct (Sentence v) (Sentence v)
  deriving (Eq, Functor, Foldable)

data Claim v = Claim Predicate [v]
  deriving (Eq, Functor, Foldable)

type Fact = Sentence Variable

type GroundFact = Claim BoundVar

instance Traversable Sentence where
  traverse f (Fact claim) = Fact <$> traverse f claim
  traverse f (Conjunct lhs rhs) = (Conjunct <$> traverse f lhs) <*> traverse f rhs

instance Traversable Claim where
  traverse f (Claim pred v) = Claim pred <$> traverse f v

instance (Show v) => Show (Sentence v) where
  show (Fact c) = show c

instance (Show v) => Show (Claim v) where
  show (Claim pred v) = show pred ++ "(" ++ show v ++ ")."

instance (Eq a) => Eq (PosTagged a) where
  (==) (Tag _ a) (Tag _ b) = a == b

instance (Show a) => Show (PosTagged a) where
  show (Tag pos a) = show pos <> ": " <> show a

documentParser :: Parser Document
documentParser = fmap Document $ many (clauseParser <* spaces) <* spaces

clauseParser :: Parser Clause
clauseParser = try ruleParser <|> fmap Simple groundFactParser

ruleParser :: Parser Clause
ruleParser = do
  consequent <- claimParser
  spaces
  arrowParser
  spaces
  antecedent <- factParser True
  return $ Rule $ Implication antecedent consequent

claimParser :: Parser (Claim Variable)
claimParser = do
  predicate <- many1 letter
  _ <- char '('
  subjects <- sepBy1 variableParser (char ',' >> spaces)
  _ <- char ')'
  return $ Claim predicate subjects

factParser :: Bool -> Parser Fact
factParser terminal = do
  (Fact <$> try (claimParser <* conditionalPeriod)) <|> conjunctParser
  where
    conditionalPeriod = when terminal $ void (char '.')

conjunctParser :: Parser Fact
conjunctParser = do
  lhs <- factParser False
  spaces
  conjunctionSignParser
  spaces
  rhs <- factParser True
  return $ Conjunct lhs rhs

groundFactParser :: Parser GroundFact
groundFactParser = do
  predicate <- many1 letter
  _ <- char '('

  state <- getParserState
  let pos = statePos state

  subjects <- sepBy1 boundParser (char ',' >> spaces)
  _ <- char ')'
  _ <- char '.'
  return $ Claim predicate $ map (Tag pos) subjects

arrowParser :: Parser ()
arrowParser = void $ string ":-" <|> string "<-"

conjunctionSignParser :: Parser ()
conjunctionSignParser = void $ string "/\\" <|> string "∧" <|> string ","

variableParser :: Parser Variable
variableParser = getParserState >>= var . statePos
  where
    var pos =
      Free . Tag pos <$> freeParser
        <|> Bound . Tag pos <$> boundParser

freeParser = liftA2 (:) upper $ many letter

boundParser = liftA2 (:) lower $ many letter

runDocumentParser :: FilePath -> IO (Either ParseError Document)
runDocumentParser = parseFromFile documentParser
