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
    Predicate (..),
    BoundVar (..),
    FreeVar (..),
    PosTagged (..),
    runDocumentParser,
  )
where

import Control.Monad
import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.Text

data PosTagged a = Tag SourcePos a

newtype Document = Document [Clause]
  deriving (Show)

type BoundVar = PosTagged String

type FreeVar = PosTagged String

data Variable = Bound BoundVar | Free FreeVar
  deriving (Eq, Show)

data Predicate = Equality | Literal String
  deriving (Eq)

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
  show (Claim (Literal pred) v) = show pred ++ "(" ++ show v ++ ")."
  show (Claim Equality vs) = mconcat $ intersperse " = " $ map show vs

instance (Eq a) => Eq (PosTagged a) where
  (==) (Tag _ a) (Tag _ b) = a == b

instance (Show a) => Show (PosTagged a) where
  show (Tag pos a) = show pos <> ": " <> show a

documentParser :: Parser Document
documentParser = Document <$> many item
  where
    item = comment >> clauseParser <* spaces

-- | Accept comments surrounding a given parser.
possiblyCommented :: Parser a -> Parser a
possiblyCommented p = spaces >> comment >> p <* spaces <* comment <* spaces

comment :: Parser ()
comment = void $ many $ (void (try line) <|> try inline) >> spaces
  where
    line = string "//" >> many (satisfy (/= '\n'))
    inline =
      void $
        string "/*" >> manyTill (inline <|> void anyChar) (try (string "*/"))

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
claimParser = try literalParser <|> equalityParser
  where
    literalParser = do
      predicate <- many1 letter
      _ <- possiblyCommented $ char '('
      subjects <-
        sepBy1 (possiblyCommented variableParser) (possiblyCommented $ char ',')
      _ <- possiblyCommented $ char ')'
      return $ Claim (Literal predicate) subjects

    equalityParser = do
      lhs <- possiblyCommented variableParser
      _ <- possiblyCommented $ char '='
      rhs <- possiblyCommented variableParser
      return $ Claim Equality [lhs, rhs]

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

  subjects <-
    sepBy1 (possiblyCommented boundParser) (possiblyCommented $ char ',')

  _ <- char ')'
  _ <- char '.'
  return $ Claim (Literal predicate) $ map (Tag pos) subjects

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

boundParser = stringLiteral <|> liftA2 (:) lower (many letter)

stringLiteral :: Parser String
stringLiteral = between (char '"') (char '"') $ many charOrEsc
  where
    charOrEsc = try esc <|> satisfy (/= '"')
    esc = char '\\' >> anyChar

runDocumentParser :: FilePath -> IO (Either ParseError Document)
runDocumentParser = parseFromFile documentParser
