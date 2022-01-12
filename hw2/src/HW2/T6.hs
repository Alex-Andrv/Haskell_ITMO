{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( -- * Datatypes
    ParseError (..),
    Parser (..),

    -- * functions
    pChar,
    pEof,
    parseError,
    parseExpr,
    runP,
  )
where

import Control.Applicative (Alternative (..), Applicative (..), many, optional, some)
import Control.Monad (MonadPlus, mfilter, msum, void)
import Data.Char (intToDigit, isUpper)
import Data.Ratio ((%))
import GHC.Natural (Natural)
import HW2.T1 (Annotated ((:#)), Except (Error, Success))
import HW2.T4 (Expr (Op, Val), Prim (Add, Div, Mul, Sub))
import HW2.T5 (ExceptState (ES, runES))

newtype ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP parser string = case runPairP parser (0, string) of
  Error e -> Error e
  Success (a :# _) -> Success a

runPairP :: Parser a -> (Natural, String) -> Except ParseError (Annotated (Natural, String) a)
runPairP (P except_state) = runES except_state

parseError :: Parser a
parseError = P $ ES \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  -- empty :: f 
  empty = parseError

  -- (<|>) :: f a -> f a -> f a
  (<|>) first second =
    P
      ( ES \pair -> case runPairP first pair of
          Error _ -> runPairP second pair
          Success a -> Success a
      )

pEof :: Parser ()
pEof =
  P
    ( ES \(pos, s) -> case s of
        [] -> Success (() :# (pos, ""))
        _ -> Error (ErrorAtPos pos)
    )

instance MonadPlus Parser

--    mzero :: m a
--    mzero = empty
--    mplus :: m a -> m a -> m a
--    mplus = (<|>)

-- | What happens when the string is empty?
-- Error (ErrorAtPos 0)
-- | How does the parser state change when a character is consumed?
-- (pos + 1, cs)

--- << primitives
pChar :: Parser Char
pChar = P $ ES \(pos, s) ->
  case s of
    [] -> Error (ErrorAtPos pos)
    (c : cs) -> Success (c :# (pos + 1, cs))

pSpecific :: (Char -> Bool) -> Parser Char
pSpecific is_specific = mfilter is_specific pChar

isChar :: Char -> (Char -> Bool)
isChar c = (c ==)

parserChar :: Char -> Parser Char
parserChar c = pSpecific (isChar c)

parserSpace :: Parser ()
parserSpace = void $ many $ parserChar ' '

parserCharSkipSpace :: Char -> Parser Char
parserCharSkipSpace c = do
  parserSpace
  pSpecific (isChar c)

parserDot :: Parser Char
parserDot = parserChar '.'

parserOpenParenthesis :: Parser Char
parserOpenParenthesis = parserCharSkipSpace '('

parserCloseParenthesis :: Parser Char
parserCloseParenthesis = parserCharSkipSpace ')'

parserOptional :: Parser a -> a -> Parser a
parserOptional pa v = pa <|> pure v

type CreateExpr = Expr -> Expr -> Expr

type CreatePrim = Expr -> Expr -> Prim Expr

parseBinOp :: Char -> CreatePrim -> Parser CreateExpr
parseBinOp char createExpr = do
  void $ parserCharSkipSpace char
  pure (\f -> Op . createExpr f)

parserPlus :: Parser CreateExpr
parserPlus = parseBinOp '+' Add

parserMinus :: Parser CreateExpr
parserMinus = parseBinOp '-' Sub

parserMul :: Parser CreateExpr
parserMul = parseBinOp '*' Mul

parserDiv :: Parser CreateExpr
parserDiv = parseBinOp '/' Div

------ >>>

-- << example
pAbbr :: Parser String
pAbbr = do
  abbr <- some (pSpecific isUpper)
  pEof
  pure abbr

------------ >>

------- parse expression
parseExpr :: String -> Except ParseError Expr
parseExpr = runP do
  expr <- parserEx
  parserSpace
  pEof
  pure expr

parserEx :: Parser Expr
parserEx = do
  term <- parserTerm
  parserEx1 term

parserEx1 :: Expr -> Parser Expr
parserEx1 lterm =
  parserOptional
    ( do
        constructor <- parserPlus <|> parserMinus
        rterm <- parserTerm
        parserEx1 $ constructor lterm rterm
    )
    lterm

parserTerm :: Parser Expr
parserTerm = do
  factor <- parserFactor
  parserTerm1 factor

parserTerm1 :: Expr -> Parser Expr
parserTerm1 lfactor =
  parserOptional
    ( do
        constructor <- parserMul <|> parserDiv
        rfactor <- parserFactor
        parserTerm1 $ constructor lfactor rfactor
    )
    lfactor

parserFactor :: Parser Expr
parserFactor =
  parserDouble
    <|> do
      void parserOpenParenthesis
      expr <- parserEx
      void parserCloseParenthesis
      pure expr

-------- >>>>

----- << parse Double
parserDouble :: Parser Expr
parserDouble = do
  parserSpace
  whole_part_arr <- parserNotEmptyNum
  fractional_part_arr <- parserOptionalAdditional
  let base = 10
  let getInt = foldl ((+) . (* base)) 0
  let whole_part = getInt whole_part_arr
  let fractional_part = getInt fractional_part_arr
  let power = length fractional_part_arr
  let offset = base ^ power
  let ratioValue = (whole_part * offset + fractional_part) % offset
  pure (Val (fromRational ratioValue))

parserNotEmptyNum :: Parser [Integer]
parserNotEmptyNum = some parserNum

parserOptionalAdditional :: Parser [Integer]
parserOptionalAdditional = parserOptional parserRequiredAdditional [0]

parserRequiredAdditional :: Parser [Integer]
parserRequiredAdditional = do
  void parserDot
  parserNotEmptyNum

parserNum :: Parser Integer
parserNum = msum [fmap (const $ toInteger i) (parserChar $ intToDigit i) | i <- [0 .. 9]]

----- >>
