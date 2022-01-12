module HW3.Parser where

import Control.Monad.Combinators.Expr (Operator (InfixL, InfixR, InfixN), makeExprParser)
import qualified Data.Text as Text
import Data.Void (Void)
import HW3.Base
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1, string)
import Data.Word(Word8)
import Data.ByteString(pack)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isHexDigit, digitToInt)

type Parser = Parsec Void String

-- <<---- fun parser section
hiFunLi :: [HiFun]
hiFunLi =
  [ HiFunDiv,
    HiFunMul,
    HiFunAdd,
    HiFunSub,
    HiFunAnd,
    HiFunOr,
    HiFunLessThan,
    HiFunGreaterThan,
    HiFunEquals,
    HiFunNotLessThan,
    HiFunNotGreaterThan,
    HiFunNotEquals,
    HiFunNot,
    HiFunIf,
    HiFunLength,
    HiFunToUpper,
    HiFunToLower,
    HiFunReverse,
    HiFunTrim,
    HiFunList,
    HiFunRange,
    HiFunFold,
    HiFunPackBytes,
    HiFunUnpackBytes,
    HiFunZip ,
    HiFunDecodeUtf8,
    HiFunEncodeUtf8,
    HiFunUnzip,
    HiFunSerialise,
    HiFunDeserialise,
    HiFunRead,
    HiFunWrite,
    HiFunMkDir,
    HiFunChDir
  ]

hiFun2StrRep :: HiFun -> String
hiFun2StrRep fun = case fun of
  HiFunDiv -> "div"
  HiFunMul -> "mul"
  HiFunAdd -> "add"
  HiFunSub -> "sub"
  HiFunNot -> "not"
  HiFunAnd -> "and"
  HiFunOr -> "or"
  HiFunLessThan -> "less-than"
  HiFunGreaterThan -> "greater-than"
  HiFunEquals -> "equals"
  HiFunNotLessThan -> "not-less-than"
  HiFunNotGreaterThan -> "not-greater-than"
  HiFunNotEquals -> "not-equals"
  HiFunIf -> "if"
  HiFunLength -> "length"
  HiFunToUpper -> "to-upper"
  HiFunToLower -> "to-lower"
  HiFunReverse -> "reverse"
  HiFunTrim -> "trim"
  HiFunList -> "list"
  HiFunRange -> "range"
  HiFunFold -> "fold"
  HiFunPackBytes -> "pack-bytes"
  HiFunUnpackBytes -> "unpack-bytes"
  HiFunZip  -> "zip"
  HiFunDecodeUtf8 -> "decode-utf8"
  HiFunEncodeUtf8 -> "encode-utf8"
  HiFunUnzip -> "unzip"
  HiFunSerialise -> "serialise"
  HiFunDeserialise -> "deserialise"
  HiFunRead -> "read"
  HiFunWrite -> "write"
  HiFunMkDir -> "mkdir"
  HiFunChDir -> "cd"
  _ -> undefined

hiFun' :: HiFun -> Parser HiFun
hiFun' construct = construct <$ parseSt (hiFun2StrRep construct)

hiFun :: Parser HiFun
hiFun =
  choice $ map hiFun' hiFunLi

-------- >>>>>

--- <<<---- value parser section
hiValue :: Parser HiValue
hiValue =
  choice
    [ hiValueNumber,
      hiValueBool,
      hiValueFunction,
      hiValueNull,
      hiValueString,
      hiActionCwd
    ]

hiActionCwd :: Parser HiValue
hiActionCwd = HiValueAction HiActionCwd <$ parseSt "cwd";

hiValueNumber :: Parser HiValue
hiValueNumber = HiValueNumber <$> signedRational

hiValueBool :: Parser HiValue
hiValueBool = HiValueBool <$> boolVal

hiValueFunction :: Parser HiValue
hiValueFunction = HiValueFunction <$> hiFun

hiValueNull :: Parser HiValue
hiValueNull = HiValueNull <$ parseSt "null"

hiValueString :: Parser HiValue
hiValueString = HiValueString . Text.pack <$ char '\"' <*> manyTill L.charLiteral (char '\"')

boolVal :: Parser Bool
boolVal =
  choice
    [ parseTrue,
      parseFalse
    ]

parseTrue :: Parser Bool
parseTrue = True <$ parseSt "true"

parseFalse :: Parser Bool
parseFalse = False <$ parseSt "false"

signedRational :: Parser Rational
signedRational = toRational <$> L.signed skipSpace L.scientific <* skipSpace

------------ >>>>>>

--------- <<<<< Expression parser section
operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ infixL HiFunDiv (parseStNotFollowedBy "/" "="),
      infixL HiFunMul (parseSt "*")
    ],
    [ infixL HiFunAdd (parseSt "+"),
      infixL HiFunSub (parseSt "-")
    ],
    [ infixN HiFunNotLessThan (parseSt ">="),
      infixN HiFunNotGreaterThan (parseSt "<="),
      infixN HiFunLessThan (parseSt "<"),
      infixN HiFunGreaterThan (parseSt ">"),
      infixN HiFunEquals (parseSt "=="),
      infixN HiFunNotEquals (parseSt "/=")
    ],
    [infixR HiFunAnd (parseSt "&&")],
    [infixR HiFunOr (parseSt "||")]
  ]

infixL :: HiFun -> Parser String -> Operator Parser HiExpr
infixL fun pars = InfixL (infix_ fun pars)

infixR :: HiFun -> Parser String -> Operator Parser HiExpr
infixR fun pars = InfixR (infix_ fun pars)

infixN :: HiFun -> Parser String -> Operator Parser HiExpr
infixN fun pars = InfixN (infix_ fun pars)

infix_ :: HiFun -> Parser String -> Parser (HiExpr -> HiExpr -> HiExpr)
infix_ fun pars = ((\f s -> HiExprApply (HiExprValue (HiValueFunction fun)) [f, s]) <$ pars)

infixExpr :: Parser HiExpr
infixExpr = makeExprParser hiExprRun operatorTable

parseByteList :: Parser HiExpr
parseByteList = HiExprValue . HiValueBytes . pack <$> between openSPS closeSPS bytes

parseList :: Parser HiExpr
parseList = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> between openSP closeSP hiExprSepBy

hiExprRun :: Parser HiExpr
hiExprRun = do expr <- hiExpr
               (HiExprRun expr <$ parseSt "!") <|> (return expr)

hiExpr :: Parser HiExpr -- LL(1) First = '(', 'HiVal', '[#', '['
hiExpr = do
  value <- hiExprValue <|> (parens infixExpr) <|> parseByteList <|> parseList
  tailExpr <- hiExpr'
  return (foldl HiExprApply value tailExpr)

hiExpr' :: Parser [[HiExpr]]
hiExpr' = many $ between openP closeP hiExprSepBy

hiExprSepBy :: Parser [HiExpr]
hiExprSepBy = sepBy infixExpr comma

hiExprValue :: Parser HiExpr
hiExprValue = HiExprValue <$> hiValue <* skipSpace

bytes :: Parser [Word8]
bytes = many byte

comma :: Parser String
comma = parseSt ","

openP :: Parser String
openP = parseSt "("

closeP :: Parser String
closeP = parseSt ")"

openSP :: Parser String
openSP = parseSt "["

closeSP :: Parser String
closeSP = parseSt "]"

openSPS :: Parser String
openSPS = parseSt "[#"

closeSPS :: Parser String
closeSPS = parseSt "#]"

------- <<<----- util section

parseStNotFollowedBy :: String -> String -> Parser String
parseStNotFollowedBy op notAllowed = try $ string op <* notFollowedBy (parseSt notAllowed) <* skipSpace

parseSt :: String -> Parser String
parseSt st = string st <* skipSpace

parens :: Parser a -> Parser a
parens = between openP closeP

byte :: Parser Word8
byte = toWord8 <$> hexDigitChar <*> hexDigitChar <* skipSpace
       where
         toWord8 f s = (_toWord8 f) * 16 + (_toWord8 s)
         _toWord8 c = fromIntegral (digitToInt c)

hexDigitChar :: Parser Char
hexDigitChar = satisfy isHexDigit

skipSpace :: Parser ()
skipSpace =
  L.space
    -- Like `space`, but skips 1 or more space characters.
    space1
    -- Skip from ;; until a newline.
    (L.skipLineComment ";;")
    -- Skip from /* until */. There is also `skipBlockComment`, but it doesn't handle nested comments.
    (L.skipBlockCommentNested "/*" "*/")

---- >>>>>>>>

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse =
  runParser
    -- Skip any whitespace at the beginning, expect the end of input after the atom.
    (between skipSpace eof infixExpr)
    -- Name of the source file, you can give it any name you want. I leave it blank for now.
    ""
