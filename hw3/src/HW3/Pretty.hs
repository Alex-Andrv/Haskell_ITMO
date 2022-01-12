module HW3.Pretty where

import qualified Data.ByteString as Byte (ByteString, null, unpack)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Ratio (denominator, numerator)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import Data.Sequence (Seq (Empty))
import Data.Text (Text, unpack)
import Data.Word (Word8)
import HW3.Base
import HW3.Parser (hiFun2StrRep)
import Numeric (showHex)
import Prettyprinter (Doc, annotate, pretty)
import Prettyprinter.Render.Terminal.Internal (AnsiStyle, Color (Green), bold, color)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue val = annotate (color Green <> bold) (pretty $ getString val)

getString :: HiValue -> String
getString val = case val of
  HiValueNumber x -> prettyNumber x
  HiValueBool x -> prettyBool x
  HiValueFunction x -> prettyFun x
  HiValueString x -> prettyString x
  HiValueList li -> prettyList li
  HiValueNull -> "null"
  HiValueBytes bytes -> prettyBytes bytes
  HiValueAction act -> prettyAct act
  _ -> undefined

prettyBool :: Bool -> String
prettyBool True = "true"
prettyBool False = "false"

prettyNumber :: Rational -> String
prettyNumber x = _prettyNumber x (numerator x) (denominator x)

_prettyNumber :: Rational -> Integer -> Integer -> String
_prettyNumber _ x 1 = show x
_prettyNumber number x y
  | (isFiniteDecimalFractions y) =
    let (val, _) = fromRationalRepetendUnlimited number
     in formatScientific Fixed Nothing val
  | abs (x) < y = ((show x) ++ "/" ++ (show y))
  | abs (x) > y =
    let (integer, remain) = (quotRem x y)
     in if remain > 0
          then ((show integer) ++ " + " ++ (show remain) ++ "/" ++ (show y))
          else ((show integer) ++ " - " ++ (show (abs remain)) ++ "/" ++ (show y))
_prettyNumber _ _ _ = undefined

isFiniteDecimalFractions :: Integer -> Bool
isFiniteDecimalFractions 1 = True
isFiniteDecimalFractions x
  | even x = isFiniteDecimalFractions (div x 2)
  | mod x 5 == 0 = isFiniteDecimalFractions (div x 5)
  | otherwise = False

prettyFun :: HiFun -> String
prettyFun = hiFun2StrRep

prettyString :: Text -> String
prettyString text = "\"" ++ (unpack text) ++ "\""

prettyList :: Seq HiValue -> String
prettyList Empty = "[ ]"
prettyList li = "[ " ++ (intercalate ", " (map getString (toList li))) ++ " ]"

prettyAct :: HiAction -> String
prettyAct act = let wrap st = "\"" ++ st ++ "\"" in
  case act of
    HiActionRead filePath -> (hiFun2StrRep HiFunRead) ++ "(" ++ (wrap filePath) ++ ")"
    HiActionWrite filePath bytes -> (hiFun2StrRep HiFunWrite) ++ "(" ++ (wrap filePath) ++ ", " ++ (prettyBytes bytes) ++ ")"
    HiActionMkDir filePath -> (hiFun2StrRep HiFunMkDir) ++ "(" ++ (wrap filePath) ++ ")"
    HiActionChDir filePath -> (hiFun2StrRep HiFunChDir) ++ "(" ++ (wrap filePath) ++ ")"
    HiActionCwd -> "cwd"

prettyBytes :: Byte.ByteString -> String
prettyBytes bytes
  | Byte.null bytes = "[# #]"
  | otherwise = "[# " ++ (unwords (map _showHex (Byte.unpack bytes))) ++ " #]"

_showHex :: Word8 -> String -- TODO improve
_showHex n
  | n < 16 = '0' : (showHex n "")
  | otherwise = showHex n ""
