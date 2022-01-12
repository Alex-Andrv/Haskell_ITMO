{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module HW3.Evaluator (eval) where

import Codec.Compression.Zlib
import Codec.Serialise (deserialise, serialise)
import Codec.Serialise.Class (Serialise)
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.ByteString as Byte (ByteString, append, drop, index, length, pack, take, unpack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (foldlM)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import Data.Sequence (Seq (Empty, (:|>)), empty, fromList, (><), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word8)
import HW3.Action
import HW3.Base
import System.Directory.Internal (Permissions)

type ET m = ExceptT HiError m HiValue -- alias

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval e = runExceptT (eval' e)

eval' :: HiMonad m => HiExpr -> ET m
eval' (HiExprValue hiValue) = return hiValue
eval' (HiExprRun hiExpr) = do
  hiValue <- eval' hiExpr
  case hiValue of
    HiValueAction hiAct -> lift $ runAction hiAct
    _ -> throwE HiErrorInvalidArgument
eval' (HiExprApply hiExpr hiExprs) = do
  expr <- eval' hiExpr
  case expr of
    HiValueFunction fun -> evalFun fun hiExprs
    HiValueString text -> evalSlice text (map eval' hiExprs)
    HiValueList list -> evalSlice list (map eval' hiExprs)
    HiValueBytes bytes -> evalSlice bytes (map eval' hiExprs)
    _ -> throwE HiErrorInvalidFunction

instance Serialise HiValue

instance Serialise HiFun

instance Serialise HiAction

class Slice t where
  lengthS :: t -> Int
  indexS :: t -> Int -> HiValue
  getSliceS :: t -> Int -> Int -> HiValue

instance Slice (Seq HiValue) where -- оооо damn, it works
  lengthS list = Seq.length list
  indexS list = Seq.index list
  getSliceS list start end = HiValueList (Seq.take (end - start) (Seq.drop start list))

instance Slice (Text.Text) where
  lengthS text = Text.length text
  indexS text i = HiValueString (Text.singleton (Text.index text i))
  getSliceS text start end = HiValueString (Text.take (end - start) (Text.drop start text))

instance Slice (Byte.ByteString) where
  lengthS bytes = Byte.length bytes
  indexS bytes i = HiValueNumber (toRational $ Byte.index bytes i)
  getSliceS bytes start end = HiValueBytes (Byte.take (end - start) (Byte.drop start bytes))

evalSlice :: (HiMonad m, Slice t) => t -> [ET m] -> ET m
evalSlice text [arg1] = do
  arg <- arg1
  case arg of
    HiValueNumber ind -> getByIndex text ind
    _ -> throwE HiErrorInvalidArgument
evalSlice text [arg1, arg2] = do
  a1 <- arg1
  a2 <- arg2
  evalSlice' text a1 a2
evalSlice _ _ = throwE HiErrorArityMismatch

getByIndex :: (HiMonad m, Slice t) => t -> Rational -> ET m
getByIndex text ind
  | (_isNatural ind) && (i < len) = return (indexS text i)
  | otherwise = return (HiValueNull)
  where
    len = lengthS text
    i = _toInt ind

evalSlice' :: (HiMonad m, Slice t) => t -> HiValue -> HiValue -> ET m
evalSlice' text HiValueNull r = evalSlice' text (HiValueNumber 0) r
evalSlice' text l HiValueNull = evalSlice' text l (HiValueNumber (toRational (lengthS text)))
evalSlice' text (HiValueNumber left) (HiValueNumber right)
  | (_isInteger left) && (_isInteger right) =
    let l = _toInt left
        r = _toInt right
     in return $ slice text l r
  | otherwise = throwE HiErrorInvalidArgument
evalSlice' _ _ _ = throwE HiErrorInvalidArgument

slice :: Slice t => t -> Int -> Int -> HiValue
slice text left right =
  let len = lengthS text
      normalize i
        | i < 0 = len + i
        | otherwise = i
      start = normalize left
      end = normalize right
   in getSliceS text start end

hiValueListCreate :: HiMonad m => [HiExpr] -> ET m
hiValueListCreate args =
  HiValueList
    <$> ( foldlM
            ( \hiValTail hiExpr -> do
                hiVal <- eval' hiExpr
                return (hiValTail |> hiVal)
            )
            empty
            args
        )

evalFun :: HiMonad m => HiFun -> [HiExpr] -> ET m
evalFun fun agrs = case fun of
  HiFunOr -> evalAndLazy agrs
  HiFunAnd -> evalOrLazy agrs
  HiFunIf -> evalIfLazy agrs
  f -> evalFunNoLazy f agrs

evalOrLazy :: HiMonad m => [HiExpr] -> ET m
evalOrLazy [arg1, arg2] = do
  a1 <- eval' arg1
  case a1 of
    HiValueBool True -> return $ HiValueBool True
    HiValueBool _ -> do
      a2 <- eval' arg2
      evalBinFun' (HiFunOr) a1 a2
    _ -> throwE HiErrorInvalidArgument
evalOrLazy _ = throwE HiErrorArityMismatch

evalAndLazy :: HiMonad m => [HiExpr] -> ET m
evalAndLazy [arg1, arg2] = do
  a1 <- eval' arg1
  case a1 of
    HiValueBool False -> return $ HiValueBool False
    HiValueBool _ -> do
      a2 <- eval' arg2
      evalBinFun' (HiFunAnd) a1 a2
    _ -> throwE HiErrorInvalidArgument
evalAndLazy _ = throwE HiErrorArityMismatch

evalIfLazy :: HiMonad m => [HiExpr] -> ET m
evalIfLazy [a1, a2, a3] = do
  a <- eval' a1
  case a of
    HiValueBool True -> eval' a2
    HiValueBool False -> eval' a3
    _ -> throwE HiErrorInvalidArgument
evalIfLazy _ = throwE HiErrorArityMismatch

evalFunNoLazy :: HiMonad m => HiFun -> [HiExpr] -> ET m
evalFunNoLazy fun [x] = evalUnaryFun fun x
evalFunNoLazy fun [x1, x2] = evalBinFun fun x1 x2
evalFunNoLazy HiFunList args = hiValueListCreate args
evalFunNoLazy _ _ = throwE HiErrorArityMismatch

evalUnaryFun :: HiMonad m => HiFun -> HiExpr -> ET m
evalUnaryFun fun arg = do
  a <- eval' arg
  evalUnaryFun' fun a

evalUnaryFun' :: HiMonad m => HiFun -> HiValue -> ET m
evalUnaryFun' HiFunNot a = case a of
  HiValueBool val -> return (HiValueBool (not val))
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunLength a = case a of
  HiValueString text -> return $ HiValueNumber $ toRational $ Text.length text
  HiValueList list -> return $ HiValueNumber $ toRational $ Seq.length list
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunReverse a = case a of
  HiValueString text -> return $ HiValueString $ Text.reverse text
  HiValueList list -> return $ HiValueList $ Seq.reverse list
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunPackBytes a =
  case a of
    HiValueList list
      | between0And255 list -> return $ HiValueBytes $ Byte.pack $ seq2List list
      | otherwise -> throwE HiErrorInvalidArgument
      where
        between0And255 =
          all
            ( \case
                HiValueNumber n -> n >= 0 && n <= 255
                _ -> False
            )
        seq2List =
          foldr
            ( \el liTail -> case el of
                (HiValueNumber n) -> (fromInteger $ _toInteger n) : liTail
                _ -> undefined
            )
            []
    _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunUnpackBytes a = case a of
  HiValueBytes bytes -> return $ HiValueList $ fromList $ map (HiValueNumber . toRational) $ Byte.unpack bytes
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunDecodeUtf8 a = case a of
  HiValueBytes bytes -> case decodeUtf8' bytes of
    Right ans -> return $ HiValueString $ ans
    Left _ -> return $ HiValueNull
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunEncodeUtf8 a = case a of
  HiValueString st -> return $ HiValueBytes $ encodeUtf8 st
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunZip a = case a of
  HiValueBytes bytes -> return $ HiValueBytes $ toStrict $ compressWith defaultCompressParams {compressLevel = bestCompression} $ fromStrict bytes
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunUnzip a = case a of
  HiValueBytes bytes -> return $ HiValueBytes $ toStrict $ decompressWith defaultDecompressParams $ fromStrict bytes
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunSerialise arg = return $ HiValueBytes (toStrict $ serialise arg)
evalUnaryFun' HiFunDeserialise a = case a of
  HiValueBytes bytes -> return $ deserialise $ fromStrict bytes
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunRead a = case a of
  HiValueString filePath -> return $ HiValueAction $ HiActionRead $ Text.unpack filePath
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunMkDir a = case a of
  HiValueString dirPath -> return $ HiValueAction $ HiActionMkDir $ Text.unpack dirPath
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunChDir a = case a of
  HiValueString newDirPath -> return $ HiValueAction $ HiActionChDir $ Text.unpack newDirPath
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunToUpper a = case a of
  HiValueString text -> return $ (HiValueString . Text.toUpper) text
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunToLower a = case a of
  HiValueString text -> return $ (HiValueString . Text.toLower) text
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' HiFunTrim a = case a of
  HiValueString text -> return $ (HiValueString . Text.strip) text
  _ -> throwE HiErrorInvalidArgument
evalUnaryFun' _ _ = throwE HiErrorArityMismatch

evalBinFun :: HiMonad m => HiFun -> HiExpr -> HiExpr -> ET m
evalBinFun fun arg1 arg2 = do
  a1 <- eval' arg1
  a2 <- eval' arg2
  evalBinFun' fun a1 a2

evalBinFun' :: HiMonad m => HiFun -> HiValue -> HiValue -> ET m
evalBinFun' HiFunDiv arg1 arg2 = case (arg1, arg2) of
  (HiValueNumber a1, HiValueNumber a2) 
    | a2 == 0 -> throwE HiErrorDivideByZero
    | otherwise -> return (HiValueNumber (a1 / a2))
  (HiValueString path1, HiValueString path2)
    -> return (HiValueString (Text.append (Text.snoc path1 '/') path2))
  _ -> throwE HiErrorInvalidArgument
evalBinFun' HiFunMul arg1 arg2 = case (arg1, arg2) of
  (HiValueNumber a1, HiValueNumber a2) -> return $ HiValueNumber $ a1 * a2
  (HiValueString text1, HiValueNumber cnt)
    | _isNatural cnt -> return (HiValueString (stimes (_toInt cnt) text1))
    | otherwise -> throwE HiErrorInvalidArgument
  (HiValueList list1, HiValueNumber cnt) -> return $ HiValueList $ stimes (_toInt cnt) list1
  (HiValueBytes bites, HiValueNumber cnt) -> return $ HiValueBytes $ stimes (_toInt cnt) bites
  _ -> throwE HiErrorInvalidArgument
evalBinFun' HiFunAdd arg1 arg2 = case (arg1, arg2) of
  (HiValueNumber a1, HiValueNumber a2) -> return $ HiValueNumber $ a1 + a2
  (HiValueString text1, HiValueString text2) -> return $ HiValueString $ Text.append text1 text2
  (HiValueList list1, HiValueList list2) -> return $ HiValueList $ list1 >< list2
  (HiValueBytes bites1, HiValueBytes bites2) -> return $ HiValueBytes $ Byte.append bites1 bites2
  _ -> throwE HiErrorInvalidArgument
evalBinFun' HiFunSub arg1 arg2 = case (arg1, arg2) of
  (HiValueNumber a1, HiValueNumber a2) -> return (HiValueNumber (a1 - a2))
  _ -> throwE HiErrorInvalidArgument
evalBinFun' HiFunRange arg1 arg2 = case (arg1, arg2) of
  (HiValueNumber a1, HiValueNumber a2) -> return $ HiValueList $ fromList $ map HiValueNumber [a1 .. a2]
  _ -> throwE HiErrorInvalidArgument
evalBinFun' HiFunFold arg1 arg2 = case (arg1, arg2) of
   (HiValueFunction _, HiValueList Empty) -> return (HiValueNull)
   (HiValueFunction fun, HiValueList list) -> foldlM1
                                                  (\f s -> evalBinFun' fun f s)
                                                  list
   _ -> throwE HiErrorInvalidArgument
evalBinFun' HiFunWrite arg1 arg2 = case (arg1, arg2) of
  (HiValueString filePath, HiValueString st) -> return $ HiValueAction $ HiActionWrite (Text.unpack filePath) (encodeUtf8 st)
  (HiValueString filePath, HiValueBytes bytes) -> return $ HiValueAction $ HiActionWrite (Text.unpack filePath) bytes
  _ -> throwE HiErrorInvalidArgument
evalBinFun' HiFunOr arg1 arg2 = case (arg1, arg2) of  
  (HiValueBool a1, HiValueBool a2) -> return $ HiValueBool $ a1 || a2
  _ -> throwE HiErrorInvalidArgument
evalBinFun' HiFunAnd arg1 arg2 = case (arg1, arg2) of 
  (HiValueBool a1, HiValueBool a2) -> return $ HiValueBool $ a1 && a2
  _ -> throwE HiErrorInvalidArgument
evalBinFun' HiFunEquals a1 a2 = return $ HiValueBool (a1 == a2)
evalBinFun' HiFunNotEquals a1 a2 = return $ HiValueBool (a1 /= a2)
evalBinFun' HiFunLessThan a1 a2 = return $ HiValueBool (a1 < a2)
evalBinFun' HiFunGreaterThan a1 a2 = return $ HiValueBool (a1 > a2)
evalBinFun' HiFunNotLessThan a1 a2 = return $ HiValueBool (a1 >= a2)
evalBinFun' HiFunNotGreaterThan a1 a2 = return $ HiValueBool (a1 <= a2)
evalBinFun' _ _ _ = throwE HiErrorArityMismatch

_isNatural :: Rational -> Bool -- Natural number and 0
_isNatural r = (_isInteger r) && (r >= 0)

_isInteger :: Rational -> Bool
_isInteger r = (denominator r) == 1

_toInt :: Rational -> Int
_toInt = fromInteger . numerator

_toInteger :: Rational -> Integer
_toInteger = numerator

foldlM1 :: HiMonad m => (HiValue -> HiValue -> ET m) -> Seq HiValue -> ET m
foldlM1 _ (Empty :|> x) = return x -- not a canonical definition.
foldlM1 fun (xs :|> s) = do
  f <- foldlM1 fun xs
  fun f s
foldlM1 _ _ = undefined
