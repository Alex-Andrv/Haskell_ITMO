{-# LANGUAGE DeriveGeneric #-}

module HW3.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiMonad (..)
  , HiValue (..)
  , HiAction (..)
  , HiMonad
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Sequence (Seq)
import GHC.Generics (Generic)

data HiFun

  = HiFunDiv --p7 l
  | HiFunMul --p7 l
  | HiFunAdd --p6 l
  | HiFunSub --p6 l

  | HiFunNot
  | HiFunAnd --p3 r
  | HiFunOr --p2 r
  | HiFunLessThan --p4
  | HiFunGreaterThan --p4
  | HiFunEquals --p4
  | HiFunNotLessThan --p4
  | HiFunNotGreaterThan --p4
  | HiFunNotEquals --p4
  | HiFunIf

  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
    
  | HiFunList
  | HiFunRange
  | HiFunFold
  
  | HiFunPackBytes
  | HiFunUnpackBytes 
  | HiFunZip 
  | HiFunDecodeUtf8
  | HiFunEncodeUtf8
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  
  
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  

  
  deriving (Show, Eq, Ord, Generic)

data HiValue
  = 
    HiValueBool Bool           
  | HiValueFunction HiFun

  | HiValueNumber Rational

  | HiValueNull
  | HiValueString Text
  
  | HiValueList (Seq HiValue)
  
  | HiValueBytes ByteString
  
  | HiValueAction HiAction

  deriving (Show, Eq, Ord, Generic)

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  deriving (Show, Eq, Ord)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq, Ord)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  deriving (Show, Eq, Ord, Generic)


class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue