-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Char(module Data.Char, Char) where
import Primitives
import Data.Bool
import Data.Eq
import Data.Int

type String = [Char]

instance Eq Char where
  (==) = primCharEQ
  (/=) = primCharNE

--Y{-  Overlapping instance for ghc
instance Eq [Char] where
  (==) = primStringEQ
--Y-}

chr :: Int -> Char
chr = primChr

ord :: Char -> Int
ord = primOrd

isLower :: Char -> Bool
isLower c = (primCharLE 'a' c) && (primCharLE c 'z')

isUpper :: Char -> Bool
isUpper c = (primCharLE 'A' c) && (primCharLE c 'Z')

isAlpha :: Char -> Bool
isAlpha c = isLower c || isUpper c

isDigit :: Char -> Bool
isDigit c = (primCharLE '0' c) && (primCharLE c '9')

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

isPrint :: Char -> Bool
isPrint c = primCharLE ' ' c && primCharLE c '~'

eqChar :: Char -> Char -> Bool
eqChar = primCharEQ

neChar :: Char -> Char -> Bool
neChar = primCharNE

leChar :: Char -> Char -> Bool
leChar = primCharLE

ltChar :: Char -> Char -> Bool
ltChar = primCharLT

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n'

toLower :: Char -> Char
toLower c | leChar 'A' c && leChar c 'Z' = chr (ord c - ord 'A' + ord 'a')
          | True = c

toUpper :: Char -> Char
toUpper c | leChar 'a' c && leChar c 'a' = chr (ord c - ord 'a' + ord 'A')
          | True = c
