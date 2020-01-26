{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Hs.Doc
  ( HsDocString
  , LHsDocString
  , mkHsDocString
  , mkHsDocStringUtf8ByteString
  , unpackHDS
  , hsDocStringToByteString
  , ppr_mbDoc

  , appendDocs
  , concatDocs

  , DeclDocMap(..)
  , emptyDeclDocMap

  , ArgDocMap(..)
  , emptyArgDocMap
  ) where

#include "HsVersions.h"

import GhcPrelude

import Binary
import Name
import Outputable
import SrcLoc

import Data.ByteString (ByteString)
import Data.Data
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import FastString

-- | Haskell Documentation String
--
-- Internally this is a UTF8-Encoded 'ByteString'.
-- Should probably just be ShortByteString, can't be ByteString as it ends up
-- in interface files.
newtype HsDocString = HsDocString FastString
  -- There are at least two plausible Semigroup instances for this type:
  --
  -- 1. Simple string concatenation.
  -- 2. Concatenation as documentation paragraphs with newlines in between.
  --
  -- To avoid confusion, we pass on defining an instance at all.
  deriving (Eq, Show, Data)

-- | Located Haskell Documentation String
type LHsDocString = Located HsDocString

instance Binary HsDocString where
  put_ bh (HsDocString bs) = put_ bh bs
  get bh = HsDocString <$> get bh

instance Outputable HsDocString where
  ppr = doubleQuotes . text . unpackHDS

mkHsDocString :: String -> HsDocString
mkHsDocString s = HsDocString (mkFastString s)

-- | Create a 'HsDocString' from a UTF8-encoded 'ShortByteString'.
mkHsDocStringUtf8ByteString :: ByteString -> HsDocString
mkHsDocStringUtf8ByteString = HsDocString . mkFastStringByteString

unpackHDS :: HsDocString -> String
unpackHDS = unpackFS . hsDocStringToByteString

-- | Return the contents of a 'HsDocString' as a UTF8-encoded 'ByteString'.
hsDocStringToByteString :: HsDocString -> FastString
hsDocStringToByteString (HsDocString bs) = bs

ppr_mbDoc :: Maybe LHsDocString -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

-- | Join two docstrings.
--
-- Non-empty docstrings are joined with two newlines in between,
-- resulting in separate paragraphs.
appendDocs :: HsDocString -> HsDocString -> HsDocString
appendDocs x y =
  fromMaybe
    (HsDocString nilFS)
    (concatDocs [x, y])

-- | Concat docstrings with two newlines in between.
--
-- Empty docstrings are skipped.
--
-- If all inputs are empty, 'Nothing' is returned.
concatDocs :: [HsDocString] -> Maybe HsDocString
concatDocs xs =
    if nullFS b
      then Nothing
      else Just (HsDocString b)
  where
    b = concatFS .
        intersperse (mkFastString "\n\n")
      . filter (not . nullFS)
      . map hsDocStringToByteString
      $ xs

-- | Docs for declarations: functions, data types, instances, methods etc.
newtype DeclDocMap = DeclDocMap (Map Name HsDocString)

instance Binary DeclDocMap where
  put_ bh (DeclDocMap m) = put_ bh (Map.toList m)
  -- We can't rely on a deterministic ordering of the `Name`s here.
  -- See the comments on `Name`'s `Ord` instance for context.
  get bh = DeclDocMap . Map.fromList <$> get bh

instance Outputable DeclDocMap where
  ppr (DeclDocMap m) = vcat (map pprPair (Map.toAscList m))
    where
      pprPair (name, doc) = ppr name Outputable.<> colon $$ nest 2 (ppr doc)

emptyDeclDocMap :: DeclDocMap
emptyDeclDocMap = DeclDocMap Map.empty

-- | Docs for arguments. E.g. function arguments, method arguments.
newtype ArgDocMap = ArgDocMap (Map Name (Map Int HsDocString))

instance Binary ArgDocMap where
  put_ bh (ArgDocMap m) = put_ bh (Map.toList (Map.toAscList <$> m))
  -- We can't rely on a deterministic ordering of the `Name`s here.
  -- See the comments on `Name`'s `Ord` instance for context.
  get bh = ArgDocMap . fmap Map.fromDistinctAscList . Map.fromList <$> get bh

instance Outputable ArgDocMap where
  ppr (ArgDocMap m) = vcat (map pprPair (Map.toAscList m))
    where
      pprPair (name, int_map) =
        ppr name Outputable.<> colon $$ nest 2 (pprIntMap int_map)
      pprIntMap im = vcat (map pprIPair (Map.toAscList im))
      pprIPair (i, doc) = ppr i Outputable.<> colon $$ nest 2 (ppr doc)

emptyArgDocMap :: ArgDocMap
emptyArgDocMap = ArgDocMap Map.empty
