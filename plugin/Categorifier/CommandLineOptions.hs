{-# LANGUAGE OverloadedStrings #-}

-- | Manages and parses /all/ the options for every part of the plugin. This is because the set of
--   command-line options is shared across the entire plugin, so we can't have each component check
--   for its own plugins while also erroring if we get unrecognized options.
module Categorifier.CommandLineOptions
  ( OptionGroup (..),
    partitionOptions,
  )
where

import qualified Categorifier.GHC.Driver as GhcPlugins
import Data.Bifunctor (Bifunctor (..))
import Data.Either.Validation (Validation (..))
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tuple.Extra (firstM)

-- | An enumeration of the various command-line option groups the plugin supports.
data OptionGroup
  = AdditionalBoxersOptions
  | AutoInterpreterOptions
  | BenchmarkOption
  | DebugOption
  | DeferFailuresOption
  | HierarchyOptions
  | LookupOptions
  | MakerMapOptions
  deriving (Eq, Ord)

groupFromText :: Text -> Maybe OptionGroup
groupFromText = \case
  "additional-boxers" -> pure AdditionalBoxersOptions
  "autointerpreter" -> pure AutoInterpreterOptions
  "benchmark" -> pure BenchmarkOption
  "debug" -> pure DebugOption
  "defer-failures" -> pure DeferFailuresOption
  "hierarchy" -> pure HierarchyOptions
  "lookup" -> pure LookupOptions
  "maker-map" -> pure MakerMapOptions
  _ -> Nothing

-- | We expect all options to be in the format `<group>:<value>`, and we (often) allow duplicate
--   entries in the same group. This splits them so that we have a list of values for each group
--  (with the order within the group maintained).
--
--   This fails (returning the failing group names) if any of the option groups are unrecognized.
--
--  __NB__: GHC hands us the options in reverse order
--         (https://gitlab.haskell.org/ghc/ghc/-/issues/17884), so we fix the order here. At some
--          point, GHC will hopefully fix this, which means we'll need to cunditionalize the
--          reversal for a time.
partitionOptions ::
  [GhcPlugins.CommandLineOption] -> Validation (NonEmpty Text) (Map OptionGroup [Text])
partitionOptions =
  fmap (Map.fromListWith (<>))
    . traverse
      ( \opt ->
          firstM findKey . maybe (Text.pack opt, []) (bimap Text.pack (pure . Text.pack)) $
            splitAroundElem separator opt
      )
    . reverse
  where
    separator = ':'
    splitAroundElem :: Eq a => a -> [a] -> Maybe ([a], [a])
    splitAroundElem e as = fmap (drop 1) . flip splitAt as <$> elemIndex e as
    findKey :: Text -> Validation (NonEmpty Text) OptionGroup
    findKey k = maybe (Failure $ pure k) pure $ groupFromText k
