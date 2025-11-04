{-# LANGUAGE CPP #-}

{-|
Copyright (c) Microsoft Corporation.
Licensed under the MIT License.
-}

module ReleaseVersion
  ( releaseVersion
  , releaseBuildChannel
  , BuildChannel
  , makeReleaseName
  ) where

import Data.Version

data BuildChannel = Local | Nightly | Release deriving (Show, Read)

releaseBuildChannel :: BuildChannel
releaseVersionMajor :: Int
releaseVersionMinor :: Int
releaseVersionPatch :: Int

#ifdef RELEASE_VERSION_MAJOR
releaseVersionMajor = RELEASE_VERSION_MAJOR
#else
releaseVersionMajor = 0
#endif

#ifdef RELEASE_VERSION_MINOR
releaseVersionMinor = RELEASE_VERSION_MINOR
#else
releaseVersionMinor = 0
#endif

#ifdef RELEASE_VERSION_PATCH
releaseVersionPatch = RELEASE_VERSION_PATCH
#else
releaseVersionPatch = 0
#endif

-- Convert numeric value to BuildChannel

#ifdef RELEASE_BUILD_CHANNEL
releaseBuildChannel = case RELEASE_BUILD_CHANNEL of
    1 -> Nightly
    2 -> Release
    _ -> Local
#else
releaseBuildChannel = Local
#endif

releaseVersion :: Version
releaseVersion = makeVersion [releaseVersionMajor, releaseVersionMinor, releaseVersionPatch]

makeReleaseName :: Version -> BuildChannel -> String
makeReleaseName version channel =
  let versionStr = showVersion version
  in case channel of
       Release -> versionStr
       Local -> versionStr ++ " (local)"
       Nightly -> versionStr ++ " (nightly)"
