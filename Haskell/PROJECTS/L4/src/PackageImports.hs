--- PackageImports
{-# LANGUAGE PackageImports #-}
module PackageImports where

import "unordered-containers" Data.HashSet

funcPackageImports1 = do
  putStrLn "Using {-# LANGUAGE PackageImports #-}"
  print $ singleton 'a'