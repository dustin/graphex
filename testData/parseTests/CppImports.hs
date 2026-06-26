{-# LANGUAGE CPP #-}
module CppImports where

#if MIN_VERSION_base(4,18,0)
import Data.Foo
#else
import Data.Bar
#endif
import Data.Baz
