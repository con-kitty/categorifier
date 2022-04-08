{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module Categorifier.GHC.Builtin
  ( module PrelNames,
    module PrimOp,
    module TysPrim,
    module TysWiredIn,
    module Unique,
    pattern DoubleToFloatOp,
    pattern FloatToDoubleOp,
    pattern IntToDoubleOp,
    pattern IntToFloatOp,
    pattern WordToDoubleOp,
    pattern WordToFloatOp,
    integerFromInt64Name,
    integerToDoubleName,
    integerToFloatName,
  )
where

import qualified Categorifier.GHC.Types as Types
#if MIN_VERSION_ghc(9, 0, 0)
import GHC.Builtin.Names as PrelNames hiding
  ( integerFromInt64Name,
    integerToDoubleName,
    integerToFloatName,
  )
import qualified GHC.Builtin.Names as PrelNames
import GHC.Builtin.Types as TysWiredIn
import GHC.Builtin.Types.Prim as TysPrim
#if MIN_VERSION_ghc(9, 2, 0)
import GHC.Builtin.PrimOps as PrimOp hiding
  ( DoubleToFloatOp,
    FloatToDoubleOp,
    IntToDoubleOp,
    IntToFloatOp,
    WordToDoubleOp,
    WordToFloatOp,
  )
import qualified GHC.Builtin.PrimOps as PrimOp
import GHC.Builtin.Uniques as Unique
#else
import GHC.Builtin.PrimOps as PrimOp
import GHC.Types.Unique as Unique
#endif
#else
import PrelNames
import PrimOp
import TysPrim
import TysWiredIn
import Unique
#endif

pattern DoubleToFloatOp :: PrimOp

pattern FloatToDoubleOp :: PrimOp

pattern IntToDoubleOp :: PrimOp

pattern IntToFloatOp :: PrimOp

pattern WordToDoubleOp :: PrimOp

pattern WordToFloatOp :: PrimOp
#if MIN_VERSION_ghc(9, 2, 0)
pattern DoubleToFloatOp = PrimOp.DoubleToFloatOp
pattern FloatToDoubleOp = PrimOp.FloatToDoubleOp
pattern IntToDoubleOp = PrimOp.IntToDoubleOp
pattern IntToFloatOp = PrimOp.IntToFloatOp
pattern WordToDoubleOp = PrimOp.WordToDoubleOp
pattern WordToFloatOp = PrimOp.WordToFloatOp
#else
pattern DoubleToFloatOp = Double2FloatOp
pattern FloatToDoubleOp = Float2DoubleOp
pattern IntToDoubleOp = Int2DoubleOp
pattern IntToFloatOp = Int2FloatOp
pattern WordToDoubleOp = Word2DoubleOp
pattern WordToFloatOp = Word2FloatOp
#endif

integerFromInt64Name :: Types.Name
#if MIN_VERSION_ghc(9, 0, 0)
integerFromInt64Name = PrelNames.integerFromInt64Name
#else
integerFromInt64Name = smallIntegerName
#endif

integerToDoubleName :: Types.Name
#if MIN_VERSION_ghc(9, 0, 0)
integerToDoubleName = PrelNames.integerToDoubleName
#else
integerToDoubleName = doubleFromIntegerName
#endif

integerToFloatName :: Types.Name
#if MIN_VERSION_ghc(9, 0, 0)
integerToFloatName = PrelNames.integerToFloatName
#else
integerToFloatName = floatFromIntegerName
#endif
