%%[99
-- Compiler knows the Int types of this module are defined here

module EHC.Int
  ( Int8, Int16, Int32, Int64
  )
  where

import EHC.Prelude

#include "MachineDependencies.h"
#include "IntLikeInstance.h"

data Int8
data Int16
data Int32
data Int64
%%]

%%[99
PRIMS_BOUNDED(Int64,primMinInt64,primMaxInt64)
PRIMS_CONVERSION_INTEGER(Int64,primIntegerToInt64,primInt64ToInteger)
PRIMS_CONVERSION_INT(Int64,primIntToInt64,primInt64ToInt)

PRIMS_EQ(Int64,primEqInt64)
PRIMS_ORD2(Int64,primCmpInt64,primLtInt64,primGtInt64)
PRIMS_NUM(Int64,primAddInt64,primSubInt64,primMulInt64,primNegInt64)
PRIMS_INTEGRAL1(Int64,primDivInt64,primModInt64,primQuotInt64,primRemInt64)

INSTANCE_EQ(Int64,primEqInt64)
INSTANCE_ORD2(Int64,primCmpInt64,primLtInt64,primGtInt64)
INSTANCE_BOUNDED(Int64,primMinInt64,primMaxInt64)
INSTANCE_NUM(Int64,primAddInt64,primSubInt64,primMulInt64,primNegInt64,primIntegerToInt64,primIntToInt64)
INSTANCE_ENUM(Int64,primIntToInt64,primInt64ToInt)
INSTANCE_REAL(Int64)
INSTANCE_INTEGRAL1(Int64,primDivInt64,primModInt64,primQuotInt64,primRemInt64,primInt64ToInteger,primInt64ToInt)
INSTANCE_SHOW(Int64)

%%]

If possible (when 32 bits fit into Int), use Int stuff, otherwise boxed with additional primitives.

%%[99
PRIMS_BOUNDED(Int32,primMinInt32,primMaxInt32)
PRIMS_CONVERSION_INTEGER(Int32,primIntegerToInt32,primInt32ToInteger)
PRIMS_CONVERSION_INT(Int32,primIntToInt32,primInt32ToInt)

#if USE_32_BITS
PRIMS_EQ(Int32,primEqInt32)
PRIMS_ORD2(Int32,primCmpInt32,primLtInt32,primGtInt32)
PRIMS_NUM(Int32,primAddInt32,primSubInt32,primMulInt32,primNegInt32)
PRIMS_INTEGRAL1(Int32,primDivInt32,primModInt32,primQuotInt32,primRemInt32)
#else
PRIMS2_EQ(Int32,primEqInt32,"primEqInt")
PRIMS2_ORD2(Int32,primCmpInt32,"primCmpInt",primLtInt32,"primLtInt",primGtInt32,"primGtInt")
PRIMS2_NUM(Int32,primAddInt32,"primAddInt",primSubInt32,"primSubInt",primMulInt32,"primMulInt",primNegInt32,"primNegInt")
PRIMS2_INTEGRAL1(Int32,primDivInt32,"primDivInt",primModInt32,"primModInt",primQuotInt32,"primQuotInt",primRemInt32,"primRemInt")
#endif

INSTANCE_EQ(Int32,primEqInt32)
INSTANCE_ORD2(Int32,primCmpInt32,primLtInt32,primGtInt32)
INSTANCE_BOUNDED(Int32,primMinInt32,primMaxInt32)
INSTANCE_NUM(Int32,primAddInt32,primSubInt32,primMulInt32,primNegInt32,primIntegerToInt32,primIntToInt32)
INSTANCE_ENUM(Int32,primIntToInt32,primInt32ToInt)
INSTANCE_REAL(Int32)
INSTANCE_INTEGRAL1(Int32,primDivInt32,primModInt32,primQuotInt32,primRemInt32,primInt32ToInteger,primInt32ToInt)
INSTANCE_SHOW(Int32)

%%]

%%[99
PRIMS_BOUNDED(Int16,primMinInt16,primMaxInt16)
PRIMS2_CONVERSION_INTEGER(Int16,primIntegerToInt16,"primIntegerToInt",primInt16ToInteger,"primIntToInteger")
PRIMS_CONVERSION_INT(Int16,primIntToInt16,primInt16ToInt)

PRIMS2_EQ(Int16,primEqInt16,"primEqInt")
PRIMS2_ORD2(Int16,primCmpInt16,"primCmpInt",primLtInt16,"primLtInt",primGtInt16,"primGtInt")
PRIMS2_NUM(Int16,primAddInt16,"primAddInt",primSubInt16,"primSubInt",primMulInt16,"primMulInt",primNegInt16,"primNegInt")
PRIMS2_INTEGRAL1(Int16,primDivInt16,"primDivInt",primModInt16,"primModInt",primQuotInt16,"primQuotInt",primRemInt16,"primRemInt")

INSTANCE_EQ(Int16,primEqInt16)
INSTANCE_ORD2(Int16,primCmpInt16,primLtInt16,primGtInt16)
INSTANCE_BOUNDED(Int16,primMinInt16,primMaxInt16)
INSTANCE_NUM(Int16,primAddInt16,primSubInt16,primMulInt16,primNegInt16,primIntegerToInt16,primIntToInt16)
INSTANCE_ENUM(Int16,primIntToInt16,primInt16ToInt)
INSTANCE_REAL(Int16)
INSTANCE_INTEGRAL1(Int16,primDivInt16,primModInt16,primQuotInt16,primRemInt16,primInt16ToInteger,primInt16ToInt)
INSTANCE_SHOW(Int16)

%%]

%%[99
PRIMS_BOUNDED(Int8,primMinInt8,primMaxInt8)
PRIMS2_CONVERSION_INTEGER(Int8,primIntegerToInt8,"primIntegerToInt",primInt8ToInteger,"primIntToInteger")
PRIMS_CONVERSION_INT(Int8,primIntToInt8,primInt8ToInt)

PRIMS2_EQ(Int8,primEqInt8,"primEqInt")
PRIMS2_ORD2(Int8,primCmpInt8,"primCmpInt",primLtInt8,"primLtInt",primGtInt8,"primGtInt")
PRIMS2_NUM(Int8,primAddInt8,"primAddInt",primSubInt8,"primSubInt",primMulInt8,"primMulInt",primNegInt8,"primNegInt")
PRIMS2_INTEGRAL1(Int8,primDivInt8,"primDivInt",primModInt8,"primModInt",primQuotInt8,"primQuotInt",primRemInt8,"primRemInt")

INSTANCE_EQ(Int8,primEqInt8)
INSTANCE_ORD2(Int8,primCmpInt8,primLtInt8,primGtInt8)
INSTANCE_BOUNDED(Int8,primMinInt8,primMaxInt8)
INSTANCE_NUM(Int8,primAddInt8,primSubInt8,primMulInt8,primNegInt8,primIntegerToInt8,primIntToInt8)
INSTANCE_ENUM(Int8,primIntToInt8,primInt8ToInt)
INSTANCE_REAL(Int8)
INSTANCE_INTEGRAL1(Int8,primDivInt8,primModInt8,primQuotInt8,primRemInt8,primInt8ToInteger,primInt8ToInt)
INSTANCE_SHOW(Int8)

%%]