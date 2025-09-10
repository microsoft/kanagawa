// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module base
    { optional
    , make_optional
    , pair
    , make_pair
    , first
    , second
    , array
    , index_t
    , count_t
    , bitindex_t
    , bitcount_t
    , float
    , size_t
    , module .cmdargs
    , module base.system
    , module control.async
    , module control.loop
    , module control.wait
    , module data.closure
    , module data.memory
    , module debug.print
    , module hardware.dsp
    , module type.coerce
    }

import .cmdargs
import base.system
import control.async
import control.loop
import control.wait
import data.array
import data.closure
import data.memory
import data.optional
import data.tuple
import debug.print
import hardware.dsp
import compiler.config
import compiler.device.schema as device_schema
import numeric.float32.operator as F
import type.coerce
import type.cstddef
import type.stdtype

// The device specific module `numeric.float32.operator` must expose the
// following functions which are used by the compiler to implement floating
// point operators.
static assert (decltype(F::mul) == (float32, float32) -> float32);
static assert (decltype(F::add) == (float32, float32) -> float32);
static assert (decltype(F::sub) == (float32, float32) -> float32);
static assert (decltype(F::mad) == (float32, float32, float32) -> float32);

// Check that the version of the device configuration schema is supported by the compiler.
static assert (version <= device_schema::schema_version);