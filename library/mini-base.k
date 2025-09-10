// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Copyright (c), Microsoft Corporation. All rights reserved.

This is a minimalist version of the base module for use in some
compiler tests and for the codechecker application that validates
Kanagawa sources embedded in markdown documents.
*/

module mini-base
    { module .cmdargs
    , module data.closure
    , module debug.print
    }

import .cmdargs
import data.closure
import debug.print
import compiler.config
import compiler.device.schema as device_schema

static assert (version <= device_schema::schema_version);
