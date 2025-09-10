// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module test.internal.params
    { module test.internal.params \ .cmdargs
    , module .cmdargs
    }

import .cmdargs

//| Default value for `tag` parameter.
// Override using `--define tag=foo` compiler command line options.
const auto tag = 0;

//| Default value for mask selects all tests, unless a specific tag is specified.
// Override using `--define mask=foo` compiler command line option.
const auto mask = 0;

//| Only print out failed tests by default.
// Override using `--define verbose=true` compiler command line option.
const auto verbose = false;
