// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module diff.foo_self
    { module diff.foo \ diff.foo_self
    , module diff.foo_self
    }

import diff.foo

using foo_t = bool;

const foo_t foo = false;

const auto self = 99;
