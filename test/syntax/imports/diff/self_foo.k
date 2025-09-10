// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module diff.self_foo
    { module diff.self_foo \ diff.foo
    , module diff.foo
    }

import diff.foo

using foo_t = bool;

const foo_t foo = false;

const auto self = 99;
