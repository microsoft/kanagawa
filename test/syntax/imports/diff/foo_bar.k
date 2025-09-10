// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module diff.foo_bar
    { module diff.foo \ diff.bar
    , module diff.bar
    }

import diff.foo
import diff.bar
