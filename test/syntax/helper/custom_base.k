// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.custom_base
  { Foo
  , module mini-base
  }

import mini-base

inline uint32 Foo()
{
    return 0;
}
