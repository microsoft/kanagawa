// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import risc_v_wrapper
import processor.risc_v
import .cmdargs as Config

[[name("risc_v_wrapper")]]
export risc_v_wrapper<Config::HARTS, ENABLE_M ? Extension::M : Extension::None>;
