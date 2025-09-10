// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module compiler.options
    { module compiler.options }

//| Value of `--backend` compiler option
enum Backend : uint2
{
    Sv,
    None
}