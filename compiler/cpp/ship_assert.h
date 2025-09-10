// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Redirect assertions to a function that will throw an exception on release builds
#ifdef NDEBUG
#undef assert
#define assert(EXPR) KanagawaAssert((EXPR), __FILE__, __LINE__)
#endif
