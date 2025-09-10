// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|
Module with doc comments
*/

module helper.doc_comment_module
    { //| Foo and friends
      Foo //< Foo struct
    , make_foo
    , //| Bar and friends
      Bar //< Bar
    , make_bar
    }


struct Foo{};

struct Bar{};

inline Foo make_foo()
{
    return {};
}

inline Bar make_bar()
{
    return {};
}
