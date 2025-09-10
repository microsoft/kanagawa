// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//
// Copyright (c), Microsoft Corporation. All rights reserved.
//

import data.array
import data.string
import test.unit as unit
import test.runner

inline void test_folds(unit::tag_t tag)
{
    uint32[5] a = {0, 1, 2, 3, 4};

    unit::assert_equal(tag, "01234", fold_map(show, a));
    unit::assert_equal(tag, "0\n1\n2\n3\n4\n", unlines(map(show, a)));
    unit::assert_equal(tag, "0 1 2 3 4", unwords(a));
    unit::assert_equal(tag, "0 1 2 3 4", unwords(map(show, a)));
    unit::assert_equal(tag, "0, 1, 2, 3, 4", punctuate(", ", a));
    unit::assert_equal(tag, "0, 1, 2, 3, 4", comma_sep(a));
    unit::assert_equal(tag, "foobar", concatenate({"foo", "bar"}));
    unit::assert_equal(tag, "----------", replicate<10>("-"));
    unit::assert_equal(tag, "(0),(1),(2),(3),(4)", punctuate(",", map(parens, a)));
    unit::assert_equal(tag, "(0, 1, 2, 3, 4)", parens(comma_sep(a)));
    unit::assert_equal(tag, "[0, 1, 2, 3, 4]", brackets(comma_sep(a)));
}

inline void test_bracketing(unit::tag_t tag)
{
    uint32[5] a = {0, 1, 2, 3, 4};
    bool b = true;

    unit::assert_equal(tag, "(true)", parens(b));
    unit::assert_equal(tag, "[true]", brackets(b));
    unit::assert_equal(tag, "{{true}", braces(b));
    unit::assert_equal(tag, "<true>", angles(b));
    unit::assert_equal(tag, "\"true\"", quotes(b));
    unit::assert_equal(tag, "'true'", squotes(b));
    unit::assert_equal(tag, "`true`", backquotes(b));
    unit::assert_equal(tag, "<[(true)]>", angles(brackets(parens(b))));
}

inline void test_main()
{
    unit::test<1>(test_folds);
    unit::test<1>(test_bracketing);
}
