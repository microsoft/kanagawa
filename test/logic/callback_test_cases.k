// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner
import helper.callback_tests
import helper.callbacks as cb

inline void test_main()
{
    // 1 test for global objects, the others use static locals
    unit::test<1, 0>(unit::fixture<AssignDuringMemberDeclToInlineHelper>());
    unit::test<2, 0>(TestObject<AssignDuringMemberDeclToNonInlineHelper>);
    unit::test<3, 0>(TestObject<AssignDuringMemberDeclToMemberHelper>);
    unit::test<4, 0>(TestObject<DoubleDereferenceHelper>);
    unit::test<5, 0>(TestObject<cb::CallbackTester>);
    unit::test<6, 0>(TestObject<AsyncCallbackHelper>);
    unit::test<7, 0>(TestObject<CallbackInitializedToMemberHelper>);

    unit::test<1, 1>(TestObject<CallbackInitializedToMemberArrayElementHelper>);
    unit::test<2, 1>(TestObject<NestedObjectHelper>);
    unit::test<3, 1>(TestObject<NestedClassHelper>);
    unit::test<4, 1>(TestObject<ScopeHelper>);
    unit::test<5, 1>(TestObject<ParamSubtypingHelper>);
    unit::test<6, 1>(TestObject<ParamSubtypingNestedObjectHelper>);
    unit::test<7, 1>(TestObject<ReturnSubtypingHelper>);

    unit::test<1, 2>(TestObject<ReturnSubtypingNestedHelper>);
    unit::test<2, 2>(TestObject<StructInOut>);
    unit::test<3, 2>(CallbackFromHof);
    unit::test<4, 2>(LambdaDemux);
    unit::test<5, 2>(unit::fixture<DeclaredInModuleTest>());
    unit::test<6, 2>(ConstParamTest);

    unit::test<1, 3>(NestedObjectInitializerTest);
    unit::test<1, 4>(InitializerBeforeDeclarationTest);
}
