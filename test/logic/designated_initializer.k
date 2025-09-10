// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import test.unit as unit
import test.runner
import helper.designated_initializer_tests

inline void test_main()
{
    unit::test<1, 0>(Struct);
    unit::test<2, 0>(Union);

    unit::test<1, 1>(UnionInStruct);
    unit::test<2, 1>(StructInUnion);

    unit::test<1, 2>(ArrayInUnion);
    unit::test<2, 2>(StructInStruct);
    unit::test<2, 3>(DuplicateFields);

    unit::test<1, 3>(StructInArrayOrMemory<array>);
    unit::test<2, 3>(StructInArrayOrMemory<memory>);

    unit::test<2, 4>(StaticObjectGlobalFunctions);
    unit::test<3, 4>(StaticArrayOfObjects);

    unit::test<1, 5>(NestedObjectGlobalFunctionOuterStatic);
    unit::test<2, 5>(unit::fixture<NestedObjectGlobalFunctionOuterTest>());
    unit::test<3, 5>(NestedObjectCallbackPrioritzation);
    unit::test<4, 5>(CallbackPrioritzation);
    unit::test<5, 5>(CallbackToMethodStatic);
    unit::test<6, 5>(unit::fixture<CallbackToMethodGlobalTest>());

    unit::test<1, 6>(ThreeLevelNesting);
    unit::test<2, 6>(SelectiveOverride);
    unit::test<3, 6>(StaticLocalInline);
    unit::test<4, 6>(CallbackSetToModule);
    unit::test<5, 6>(ClassInOtherModule);
    unit::test<6, 6>(SiblingMethod);
    unit::test<7, 6>(EmptyInitializerList);

    unit::test<1, 7>(NestedClass);
    unit::test<2, 7>(CallbackAssignedToCallback);
    unit::test<3, 7>(InnerCallbackAssignedToCallback);
    unit::test<4, 7>(MemberArrayOfObjects);
    unit::test<5, 7>(NameConflict);
    unit::test<6, 7>(CallOuterMethodFromLambda);
    unit::test<7, 7>(MethodUsedBeforeObject);

    unit::test<1, 8>(unit::fixture<DefaultInitializer>());
    unit::test<2, 8>(unit::fixture<OuterDefaultInitializerIgnored1>());
    unit::test<3, 8>(unit::fixture<OuterDefaultInitializerIgnored2>());
    unit::test<4, 8>(unit::fixture<InitializeFromGrandParent>());
    unit::test<5, 8>(unit::fixture<DefaultInitializeToMethod>());
    unit::test<6, 8>(unit::fixture<SubTyping>());
}
