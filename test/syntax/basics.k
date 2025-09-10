// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    // shift by signed amount type inference
    inline void main()
    {
        int5 amt = -1;
        uint32 x = 3;
        auto y = x << amt;

        // 5-bit shift (reinterpreted as unsigned)
        static assert(bitsizeof(y) == 63);
    }

expected:0

    // uint - uint type inference
    inline void main()
    {
        uint8 a = 123;
        uint8 b = 456;

        auto c = a - b;
        static assert(bitsizeof(c) == 9);
    }

expected:0

    // static for with negative bound
    inline void main()
    {
        static for(const auto i : -3)
        {
        }
    }

expected:1

    // clog2 of negative amount
    inline void main()
    {
        println(clog2(-1));
    }

expected:1

    // shift by negative amount
    inline void main()
    {
        static assert((3 << -1) == 1);
    }

expected:0

    // shift by negative amount
    inline void main()
    {
        static assert((3 >> -1) == 6);
    }

expected:0

    // subtract results in an expression with 1 bit wider type
    inline void main()
    {
        int8 x = 3;

        auto y = x - 1;
        static assert(bitsizeof(y) == 9);
    }

expected:0

    // add results in an expression with 1 bit wider type
    inline void main()
    {
        uint8 x = 3;

        auto y = x + 1;
        static assert(bitsizeof(y) == 9);
    }

expected:0

    // array read with negative index
    class C
    {
        uint32[8] _array;

    public:
        void main()
        {
            println(_array[-1]);
        }
    }

    export C;

expected:9

    // array write with negative index

    class C
    {
        uint32[8] _array;
    public:
        void main()
        {
            _array[-1] = 3;
            println(_array);
        }
    }

    export C;

expected:9

    using t=int32;
    const t x = 1;

    template <typename T>
    class Foo
    {
        static assert(x == 1);
    }

    inline void main()
    {
        static Foo<bool> foo;
    }

expected:0

    // Constant parameter for inline function call
    class Foo525
    {
        uint32 _val;

    public:
        inline void Set(uint32 v)
        {
            _val = v;
        }

        uint32 Get()
        {
            return _val;
        }
    }

    class C
    {
        Foo525[2] _objects525;

        inline uint32 GetVal(const uint1 objectId)
        {
            return _objects525[objectId].Get();
        }

    public:
        void main()
        {
            _objects525[0].Set(6);
            _objects525[1].Set(123);

            assert(6 == GetVal(0));
            assert(123 == GetVal(1));
        }
    }

    export C;

expected:0

    import hardware.dsp

    // multiplication result size
    inline void main(uint25 x, uint37 y)
    {
        auto a = x * y;
        static assert(62 == bitsizeof(a));
    }

expected:0

    import hardware.dsp

    // multiplication result size
    inline void main(int25 x, uint37 y)
    {
        auto a = x * y;
        static assert(62 == bitsizeof(a));
    }

expected:0

    import hardware.dsp

    // multiplication result size
    inline void main(int25 x, int37 y)
    {
        auto a = x * y;
        static assert(62 == bitsizeof(a));
    }

expected:0

    // 0-initializing a memory

    inline void main()
    {
        static [[memory]] uint32[512] m_Credits = {};
        println(m_Credits[0]);
    }

expected:0

    class Main
    {
        // max_threads is not literal
        const auto ThreadCount = 2;

    public:
        [[max_threads(ThreadCount)]] void main()
        {
        }
    }

    export Main;

expected:0

    // max_threads is not constant
    class Main
    {
        auto ThreadCount = 2;

    public:
        [[max_threads(ThreadCount)]] void main()
        {
        }
    }

    export Main;

expected:1

    // schedule(0)

    inline void main()
    {
        static [[memory]] uint32[512] _mem;
        uint32 slotIndex;

        [[schedule( 0)]]
        {
            auto val = _mem[slotIndex];
            _mem[1] = 3;
            _mem[slotIndex] = val;
        }
    }

expected:57

    // schedule(N) not known at compile time
    inline void main()
    {
        uint32 UpdateRate = 2;
        static [[memory]] uint32[512] _mem;
        uint32 slotIndex;

        [[schedule( UpdateRate)]]
        {
            auto val = _mem[slotIndex];
            _mem[1] = 3;
            _mem[slotIndex] = val;
        }
    }

expected:1

    // clog2 value not known at compile time
    inline void main()
    {
        uint32 x = 3;

        println(clog2(x));
    }

expected:1

    // clog2 on 0
    inline void main()
    {
        println(clog2(0));
    }

expected:1

    // clog2 on signed integer
    inline void main()
    {
        const int32 x = 3;

        println(clog2(x));
    }

expected:0

    // clog2 on a struct
    struct S
    {
        uint32 x;
    }

    inline void main()
    {
        S s;

        println(clog2(s));
    }

expected:1
    // static local class
    class C622
    {
    }

    inline void main()
    {
        static C622 helper;
    }

expected:0

    // static global
    static uint32 g = 3;

expected:1

    // static global
    static uint32 g;

expected:1

    // static referenced outside scope
    inline void main()
    {
        static uint32 x = 0;
    }

    inline void main()
    {
        println(x);
    }

expected:1


    // local declaration with namespace
    inline void main()
    {
        uint32 testnamespace::x = 3;
    }

expected:1

    // local declaration with namespace
    inline void main()
    {
        uint32 testnamespace::x;
    }

expected:1


    // object.namespace::foo()
    class C
    {
    public:
        inline void Foo()
        {
            print("help\n");
        }
    }


    inline void main()
    {
        static C _c;
        _c.testnamespace::Foo();
    }

expected:1

    // memory with uint<N> syntax
    inline void main()
    {
        static [[memory]] uint<8>[512] _scheduleable;
    }

expected:0

    // return void - used to crash the compiler
    inline void foo()
    {
    }

    inline void main()
    {
        return foo();
    }

expected:0

    // test for protection modifiers associated with reordering of member variables that occurs
    class C
    {
    public:
        uint32 Foo()
        {
            return x;
        }

    private:
        uint32 x;
    }

    inline void main()
    {
        static C _c;
        println(_c.Foo());
    }

expected:0

    // test for protection modifiers associated with reordering of member variables that occurs
    class C
    {
    private:
        uint32 Foo()
        {
            return x;
        }

    public:
        uint32 x;
    }


    inline void main()
    {
        static C _c;
        println(_c.Foo());
    }

expected:29

    // invalid src for lutmul
    inline void main()
    {
        float32 a;
        float32 b;

        auto x = lutmul(a, b);
    }

expected:16

    // invalid src for lutmul
    struct Bar
    {
        uint8 x;
    }

    inline void main()
    {
        Bar b;
        Bar c;

        auto x = lutmul(b, c);
    }

expected:1

    // lutmul return types
    inline void main()
    {
        int8 a = 3;
        int4 b = 9;

        auto x = lutmul(a, b);
        static assert(bitsizeof(x) == 12);
    }

expected:0

    // lutmul return types
    inline void main()
    {
        uint8 a = 3;
        uint4 b = 9;

        auto x = lutmul(a, b);
        static assert(bitsizeof(x) == 12);
    }

expected:0

    // lutmul, mixing signed and unsigned types
    inline void main()
    {
        uint8 a = 4;
        int8 b = -2;

        auto x = lutmul(a, b);
    }

expected:0

    class Main
    {
        // startup has a parameter
        [[reset]] void startup(uint64 x)
        {
        }
    }

    export Main;

expected:1

    class Main
    {
        // startup async
        [[async]] [[reset]] void startup()
        {
        }
    }

    export Main;

expected:18

    // static assert expression is not a bool
    static assert(1);

expected:1

    // static assert at class scope
    class Bar
    {
        static assert(true);
    }

    inline void main()
    {
        static Bar _bar;
    }

expected:0

    // static assert at class scope
    class Bar
    {
        static assert(false);
    }

    inline void main()
    {
        static Bar _bar;
    }

expected:1

    // global static assert that evalautes to false
    const auto Foo = 3 + 4;

    static assert(Foo == 8);

expected:1

    // global static assert
    const auto Foo = 3 + 4;

    static assert(Foo == 7);

expected:0

    // using a nested class from the outside
    class Foo
    {
    public:
        class Bar
        {
        }
    }

    inline void main()
    {
        static Bar _bar;
    }

expected:1

    // typedef going out of scope
    class Bar494
    {
    public:
        using MyType=uint3;
    }

    inline void main()
    {
        static MyType _x;
    }

expected:1

    // wrong number of template params
    template <uint32 var>
    class Foo
    {
    }

    using A=Foo<3,4>;

expected:1

    // template typedef refers to non-existant class
    using A=Foo<uint32>;

expected:1

    // duplicate template class name
    template <uint32 var>
    class Foo
    {
    }

    template <uint32 var>
    class Foo
    {
    }

expected:1

    // template class that does not have a "}"
    template <uint32 var>
    class Foo
    {

expected:1

    // template class that does not have a "{"
    template <uint32 var>
    class Foo
    }

expected:1

    // 2 template typedefs going to the same value
    template <uint32 var>
    class Foo
    {
    }

    using A=Foo<3>;
    using A=Foo<4>;

expected:1

    // 2 template parameters with the same name
    template<uint32 var, uint16 var>
    class Foo
    {
    }

    using MyFoo=Foo<2, 3>;

expected:1

    // thread rate on inline function
    [[thread_rate(1)]] inline void main()
    {
    }

expected:18

    class Main
    {
        // thread rate = 0
        [[thread_rate(0)]] void main()
        {
        }
    }

    export Main;

expected:18

    class Main
    {
        // thread rate specified twice
        [[thread_rate(1), thread_rate(1)]] void main()
        {
        }
    }

    export Main;

expected:18

    // typedef where type is not a typename
    using A32=asdf;

    inline void main()
    {
        static A32 x;
    }

expected:1

    // duplicate typedef
    using A32=uint16;
    using A32=uint32;

expected:1

    // typedef a built-in type
    using uint32=uint16;

expected:1

    // repro of an epc bug
    // Shifting by a large variable causes a verilog variable to have -1 width

    inline uint64 Foo436(uint32 a, uint32 shiftAmount)
    {
        return (a << shiftAmount);
    }

    inline void main()
    {
        uint32 _shiftAmount436 = 5;

        assert(128 == Foo436(4, _shiftAmount436));
    }

expected:1

    // Test that ordered state is popped back at the end of a reorder block

    inline void Foo(uint32 x)
    {
        static uint32 _x;
        reorder
        {
            for (const auto i : x)
            {
                _x++;
            }
        }

        [[unordered]] for (const auto i : x)
        {
            _x++;
        }
    }

    class C
    {
    public:
        void main(uint32 x)
        {
            Foo(x);
        }
    }

    export C;

expected:12

    // unsupported float32 operator
    inline void main()
    {
        float32 a = 4.3;
        float32 b = 3.7;

        float32 c = a & b;
    }

expected:1

    // static assert unsuccessful
    inline void main()
    {
        static assert(false);
    }

expected:1

    // static assert successful
    inline void main()
    {
        static assert(true);
    }

expected:0

    // static assert not known
    inline void main()
    {
        uint8 a;
        bool b = (a == 2);
        static assert(b);
    }

expected:1

    // inline function with max_threads modifier
    [[max_threads(1)]] inline void main()
    {
    }

expected:18

    // call_rate specified twice
    inline void Bar()
    {
    }

    inline void main()
    {
        [[call_rate(1), call_rate(1)]] Bar();
    }

expected:65

    // Offsetof with bad member name
    struct Foo
    {
        uint32 _x;
        uint32 _y;
    }

    inline void main()
    {
        auto x = bitoffsetof(Foo, _z);
    }

expected:1

    // Offsetof with non-struct type
    inline void main()
    {
        auto x = bitoffsetof(uint32, x);
    }

expected:1

    // FIFO_depth specified more than once
    inline void Foo()
    {
    }

    class C
    {
        void main(bool b)
        {
            [[fifo_depth(3), fifo_depth(5)]] Foo();
        }
    }

    export C;

expected:65

    import hardware.dsp

    // type checking for fmac32 param
    inline void main()
    {
        float32 p = fmac32(3, 4, false);
    }

expected:1

    import hardware.dsp

    // type checking for fadd32 result
    inline void main()
    {
        int32 p = fmac32(3.0, 4.1, false);
    }

expected:1

    import hardware.dsp

    // type checking for fadd32 param
    inline void main()
    {
        float32 p = fadd32(3, 4);
    }

expected:1

    import hardware.dsp

    // type checking for fadd32 result
    inline void main()
    {
        int32 p = fadd32(3.0, 4.1);
    }

expected:1

    import hardware.dsp

    // type checking for fmul32 result
    inline void main()
    {
        int32 p = fmul32(3.0, 4.1);
    }

expected:1

    import hardware.dsp

    // type checking for fmul32 param
    inline void main()
    {
        float32 p = fmul32(3, 4);
    }

expected:1

    // implicit uint32->float32 cast
    inline void main()
    {
        uint32 i = 4;

        float32 f = i;
    }

expected:1

    // implicit float32->uint32 cast
    inline void main()
    {
        float32 f = 2.0;

        uint32 i = f;
    }

expected:1

    // large mux that reads from signed data (sign extension mask is 64-bits)
    inline void Foo(uint6 x)
    {
        int32[64] ary;

        int32 i = mux(x,
                      ary[0], ary[1], ary[2], ary[3], ary[4], ary[5], ary[6], ary[7],
                      ary[0], ary[1], ary[2], ary[3], ary[4], ary[5], ary[6], ary[7],
                      ary[0], ary[1], ary[2], ary[3], ary[4], ary[5], ary[6], ary[7],
                      ary[0], ary[1], ary[2], ary[3], ary[4], ary[5], ary[6], ary[7],
                      ary[0], ary[1], ary[2], ary[3], ary[4], ary[5], ary[6], ary[7],
                      ary[0], ary[1], ary[2], ary[3], ary[4], ary[5], ary[6], ary[7],
                      ary[0], ary[1], ary[2], ary[3], ary[4], ary[5], ary[6], ary[7],
                      ary[0], ary[1], ary[2], ary[3], ary[4], ary[5], ary[6], ary[7]);
    }

expected:26

    // c comment that isn't closed at the end of a file

    /* begining of a comment

expected:1

    // comments

    /* this is a comment */

    /*

    so is this

    */

    /*

    nested comments /* */

    asdf
    */

    inline void main()
    {
    }

expected:0

    // unrolled for with induction variable that is just the right size
    inline void main()
    {
        static for (const uint2 y : 4)
        {
        }
    }

expected:0

    // unrolled for with induction variable that is too small
    inline void main()
    {
        static for (const uint2 y : 5)
        {
        }
    }

expected:25

    // unrolled for with induction variable that is too small
    inline void main()
    {
        static for (const uint3 y : 9)
        {
        }
    }

expected:25

    // unrolled for with signed induction variable
    inline void main()
    {
        static for (const int32 y : 4)
        {
        }
    }

expected:16

    // unrolled for with signed bound
    inline void main()
    {
        static for (const uint32 y : -2)
        {
        }
    }

expected:25

    // writing to induction variable in unrolled for
    inline void main()
    {
        static for (const uint32 y : 10)
        {
            y = 2;
        }
    }

expected:1


    class C
    {
    public:
        void main()
        {
            Bar();
        }
    private:
        // async indirect recursion
        [[async]] void Goo()
        {
            Bar();
        }

        [[async]] void Bar()
        {
            Goo();
        }
    }

    export C;

expected:37

    class C
    {
    public:
        void main()
        {
            Bar();
        }
    private:
        // indirect recursion
        void Goo()
        {
            Bar();
        }

        void Bar()
        {
            Goo();
        }

    }

    export C;

expected:37


    class C
    {
    public:
        void main()
        {
            Bar();
        }
    public:
        // async direct recursion
        [[async]] void Bar()
        {
            Bar();
        }
    }

    export C;

expected:37


    class C
    {
    public:
        void main()
        {
            Bar();
        }
    private:

        // direct recursion
        void Bar()
        {
            Bar();
        }
    }

    export C;

expected:37

    // fan_out value not known
    inline void main()
    {
        uint32 x = 2;

        uint32 size = 4;

        uint32[4] f = fan_out<size>(x);
    }

expected:17


    // fan_out<0>
    inline void main()
    {
        uint32 x = 2;

        uint32[4] f = fan_out<0>(x);
    }

expected:1

    // bool fan_out
    inline void main()
    {
        uint32 x = 2;

        uint32[4] f = fan_out<true>(x);
    }

expected:16

    // int fan_out
    inline void main()
    {
        uint32 x = 2;

        const int8 size = 4;

        uint32[4] f = fan_out<size>(x);
    }

expected:0

    // valid use of static
    const uint3 _idx = 2;

    inline uint32 Foo()
    {
        uint32[8] _a;

        return _a[static(_idx)];
    }

expected:0

    // invalid use of static
    class C
    {
    public:
        void main()
        {
            uint32[8] _a;
            uint3 _idx = 2;
            auto x = _a[static(_idx)];
        }
    }

    export C;

expected:36

    // memory of struct, read-modify-write
    struct Foo339
    {
        uint8 x;
        uint7 y;
    }


    class C
    {
    public:
        void main()
        {
            go();
        }

    private:
        [[max_threads(1)]] void go()
        {
            TestCase339();
        }

        void TestCase339()
        {
            static [[memory]] Foo339[512] _mem339;

            _mem339[1].x = 2;
        }

    }

    export C;

expected:9

    import data.memory

    // array of memories, dynamic index


    class C
    {
    public:
        void main()
        {
            go();
        }
    private:
        [[max_threads(1)]] void go()
        {
            Foo();
        }
        void Foo()
        {
            uint32 _x = 0;
            static memory<uint32, 512>[32] _mem;
            for (const auto x : 10)
            {
                _x = _mem[x][3];
            }
        }
    }

    export C;

expected:9

    class C
    {
    public:
        void main()
        {
            [[call_rate(4)]] Foo();
        }
    private:
        // Call rate used not under predication
        void Foo()
        {
        }
    }

    export C;

expected:35

    // Call rate specified twice
    inline void Foo()
    {
    }

    class C
    {
        void main(bool b)
        {
            if (b)
            {
                [[call_rate(4), call_rate(3)]] Foo();
            }
        }
    }

    export C;

expected:65

    // Call before intrinsic
    inline void main()
    {
        [[call_rate(4)]] print("test");
    }

expected:27

    // Call rate modifier not known
    inline void Foo()
    {
    }

    inline void main()
    {
        uint32 x = 4;

        [[call_rate(x)]] Foo();
    }

expected:1

    // Call rate modifier = 0
    inline void Foo()
    {
    }

    inline void main()
    {
        [[call_rate(0)]] Foo();
    }

expected:27

    // Call rate modifier that is a struct
    struct MyStruct
    {
        uint32 _x;
    }


    inline void Foo()
    {
    }

    inline void main()
    {
        MyStruct _struct;
        [[call_rate(_struct)]] Foo();
    }

expected:1

    // Call rate modifier that is a signed integer
    inline void Foo()
    {
    }

    inline void main()
    {
        [[call_rate(-2)]] Foo();
    }

expected:1

    // Call rate modifier before inline call
    inline void Foo()
    {
    }

    inline void main()
    {
        [[call_rate(2)]] Foo();
    }

expected:27

    // Sizeof class
    class C309
    {
        uint32 x;
        uint8 y;
    }


    inline void main()
    {
        static C309 _c309;
        assert(bitsizeof(_c309) == 40);
    }

expected:1

    // calling methods in an array of objects - dynamic index
    class Foo
    {
    public:
        inline void Bar()
        {
        }
    }

    class C
    {
        Foo[8] _objects;

    public:
        void main()
        {
            for (const auto i : 8)
            {
                _objects[i].Bar();
            }
        }
    }

    export C;

expected:9

    // Union contains class
    class Coo
    {
        uint32 x;
    }

    union Foo
    {
        Coo c;
    }

expected:24

    // Empty union
    union Foo
    {
    }

expected:0

    // Local variables declared in switch blocks
    class C
    {
    public:
        void main()
        {
            uint32 x = 3;

            switch (x)
            {
            case 3:
                uint32 y = 8;
                assert(y == 8);
            break;

            default:
                break;
            }

            assert(x == 3);
            assert(y == 8);
        }
    }

    export C;

expected:1

    // array size that is an expression
    inline void main()
    {
        uint32[3+4] ary;
    }

expected:0

    // invalid type inference of class
    class Foo
    {
    }


    inline void main()
    {
        static Foo _foo;
        auto x = _foo;
    }

expected:24

    // invalid type inference of memory
    inline void main()
    {
        static [[memory]] uint32[4] _mem;
        auto x = _mem;
    }

expected:24

    // invalid type inference
    inline void main()
    {
        auto x;
    }

expected:1

    // invalid type inference
    inline void main()
    {
        uint32 y = 3;

        auto[3] x = y;
    }

expected:1

    // * by non pow 2
    inline void main()
    {
        uint32 x = 5 * 7;
    }

expected:0

    // / by non pow 2
    inline void main()
    {
        uint32 x = 4 / 3;
    }

expected:0

    // % by non pow 2
    inline void main()
    {
        uint32 x = 4 % 3;
    }

expected:0

    // * on signed integers
    inline void main()
    {
        uint32 x = 5 * -4;
    }

expected:0

    // * on signed integers
    inline void main()
    {
        uint32 x = -4 * 5;
    }

expected:0

    // / on signed integers
    inline void main()
    {
        static assert(5 / -4 == -1);
    }

expected:0

    // % on signed integers
    inline void main()
    {
        static assert(5 % -4 == 1);
    }

expected:0

    // * on boolean
    inline void main()
    {
        uint32 x = 4 * false;
    }

expected:1

    // / on boolean
    inline void main()
    {
        uint32 x = false / 4;
    }

expected:1

    // % on boolean
    inline void main()
    {
        uint32 x = false % 4;
    }

expected:1

    // * by 0
    inline void main()
    {
        uint32 x = 5 * 0;
    }

expected:0

    // * by 0
    inline void main()
    {
        uint32 x = 0 * 5;
    }

expected:0

    // / by 0
    inline void main()
    {
        uint32 x = 4 / 0;
    }

expected:1

    // % by 0
    inline void main()
    {
        uint32 x = 4 % 0;
    }

expected:1

    // % by 1
    inline void main()
    {
        auto x = 4 % 1;
    }

expected:0

    class Main
    {
    public:
        // [[max_threads(1)]] function with a predicated call - no ordering problems
        void Bar()
        {
        }

        [[max_threads(1)]] void main()
        {
            if(false)
            {
                Bar();
            }
        }
    }

    export Main;

expected:0

    class Main
    {
    public:
        // [[max_threads(1)]] function with a switch statement  - no ordering problems
        [[max_threads(1)]] void main()
        {
            uint32 x = 2;

            switch (x)
            {
            }
        }
    }

    export Main;

expected:0

    // ordered function with a predicated call

    inline void Bar()
    {
    }

    inline void main()
    {
        bool b = false;
        if(b)
        {
            Bar();
        }
    }

expected:0


    class C
    {
    public:
        void main()
        {
            Bar();
        }
    private:
        // ordered function with a call to a non-ordered inline function
        [[unordered]] void Bar()
        {
    }
    }

    export C;

expected:12


    class C2
    {
        // ordered function with a call to a non-ordered function
        [[unordered]] void Bar()
        {
        }
    public:
        void main()
        {
            Bar();
        }
    }

    export C2;

expected:12

    // ordered function with a loop
    class C3
    {
    public:
        void main()
        {
            [[unordered]] for (const auto i : 4)
            {
            }
        }
    }

    export C3;

expected:12

    class C
    {
        // sync call inside of a loop
        inline void Bar()
        {
        }

        [[unordered]] void main()
        {
            for (const auto i : 4)
            {
                Bar();
            }
        }
    }

    export C;

expected:0

    class C
    {
        // async call inside of a loop, via an inline function
        [[async]] void Bar()
        {
        }

        inline void Gak()
        {
            Bar();
        }

        void main()
        {
            for (const auto i : 4)
            {
                Gak();
            }
        }
    }

    export C;

expected:0

    class C
    {
        // async call inside of a loop
        [[async]] void Bar()
        {
        }

    public:
        void main()
        {
            for (const auto i : 4)
            {
                Bar();
            }
        }
    }

    export C;

expected:0

    class C
    {
        // [[max_threads(non pow2)]]
        [[max_threads(3)]] void main()
        {
        }
    }

    export C;

expected:18

    class C
    {
        // [[max_threads(0)]]
        [[max_threads(0)]] void main()
        {
        }
    }

    export C;

expected:18

    class C
    {
        // max_threads specified twice
        [[max_threads(1), max_threads(1)]] void main()
        {
        }
    }

    export C;

expected:18

    class C
    {
        // switch with 2 defaults
        void main()
        {
            uint32 x = 2;

            uint32 result;

            switch (x)
            {
            case 0:
                result = 7;
                break;

            case 1:
                result = 8;
                break;

            default:
                result = 1;
                break;

            default:
                result = 2;
                break;
            }
        }
    }

    export C;

expected:13

    class C
    {
        // switch with signed integer type
        void main()
        {
            int32 x = 2;

            uint32 result;

            switch (x)
            {
            case 0:
                result = 7;
                break;

            case 1:
                result = 8;
                break;
            }
        }
    }

    export C;

expected:13

    class C
    {
        // switch with non integer type
        void main()
        {
            bool b = false;

            uint32 result;

            switch (b)
            {
            case 0:
                result = 7;
                break;

            case 1:
                result = 8;
                break;
            }
        }
    }

    export C;

expected:13

    // switch with duplicate cases
    inline void main()
    {
        uint32 x = 2;

        uint32 result;

        switch (x)
        {
        case 0:
            result = 7;
            break;

        case 0:
            result = 8;
            break;
        }
    }

expected:13

    // Valid switch
    inline void main()
    {
        uint32 x = 2;

        uint32 result;

        switch (x)
        {
        case 0:
            result = 7;
            break;

        case 1:
            result = 8;
            break;
        }
    }

expected:0

    // Switch with case Y where Y is not known at compile time
    inline void main()
    {
        uint32 x = 2;

        uint32 result;

        switch (x)
        {
        case x:
            result = 7;
            break;

        case 1:
            result = 8;
            break;
        }
    }

expected:13

    // Grouping to case statements together (not allowed)
    class C
    {
    public:
        void main()
        {
            uint32 x = 2;

            uint32 result;

            switch (x)
            {
            case 2:
            case 0:
                result = 7;
                break;

            case 1:
                result = 8;
                break;
            }
        }
    }

    export C;

expected:1

    // Falling through (not allowed)
    class C
    {
    public:
        void main()
        {
            uint32 x = 2;

            uint32 result;

            switch (x)
            {
            case 0:
                result = 7;

            case 1:
                result = 8;
                break;
            }
        }
    }

    export C;

expected:1

    // Binary operation that is not used
    inline void main()
    {
        uint32 x = 1;
        uint32 y = 3;

        x + y;
    }

expected:0

    // calling non-existing function
    inline void main()
    {
        Foo();
    }

expected:1

    // bitwise AND on boolean type
    inline void TestMain()
    {
        bool a = true;
        bool b = false;

        bool c = a & b;
    }

expected:1

    // bitwise OR on boolean type
    inline void TestMain()
    {
        bool a = true;
        bool b = false;

        bool c = a | b;
    }

expected:1

    // bitwise XOR on boolean type
    inline void TestMain()
    {
        bool a = true;
        bool b = false;

        bool c = a ^ b;
    }

expected:1

    // bitwise invert on boolean type
    inline void TestMain()
    {
        bool a = true;
        bool b = ~a;
    }

expected:1

    // logical AND on integer type
    inline void TestMain()
    {
        uint1 a = 1;
        uint1 b = 0;

        uint1 c = a && b;
    }

expected:1

    // logical OR on integer type
    inline void TestMain()
    {
        uint1 a = 1;
        uint1 b = 0;

        uint1 c = a || b;
    }

expected:1

    // logical XOR on integer type
    inline void TestMain()
    {
        uint1 a = 1;
        uint1 b = 0;

        uint1 c = a ^^ b;
    }

expected:1

    // logical not on integer type
    inline void TestMain()
    {
        uint1 x = 1;

        uint1 y = !x;
    }

expected:1

    // Memory of classes
    class Foo
    {
    }

    inline void main()
    {
        static [[memory]] Foo[32] _mem;
    }

expected:24


    // Initialize mem
    inline void main()
    {
        static [[memory]] uint32[32] _mem = 2;
    }

expected:1

    // Assign memory to array

    inline void main()
    {
        static [[memory]] uint32[32] _mem;
        uint32[32] a;

        a = _mem;
    }

expected:77

    // Assign array to memory

    inline void main()
    {
        static [[memory]] uint32[32] _mem;
        uint32[32] a;

        _mem = a;
    }

expected:14

    // Memory in struct
    struct Foo
    {
        [[memory]] uint32[32] mem;
    }

expected:24

    // Local memory
    inline void main()
    {
        [[memory]] uint32[32] mem;
    }

expected:24

    // Power of 2 memory
    inline void main()
    {
        static [[memory]] uint32[32] _mem;
    }

expected:0

    // Non-Power of 2 memory
    inline void main()
    {
        static [[memory]] uint32[31] _mem;
    }

expected:0

    // Memory with 0 elements
    inline void main()
    {
        static [[memory]] uint32[0] _mem;
    }

expected:1

    // class with semi-colon
    class Foo
    {
    };

expected:0

    // Struct with semi-colon
    struct Foo
    {
        uint32 x;
    };

expected:0

    // async inline function
    [[async]] inline void main()
    {
    }

expected:18

    class C
    {
    public:
        // inline export function
        inline void main()
        {
        }
    }

    export C;

expected:1

    // Class local variable
    class Foo
    {
    }

    inline void TestMain()
    {
        Foo f;
    }

expected:24

    // Calling a method on a public member

    class Bar169
    {
    public:
        uint8 Get()
        {
            return 3;
        }
    }

    class Foo169
    {
    public:
        Bar169 _bar;
    }


    inline void main()
    {
        static Foo169 _foo169;
        assert(3 == _foo169._bar.Get());
    }

expected:0

    // struct containing a class
    class Foo
    {
    }

    // It is important that the struct contain 1 integer
    // to avoid tripping over the "empty struct" error
    struct Bar
    {
        uint32 x;
        Foo f;
    }

expected:24

    // array of objects
    class Foo169
    {
        uint32 _x;

    public:
        inline void Set(uint32 x)
        {
            _x = x;
        }

        uint32 Get()
        {
            return _x;
        }
    }

    inline void main()
    {
        static Foo169[4] _foo169;

        _foo169[0].Set(4);
    }

expected:0

    // 2 objects with the same name

    class Foo
    {
    }

    inline void main()
    {
        static Foo f;
        static Foo f;
    }

expected:1


    // Calling a method on a non-class

    inline void TestMain()
    {
        uint32 x = 4;
        x.Foo();
    }

expected:1

    // Calling a method that does not exist
    class Foo
    {
    }


    inline void TestMain()
    {
        static Foo f;
        f.Bar();
    }

expected:1

    // assigning to a class
    class Foo
    {
    }

    class Test
    {
        Foo f;
        const Foo g = f;
    }

    export Test;

expected:7

    // 2 classes with the same name
    class Foo
    {
    public:
        uint32 _y = 21;
    }

    class Foo
    {
    public:
        uint32 _x;
    }

expected:1

    // Public member variable
    class Foo163
    {
    public:
        uint32 _y = 21;
    }

    inline void main()
    {
        static Foo163 _foo163;

        assert(21 == _foo163._y);
    }

expected:0

    // Private member variable
    class Foo163
    {
    private:
        uint32 _y = 21;
    }

    inline void main()
    {
        static Foo163 _foo163;

        assert(21 == _foo163._y);
    }

expected:29

    // default private member variable
    class Foo163
    {
        uint32 _y = 21;
    }

    inline void main()
    {
        static Foo163 _foo163;

        assert(21 == _foo163._y);
    }

expected:29

    // Valid method call
    class Foo
    {
    public:
        uint32 Get()
        {
            return 8;
        }
    }

    inline void main()
    {
        static Foo _foo;

        assert(8 == _foo.Get());
    }

expected:0

    // Calling a private method
    class Foo
    {
    private:
        uint32 Get()
        {
            return 8;
        }
    }

    inline void main()
    {
        static Foo _foo;

        assert(8 == _foo.Get());
    }

expected:29

    import hardware.dsp

    // Valid umul27
    inline void main()
    {
        uint54 x = umul27(3, 4);
    }

expected:0

    // valid mux operator
    inline void main()
    {
        uint3 x = 0;
        uint3 y = 1;
        uint3 z = 2;
        uint3 w = 4;

        uint2 pred = 1;

        uint3 result = mux(pred, x, y, z, w);
    }

expected:0

    // invalid expression count
    inline void main()
    {
        uint3 x = 0;
        uint3 y = 1;
        uint3 z = 2;
        uint3 w = 4;

        uint2 pred = 1;

        uint3 result = mux(pred, x, y, z);
    }

expected:26

    // invalid index type
    inline void main()
    {
        uint3 x = 0;
        uint3 y = 1;
        uint3 z = 2;
        uint3 w = 4;

        int2 pred = 1;

        uint3 result = mux(pred, x, y, z, w);
    }

expected:26

    // valid ternary operator
    inline void main()
    {
        uint3 x = 0;
        uint3 y = 1;
        bool pred = false;

        uint3 z = pred ? x : y;
    }

expected:0

    // type conversion in ternary operator
    inline void main()
    {
        uint3 x = 0;
        int3 y = 1;
        bool pred = false;

        int4 z = pred ? x : y;
    }

expected:0

    // ternary operator with struct
    struct Foo
    {
        uint3 a;
        uint4 b;
    }
    inline void main()
    {
        Foo x;
        Foo y;
        bool pred = true;

        Foo z = pred ? x : y;
    }

expected:0

    // ternary operator with mismatching struct
    struct Foo
    {
        uint3 a;
        uint4 b;
    }

    struct Bar
    {
        uint3 a;
        uint4 b;
    }

    inline void main()
    {
        Foo x;
        Bar y;
        bool pred = true;

        Foo z = pred ? x : y;
    }

expected:1

    // if condition must be a bool
    class C
    {
    public:
        void main()
        {
            uint2 x = 0;

            if (x)
            {
            }
        }
    }

    export C;

expected:16

    // if condition must be a bool
    class C2
    {
    public:
        void main()
        {
            int1 x = 0;

            if (x)
            {
            }
        }
    }

expected:16

    // negative literal
    inline void main()
    {
        int4 x = -2;
    }

expected:0

    // Nested curly braces
    class C
    {
    public:
        int8 main()
        {
            for (const auto x : 5)
            {
                {
                    uint32 y = x;
                }
            }

            return 0;
        }
    }

    export C;

expected:0

    // Invalid variable name
    inline void main()
    {
        32int32 x;
    }

expected:1

    // Function parameters
    inline int32 main(int32 a)
    {
       return a;
    }

expected:0

    // Function parameters
    inline int32 main(int32 a, int32 b)
    {
       return b;
    }

expected:0

    // Operators
    inline int32 main(int31 a, int31 b)
    {
       return a + b;
    }

expected:0

    inline int32 main(int31 a, int31 b)
    {
       return a - b;
    }

expected:0

    inline int32 main(int32 a, int32 b)
    {
       return a & b;
    }

expected:0

    inline int32 main(int32 a, int32 b)
    {
       return a | b;
    }

expected:0

    inline int32 main(int32 a, int32 b)
    {
       return a ^ b;
    }

expected:0

    inline int32 main(int16 a, uint4 b)
    {
       return a << b;
    }

expected:0

    inline int32 main(int32 a, uint4 b)
    {
       return a >> b;
    }

expected:0

    inline int32 main(int32 a)
    {
       return -a;
    }

expected:0

    inline int32 main(int32 a)
    {
       return ~a;
    }

expected:0

    inline bool main(int32 a, int32 b)
    {
       return a == b;
    }

expected:0

    inline bool main(int32 a, int32 b)
    {
       return a != b;
    }

expected:0

    inline bool main(int32 a, int32 b)
    {
       return a > b;
    }

expected:0

    inline bool main(int32 a, int32 b)
    {
       return a < b;
    }

expected:0

    inline bool main(int32 a, int32 b)
    {
       return a <= b;
    }

expected:0

    inline uint32 main(uint32 a)
    {
       return bitsizeof(a);
    }

expected:0

    inline uint32 main(uint32 a)
    {
       return bytesizeof(a);
    }

expected:0

    inline uint32 main(int32 a)
    {
       return cast<uint32>(a);
    }

expected:0

    // Declare array
    inline void main()
    {
       uint32[7] x;
    }

expected:0

    // Read array
    inline void main()
    {
       uint32[7] x;

       uint32 y = x[2];
    }

expected:0

    // Write array
    inline void main()
    {
       uint32[7] x;

       uint32 y;

       x[2] = y;
    }

expected:0

    // If
    class C
    {
    public:
        void main(uint32 x)
        {
            uint32 result;

            if (x > 0)
            {
                result = 2;
            }
        }
    }

    export C;

expected:0

    // If/Else
    class C
    {
    public:
        void main(uint32 x)
        {
            uint32 result;

            if (x > 0)
            {
                result = 2;
            }
            else
            {
                result = 4;
            }
        }
    }

    export C;

expected:0

    // Flat If
    inline void main(uint32 x)
    {
        uint32 result;

        if (x > 0)
        {
            result = 2;
        }
    }

expected:0

    // Flat If/Else
    inline void main(uint32 x)
    {
        uint32 result;

        if (x > 0)
        {
            result = 2;
        }
        else
        {
            result = 4;
        }
    }

expected:0

    // Negate operator
    inline int32 main(int32 x)
    {
        return -x;
    }

expected:0

    // Invert operator
    inline uint32 main(uint32 x)
    {
        return ~x;
    }

expected:0

    // bytesizeof operator
    inline uint32 main(uint32 x)
    {
        return bytesizeof(x);
    }

expected:0

    // bitsizeof operator
    inline uint32 main(uint32 x)
    {
        return bitsizeof(x);
    }

expected:0

    // 2 variables with the same name in the same scope
    inline void main()
    {
        uint32 x;
        uint32 x;
    }

expected:1

    // 2 variables with the same name in different scope
    inline void main()
    {
        uint32 x;

        {
            uint32 x;
        }
    }

expected:0

    // Binary op
    inline void main()
    {
        uint32 x;
        uint32 y;
        uint32 z = x + y;
    }

expected:0

    // Binary op applied to vectors (does not work)
    inline void main()
    {
        uint32[2] x;
        uint32[2] y;
        uint32[2] z = x + y;
    }

expected:1

    // Unsigned shift amount
    inline void main()
    {
        int32 x;
        uint2 y;
        int35 z = x << y;
    }

expected:0

    // Signed shift amount
    inline void main()
    {
        int32 x;
        int2 y;
        int35 z = x << y;
    }

expected:0

    // comparison with same signedness
    inline void main()
    {
        uint32 x;
        uint32 y;
        bool b = x < y;
    }

expected:0

    // comparison with different signedness
    inline void main()
    {
        uint32 x;
        int32 y;
        bool b = x < y;
    }

expected:0

    // comparison with same signedness
    inline void main()
    {
        uint32 x;
        uint32 y;
        bool b = x <= y;
    }

expected:0

    // comparison with different signedness
    inline void main()
    {
        uint32 x;
        int32 y;
        bool b = x <= y;
    }

expected:0

    // comparison with same signedness
    inline void main()
    {
        uint32 x;
        uint32 y;
        bool b = x == y;
    }

expected:0

    // comparison with different signedness
    inline void main()
    {
        uint32 x;
        int32 y;
        bool b = x == y;
    }

expected:0

    // comparison with same signedness
    inline void main()
    {
        uint32 x;
        uint32 y;
        bool b = x != y;
    }

expected:0

    // comparison with different signedness
    inline void main()
    {
        uint32 x;
        int32 y;
        bool b = x != y;
    }

expected:0

    // comparison with same signedness
    inline void main()
    {
        uint32 x;
        uint32 y;
        bool b = x >= y;
    }

expected:0

    // comparison with different signedness
    inline void main()
    {
        uint32 x;
        int32 y;
        bool b = x >= y;
    }

expected:0

    // comparison with same signedness
    inline void main()
    {
        uint32 x;
        uint32 y;
        bool b = x > y;
    }

expected:0

    // comparison with different signedness
    inline void main()
    {
        uint32 x;
        int32 y;
        bool b = x > y;
    }

expected:0

    // Binary op with same signedness
    inline void main()
    {
        uint32 x;
        uint32 y;
        uint32 z = x + y;
    }

expected:0

    // Binary op with different signedness
    inline void main()
    {
        uint32 x;
        int32 y;
        uint32 z = x + y;
    }

expected:0

    // Binary op with same signedness
    inline void main()
    {
        uint32 x;
        uint32 y;
        uint32 z = x - y;
    }

expected:0

    // Binary op with different signedness
    inline void main()
    {
        uint32 x;
        int32 y;
        uint32 z = x - y;
    }

expected:0

    // Binary op with same signedness
    inline void main()
    {
        uint32 x;
        uint32 y;
        uint32 z = x & y;
    }

expected:0

    // Binary op with different signedness
    inline void main()
    {
        uint32 x;
        int32 y;
        uint32 z = x & y;
    }

expected:0

    // Binary op with same signedness
    inline void main()
    {
        uint32 x;
        uint32 y;
        uint32 z = x | y;
    }

expected:0

    // Binary op with different signedness
    inline void main()
    {
        uint32 x;
        int32 y;
        uint32 z = x | y;
    }

expected:0

    // Binary op with same signedness
    inline void main()
    {
        uint32 x;
        uint32 y;
        uint32 z = x ^ y;
    }

expected:0

    // Binary op with different signedness
    inline void main()
    {
        uint32 x;
        int32 y;
        uint32 z = x ^ y;
    }

expected:0

    // New scope associated with for loops
    class C
    {
    public:
        void main()
        {
            uint32 x;

            for (const auto x : 10)
            {
            }
        }
    }

    export C;

expected:0

    // New scope associated with for loops
    inline void main()
    {
        uint32 x;

        static for (const auto x : 10)
        {
        }
    }

expected:0

    // New scope for functions
    inline void main()
    {
        uint32 x;
    }

    inline void main2()
    {
        uint32 x;
    }

expected:0

    // Assignment type match
    inline void main()
    {
        uint32 x;
        uint32 y;

        x = y;
    }

expected:0

    // Assignment type mismatch (auto conversion)
    inline void main()
    {
        uint32 x;
        int32 y;

        x = y;
    }

expected:0

    // Array size match
    inline void main()
    {
        uint32[2] x;
        uint32[2] y;

        x = y;
    }

expected:0

    // Array size mis-match
    inline void main()
    {
        uint32[2] x;
        uint32[3] y;

        x = y;
    }

expected:14

    // Bit size match
    inline void main()
    {
        uint32[2] x;
        uint32[2] y;

        x = y;
    }

expected:0

    // Bit size mis-match
    inline void main()
    {
        uint32[2] x;
        uint31[2] y;

        x = y;
    }

expected:14

    // Correct array index type
    inline void main()
    {
        uint32[2] x;
        const uint32 y = 1;
        uint32 z;

        z = x[y];
    }

expected:0

    // Incorrect array index value
    class C
    {
    public:
        void main()
        {
            uint32[2] x;
            const int32 y = 2;
            uint32 z;

            z = x[y];
        }
    }

    export C;

expected:9

    // Indexing into a scalar
    inline void main()
    {
        uint32 x;
        uint32 y;
        uint32 z;

        z = x[y];
    }

expected:1

    // Correct array write
    inline void main()
    {
        uint32 x;
        const uint32 y = 1;
        uint32[2] z;

        z[y] = x;
    }

expected:0

    // Signed index
    inline void main()
    {
        uint32 x;
        const int32 y = 1;
        uint32[2] z;

        z[y] = x;
    }

expected:0

    // Indexing into a scalar
    inline void main()
    {
        uint32 x;
        uint32 y;
        uint32 z;

        z[y] = x;
    }

expected:1

    // Vector source
    inline void main()
    {
        uint32[1] x;
        uint32 y;
        uint32[2] z;

        z[y] = x;
    }

expected:14

    // void variable
    inline void main()
    {
        void x;
    }

expected:24

    // void variable
    inline void main()
    {
        void[2] x;
    }

expected:1

    // 0 bit count
    inline void main()
    {
        uint0 x;
    }

expected:16

    // 0 array size
    inline void main()
    {
        uint2[0] x;
    }

expected:1

    // 2D array
    inline void main()
    {
        uint2[2][2] x;
    }

expected:0

    // negative array size
    inline void main()
    {
        uint2[-2] x;
    }

expected:1

    // bool array array size
    inline void main()
    {
        bool[2] x;
    }

expected:0

    // Signedness cast
    inline void main()
    {
        uint32 x;
        int32 y;

        y = cast<int32>(x);
    }

expected:0

    // Size cast
    inline void main()
    {
        uint32 x;
        uint34 y;

        y = cast<uint34>(x);
    }

expected:0

    // Void cast
    inline void main()
    {
        uint32 x;
        cast<void>(x);
    }

expected:14

    // Array cast
    inline void main()
    {
        uint4[8] x;
        uint32 y;

        y = cast<uint32>(x);
    }

expected:0

    // Array cast
    inline void main()
    {
        uint32 x;
        uint4[8] y;

        y = cast<uint4[8]>(x);
    }

expected:0

    // Array cast (change size)
    inline void main()
    {
        uint4[9] x;
        uint32 y;

        y = cast<uint32>(x);
    }

expected:14

    // Array cast (change size)
    inline void main()
    {
        uint32 x;
        uint4[9] y;

        y = cast<uint4[9]>(x);
    }

expected:14

    // Array of bools
    inline void main()
    {
        bool[4] x;
    }

expected:0

    // Legal if
    class C
    {
    public:
        void main()
        {
            bool x;

            if (x)
            {
            }
        }
    }

    export C;

expected:0

    // int1 condition for if
    class C2
    {
    public:
        void main()
        {
            int1 x;

            if (x)
            {
            }
        }
    }

    export C2;

expected:16

    // uint2 condition for if
    class C3
    {
    public:
        void main()
        {
            uint2 x;

            if (x)
            {
            }
        }
    }

    export C3;

expected:16

    // Array condition for if
    class C4
    {
    public:
        void main()
        {
            bool[1] x;

            if (x)
            {
            }
        }
    }

    export C4;

expected:16

    // Legal invert
    inline void main()
    {
        uint32 x;
        uint32 y = ~x;
    }

expected:0

    // Invert array
    inline void main()
    {
        uint32[1] x;
        uint32[1] y = ~x;
    }

expected:1

    // Legal negate
    inline void main()
    {
        int32 x;
        int32 y = -x;
    }

expected:0

    // Invert array
    inline void main()
    {
        int32[1] x;
        int32[1] y = -x;
    }

expected:1

    // Legal bytesizeof
    inline void main()
    {
        uint8 x;
        uint1 y = bytesizeof(x);
    }

expected:0

    // Illegal bytesizeof (not a multiple of 8)
    inline void main()
    {
        uint9 x;
        uint1 y = bytesizeof(x);
    }

expected:1

    // Legal void return
    inline void main()
    {
        return;
    }

expected:0

    // Illegal void return
    inline uint32 main()
    {
        return;
    }

expected:21

    // Legal value return
    inline uint32 main()
    {
        uint32 x;
        return x;
    }

expected:0

    // Illegal value return
    inline void main()
    {
        uint32 x;
        return x;
    }

expected:1

    // Illegal conversion during return
    inline uint32 main()
    {
        uint4[8] x;
        return x;
    }

expected:1

    // Illegal conversion during return
    inline uint32 main()
    {
        uint4[9] x;
        return x;
    }

expected:1

    // Illegal conversion in assignment
    inline void main()
    {
        uint32 x;
        uint4[8] y;
        x = y;
    }

expected:14

    // Illegal conversion in assignment
    inline void main()
    {
        uint32 x;
        uint4[9] y;
        x = y;
    }

expected:14

    // Illegal conversion in assignment
    inline void main()
    {
        uint32[2] x;
        uint4[8] y;
        x[1] = y;
    }

expected:14

    // Illegal conversion in assignment
    inline void main()
    {
        uint32[2] x;
        uint4[9] y;
        x[1] = y;
    }

expected:14

    // legal conversion in assignment
    inline void main()
    {
        uint32[2] x;
        uint4[8] y;
        x[1] = y[2];
    }

expected:0

    // Function call
    inline uint32 foo()
    {
        return 3;
    }

    inline uint32 main()
    {
        return foo();
    }

expected:0

    // Parameter count match
    inline uint32 foo(uint32 x)
    {
        return x;
    }

    inline uint32 main()
    {
        return foo(3);
    }

expected:0

    // Parameter count mismatch
    inline uint32 foo()
    {
        return 3;
    }

    inline uint32 main()
    {
        return foo(3);
    }

expected:1

    // Parameter count mismatch
    inline uint32 foo(uint32 x)
    {
        return x;
    }

    inline uint32 main()
    {
        return foo();
    }

expected:1

    // Illegal Argument conversion
    inline uint32 foo(uint32 x, uint4[8] y)
    {
        return x;
    }

    inline uint32 main()
    {
        int8 x;
        uint32 y;
        return foo(x, y);
    }

expected:1

    // Invalid argument conversion
    inline uint32 foo(uint32 x, uint4[8] y)
    {
        return x;
    }

    inline uint32 main()
    {
        int8 x;
        uint4[9] y;
        return foo(x, y);
    }

expected:1

    // Illegal return type conversion
    inline uint4[8] foo()
    {
        uint4[8] x;
        return x;
    }

    inline uint32 main()
    {
        uint32 y = foo();
    }

expected:1

    // Illegal return type conversion
    inline uint4[8] foo()
    {
        uint4[8] x;
        return x;
    }

    inline uint32 main()
    {
        int32 y = foo();
    }

expected:1

    // Invalid return type conversion
    inline uint4[8] foo()
    {
        uint4[9] x;
        return x;
    }

    inline uint32 main()
    {
        foo();
    }

expected:1

    // Call undeclared function
    inline uint32 main()
    {
        return foo();
    }

expected:1

    // Legal call
    inline uint32 foo()
    {
        return 7;
    }

    inline uint32 main()
    {
        return foo();
    }

expected:0

    // Duplicate function name
    inline uint32 foo()
    {
        return 7;
    }

    inline uint32 foo()
    {
        return 7;
    }

    inline uint32 main()
    {
        return foo();
    }

expected:1

    // Legal call of a void function
    inline void foo()
    {
    }

    inline void main()
    {
        foo();
    }

expected:0

    // Conversion from void
    inline void foo()
    {
    }

    inline void main()
    {
        uint32 x = foo();
    }

expected:1

    // Declare void variable
    inline void main()
    {
        void x;
    }

expected:24

    // Binary op on void
    inline void foo()
    {
    }

    inline void main()
    {
        uint32 x = foo() + foo();
    }

expected:1

    // Unary op on void
    inline void foo()
    {
    }

    inline void main()
    {
        uint32 x = -foo();
    }

expected:1

    // Cast from void
    inline void foo()
    {
    }

    inline void main()
    {
        uint32 x = cast<uint32>(foo());
    }

expected:14

    // Sizeof void
    inline void foo()
    {
    }

    inline void main()
    {
        uint32 x = bitsizeof(foo());
    }

expected:16

    // nested functions
    inline void main()
    {
        inline void globalFunc2()
        {
        }
    }

expected:0

    class Main
    {
    public:
        // Async function
        [[async]] void foo()
        {
        }

        void main()
        {
            foo();
        }
    }

    export Main;

expected:0

    class Main
    {
    public:
        // Double async function
        [[async, async]] void foo()
        {
        }

        void main()
        {
            foo();
        }
    }

    export Main;

expected:18

    // Inline function
    inline void foo()
    {
    }

    inline void main()
    {
        foo();
    }

expected:0

    // Double inline function
    inline inline void foo()
    {
    }

    inline void main()
    {
        foo();
    }

expected:1

    // Inline & async function
    [[async]] inline void foo()
    {
    }

    inline void main()
    {
        foo();
    }

expected:18

    // Async local
    inline void main()
    {
        [[async]] uint32 foo;
    }

expected:1

    // Inline local
    inline void main()
    {
        inline uint32 foo;
    }

expected:1

    // Async local
    inline void main()
    {
        [[async]] uint32 foo = 0;
    }

expected:1

    // Inline local
    inline void main()
    {
        inline uint32 foo = 0;
    }

expected:1

    // Async local
    inline void main()
    {
        [[async]] uint32[1] foo;
    }

expected:1

    // Inline local
    inline void main()
    {
        inline uint32[1] foo;
    }

expected:1

    class Main
    {
    public:
        // Async function returning a value
        [[async]] uint32 foo()
        {
            return 0;
        }

        void main()
        {
            foo();
        }
    }

    export Main;

expected:20

    // Function returning a structure
    struct Foo
    {
        int32 x;
        uint2 y;
    }

    inline Foo main()
    {
        Foo foo;

        return foo;
    }

expected:0

    // Struct with no members
    struct Foo
    {
    }

    inline Foo main()
    {
        Foo foo;
        return foo;
    }

expected:0

    // 2 structs with the same name
    struct Foo
    {
        int32 x;
        uint2 y;
    }

    struct Foo
    {
        int32 x;
        uint2 y;
    }

    inline Foo main()
    {
        Foo foo;

        return foo;
    }

expected:1

    // Nested struct
    struct Boo
    {
        int32 x;
    }

    struct Foo
    {
        Boo x;
        uint2 y;
    }

    inline Foo main()
    {
        Foo foo;

        return foo;
    }

expected:0

    // Read from structure
    struct Foo
    {
        int32 x;
        uint2 y;
    }

    inline uint2 main()
    {
        Foo foo;

        return foo.y;
    }

expected:0

    // Write to structure
    struct Foo
    {
        int32 x;
        uint2 y;
    }

    inline void main()
    {
        Foo foo;

        foo.y = 4;
    }

expected:0

    // Structure with duplicate member names
    struct Foo
    {
        int32 x;
        int32 x;
    }

    inline Foo main()
    {
        Foo foo;

        return foo;
    }

expected:33

    // Cast from structure (valid size)
    struct Foo
    {
        int32 x;
        uint2 y;
    }

    inline void main()
    {
        Foo foo;

        uint34 boo = cast<uint34>(foo);
    }

expected:0

    // Cast from structure (invalid size)
    struct Foo
    {
        int32 x;
        uint2 y;
    }

    inline void main()
    {
        Foo foo;

        uint35 boo = cast<uint35>(foo);
    }

expected:14

    // Cast to structure (valid size)
    struct Foo
    {
        int32 x;
        uint2 y;
    }

    inline void main()
    {
        uint34 boo = 0;
        Foo foo;
        foo = cast<Foo>(boo);
    }

expected:0

    // Cast to structure (invalid size)
    struct Foo
    {
        int32 x;
        uint2 y;
    }

    inline void main()
    {
        uint33 boo = 0;
        Foo foo;
        foo = cast<Foo>(boo);
    }

expected:14

    // Async structure member
    struct Foo
    {
        [[async]] int32 x;
    }

    inline void main()
    {
        Foo foo;
    }

expected:1

    // Inline structure member
    struct Foo
    {
        inline int32 x;
    }

    inline void main()
    {
        Foo foo;
    }

expected:1

    // Array of structure
    struct Foo
    {
        int32 x;
        uint2 y;
    }

    inline uint2 main()
    {
        Foo[5] foo;

        return foo[2].y;
    }

expected:0

    // Structure of arrays
    struct Foo
    {
        int32[5] x;
        uint2[6] y;
    }

    inline uint2 main()
    {
        Foo foo;

        return foo.y[1];
    }

expected:0

    // array of arrays
    inline uint2 main()
    {
        uint2[4][2] foo;

        return foo[0][1];
    }

expected:0

    // Casting of structs with same bit width is allowed
    struct Foo
    {
        uint32 x;
    }

    struct Boo
    {
        uint32 x;
    }

    inline void main()
    {
        Foo foo;
        Boo boo;

        foo = cast<Foo>(boo);
    }

expected:0

    // Automatic casting of structs with same bit width is disallowed
    struct Foo
    {
        uint32 x;
    }

    struct Boo
    {
        uint32 x;
    }

    inline void main()
    {
        Foo foo;
        Boo boo;

        foo = boo;
    }

expected:14

    // Using a struct as an array index
    struct Foo
    {
        uint32 x;
    }

    class C
    {
    public:
        void main()
        {
            uint32[4] a;
            Foo foo;

            auto x = a[foo];
        }
    }

    export C;

expected:9

    // Function that returns a struct
    struct Foo
    {
        uint32 x;
    }

    inline Foo Helper()
    {
        Foo foo;

        return foo;
    }

    inline void main()
    {
        Helper();
    }

expected:0

    // Function that returns a struct
    struct Foo
    {
        uint32 x;
    }

    inline Foo Helper()
    {
        Foo foo;

        return foo;
    }

    inline void main()
    {
        Helper();
    }

expected:0

    // Function that returns a struct
    struct Foo
    {
        uint32 x;
    }

    class Main
    {
    public:
        [[async]] Foo Helper()
        {
            Foo foo;

            return foo;
        }

        void main()
        {
            Helper();
        }
    }

    export Main;

expected:20

    // Constant global variable
    const int2 g = 2;

    inline int2 main()
    {
        return g;
    }

expected:0

    // Constant global that is not initialized
    const int2 g;

    inline int2 main()
    {
        return g;
    }

expected:54

    // Constant local variable
    inline int2 main()
    {
        const int2 g = 2;
        return g;
    }

expected:0

    // Constant local variable that is not initialized
    inline int2 main()
    {
        const int2 g;
        return g;
    }

expected:54

    // Inline function can't modify variables from enclosing scope
    inline uint32 Foo()
    {
        myvar = 1;
        return 3;
    }

    inline void main()
    {
        uint32 myvar = 10;

        uint32 result = Foo();
    }

expected:1

    // true/false
    inline void main()
    {
        bool b = false;
        b = true;
    }

expected:0

    // global initialized with a function call
    inline uint32 Foo()
    {
        return 1;
    }

    const uint32 g = Foo();

expected:27

    // Legal return
    inline void main()
    {
        uint32 x = 0;

        return;
    }

expected:0

    // Illegal return
    class C
    {
    public:
        void main()
        {
            return;

            uint32 x = 0;
        }
    }

    export C;

expected:21

    // Illegal return
    class C2
    {
    public:
        void main()
        {
            return;

            uint32 x = 0;

            return;
        }
    }

    export C2;

expected:21

    // Missing return statement
    inline uint32 main()
    {
    }

expected:21

    // Struct & function with the same name
    struct main
    {
        uint32 x;
    }

    inline void main()
    {
    }

expected:1

    // Struct & function with the same name
    inline void main()
    {
    }

    struct main
    {
        uint32 x;
    }

expected:1

    // valid identifier
    inline void _1()
    {
    }

expected:0

    // valid identifier
    inline void _1foo0()
    {
    }

expected:0

    // invalid identifier
    inline void 1()
    {
    }

expected:1

    // invalid identifier
    inline void 1foo()
    {
    }

expected:1

    // Out of bounds array index
    class C
    {
    public:
        void main()
        {
            uint32[4] ary;

            ary[5] = 1;
        }
    }

    export C;

expected:9

    inline void main()
    {
        uint32 x;

        for (const auto i : x)
        {
        }
    }

expected:0

    import hardware.dsp

    class C
    {
    public:
        void main()
        {
            uint32 x;
            uint4 y;

            for (const auto i : x * y)
            {
            }
        }
    }

    export C;

expected:0

    inline void main(uint32 x)
    {
        for (const auto i : x)
        {
        }
    }

expected:0

    template <auto N>
    inline void Do(uint<N> x)
    {
        for (const auto i : x)
        {
        }
    }

    inline void main()
    {
        Do(10);
    }

expected:0

    inline void main()
    {
        for (const auto i : 10)
        {
        }
    }

expected:0

    const uint32 x = 19;

    inline void main()
    {
        for (const auto i : x)
        {
        }
    }

expected:0

struct Foo
{
    uint32 x = 0;
}

expected:1

    static assert (true == true != true);

expected:1

    static assert ((true == true) != true);

expected:1

    static assert (true == (true != true));

expected:1

    class Foo
    {
    public:
      inline void Bar()
      {
      }
    }

    Foo foo;

    inline void main()
    {
      foo.Foo::Bar();
    }

expected:1

    inline void main()
    {
        println("foobar);
    }

expected:1

    inline void main()
    {
        println("foo
               bar");
    }

expected:0

    inline void main()
    {
        const string str = "foobar;
    }

expected:1

    /*

    inline void main()
    {
        const /*string*/ str = "foobar;
    }

    /*
    int32 x;

    /*
    bool b;
    */

    */

    */

expected:0

    /*

       /*

    */

expected:1

    /*

       */

    */

expected:1

    import base.system

    inline void main()
    {
        stages<0>();
    }

expected:0

    import base.system

    inline void main()
    {
        stages<1000>();
    }

expected:0
