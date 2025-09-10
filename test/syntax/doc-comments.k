// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

    /*|
    pre class template Foo
    */
    template
        < /*| pre param N */ auto N        //< post param N
        , /*| pre param M */ uint32 M      //< post param M
        , /*| pre param T */ typename T    //< post param T
        >
    class Foo
    {
        public:
            //| pre method Bar
            inline void Bar(/*| pre x */ uint32 x /*< post x */, /*| pre b */ bool b /*< post b */)
            {
            } //< post Bar

            //| pre state
            T state;
    }
    // < post class template Foo

expected:0

    class Foo{} /*<
                 
     post class Foo

     */
    
expected:0

    // |
    // pre global const Foo
    const uint32 Foo = 10;

expected:0

    //| pre hof Foo
    inline void Foo
        ( ( uint32 //< post first param of function function param
          , uint32 // < post second param of function function param
          ) -> bool fn // < post function param fn
        , uint32 x     //< post param x
        )
    {
    }

expected:0

    template <(uint32 /*< post function param */) -> bool Fn /*<post param Fn */>
    class Foo
    {
    }

expected:0

    //| pre alias Foo

    using Foo = uint32; //< post alias Foo

expected:0

    //| pre alias template Foo
    template <typename T>
    using Foo = T;
    //< post alias template Foo

expected:0

    //| pre enum Foo
    //  with more lines
    enum Foo1 : uint4
    { //| pre One
      One //< post One
    , //| pre Two
      Two  = One + 1 //< post Two
    }
    // < post enum Foo

expected:0

    //| pre struct Foo
    struct Foo
    {
        //| pre field x
        uint32 x;

        //| pre field b
        bool b;
    }
    /*<

    post struct Foo

    */
    //| pre union Bar
    union Bar
    {
        //| pre field x
        uint32 x;

        //| pre field b
        bool b;
    }

expected:0

    //|

expected:1

    //<

expected:1

    //

expected:0

    /*| */

expected:1

    /*< */

expected:1

    /* */

expected:0

    /*|

    /* nested comment not allowed in doc comment */

    */
    struct Foo{}

expected:1

    /*

    /* nested comment */

    */
    struct Foo{}

expected:0

    /*

    /*| doc comment nested within comment */
    struct Foo{}

    */

expected:0

    //< invalid post
    struct Foo{}

expected:1

    struct Foo{}
    //| invalid pre

expected:1

    //| multi
    //< line
    /*| multi-comment
      docs
    */
    struct Foo{}

expected:0

    //| multi
    // line doc comment

    /* not a doc comment ignored

    */
    struct Foo{}

expected:0

    /*|
    multi
    line doc comment
    */

    /* not a doc comment ignored

    */
    struct Foo{}

expected:0

    //| invalid

    //| multile doc comments
    struct Foo{}

expected:1

    //| invalid

    //< multile doc comments
    struct Foo{}

expected:1

    struct Foo{}
    //< invalid

    //< multile doc comments

expected:1

    template <typename T>
    //| invlid pre
    class Foo
    {
    }

expected:1

    import helper.doc_comment_module

expected:0
