module sample.decls.initializer
    { I
    }

class I
{
public:
    uint32 a = {}; // initialized to 0

    struct Flag
    {
        bool b;
    }

    struct S
    {
        uint8 x;
        int16 y;
        Flag f;
    }

    S s1 = {3, 4, {true}}; //< x = 3, y = 4, t.f.b = true
    S s2 = {3};            //< x = 3, y = 0, t.f.b = false
    S s3 = { .y = -29 };   //< y = -29

    struct Foo
    {
        bool valid;
        uint4 index;
    }

    struct Bar
    {
        Foo[4] foos;
    }

    Bar bar = { .foos =
        { { .valid = false
          , .index = 0
          }
        , { .valid = true
          , .index = 1
          }
        }
    };
}
