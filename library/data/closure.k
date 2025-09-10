// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module data.closure
    { module data.closure.core
    , make_capture1
    , make_capture2
    , make_capture3
    , make_capture4
    , make_capture5
    , make_capture6
    , make_capture7
    , make_capture8
    , make_capture9
    , make_capture10
    , make_capture11
    , make_capture12
    , make_capture13
    , make_capture14
    , make_capture15
    , make_capture16
    , make_capture17
    , make_capture18
    , make_capture19
    , make_capture20
    , make_capture21
    }

import data.closure.core

// TODO: reimplement with variadic templates when supported

//| Capture 1 value
template
    < typename T0
    >
inline auto make_capture1
    ( T0 x0
    )
{
    struct Capture
    {
        T0 x0;
    }

    Capture capture =
        { x0
        };

    return capture;
}

//| Capture 2 values
template
    < typename T0
    , typename T1
    >
inline auto make_capture2
    ( T0 x0
    , T1 x1
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
    }

    Capture capture =
        { x0
        , x1
        };

    return capture;
}

//| Capture 3 values
template
    < typename T0
    , typename T1
    , typename T2
    >
inline auto make_capture3
    ( T0 x0
    , T1 x1
    , T2 x2
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
    }

    Capture capture =
        { x0
        , x1
        , x2
        };

    return capture;
}

//| Capture 4 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    >
inline auto make_capture4
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        };

    return capture;
}

//| Capture 5 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    >
inline auto make_capture5
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        };

    return capture;
}

//| Capture 6 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    >
inline auto make_capture6
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        };

    return capture;
}

//| Capture 7 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    >
inline auto make_capture7
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        };

    return capture;
}

//| Capture 8 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    >
inline auto make_capture8
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        };

    return capture;
}

//| Capture 9 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    , typename T8
    >
inline auto make_capture9
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    , T8 x8
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
        T8 x8;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        };

    return capture;
}

//| Capture 10 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    , typename T8
    , typename T9
    >
inline auto make_capture10
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    , T8 x8
    , T9 x9
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
        T8 x8;
        T9 x9;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        };

    return capture;
}

//| Capture 11 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    , typename T8
    , typename T9
    , typename T10
    >
inline auto make_capture11
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    , T8 x8
    , T9 x9
    , T10 x10
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
        T8 x8;
        T9 x9;
        T10 x10;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        };

    return capture;
}

//| Capture 12 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    , typename T8
    , typename T9
    , typename T10
    , typename T11
    >
inline auto make_capture12
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    , T8 x8
    , T9 x9
    , T10 x10
    , T11 x11
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
        T8 x8;
        T9 x9;
        T10 x10;
        T11 x11;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        , x11
        };

    return capture;
}

//| Capture 13 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    , typename T8
    , typename T9
    , typename T10
    , typename T11
    , typename T12
    >
inline auto make_capture13
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    , T8 x8
    , T9 x9
    , T10 x10
    , T11 x11
    , T12 x12
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
        T8 x8;
        T9 x9;
        T10 x10;
        T11 x11;
        T12 x12;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        , x11
        , x12
        };

    return capture;
}

//| Capture 14 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    , typename T8
    , typename T9
    , typename T10
    , typename T11
    , typename T12
    , typename T13
    >
inline auto make_capture14
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    , T8 x8
    , T9 x9
    , T10 x10
    , T11 x11
    , T12 x12
    , T13 x13
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
        T8 x8;
        T9 x9;
        T10 x10;
        T11 x11;
        T12 x12;
        T13 x13;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        , x11
        , x12
        , x13
        };

    return capture;
}

//| Capture 15 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    , typename T8
    , typename T9
    , typename T10
    , typename T11
    , typename T12
    , typename T13
    , typename T14
    >
inline auto make_capture15
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    , T8 x8
    , T9 x9
    , T10 x10
    , T11 x11
    , T12 x12
    , T13 x13
    , T14 x14
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
        T8 x8;
        T9 x9;
        T10 x10;
        T11 x11;
        T12 x12;
        T13 x13;
        T14 x14;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        , x11
        , x12
        , x13
        , x14
        };

    return capture;
}

//| Capture 16 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    , typename T8
    , typename T9
    , typename T10
    , typename T11
    , typename T12
    , typename T13
    , typename T14
    , typename T15
    >
inline auto make_capture16
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    , T8 x8
    , T9 x9
    , T10 x10
    , T11 x11
    , T12 x12
    , T13 x13
    , T14 x14
    , T15 x15
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
        T8 x8;
        T9 x9;
        T10 x10;
        T11 x11;
        T12 x12;
        T13 x13;
        T14 x14;
        T15 x15;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        , x11
        , x12
        , x13
        , x14
        , x15
        };

    return capture;
}

//| Capture 17 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    , typename T8
    , typename T9
    , typename T10
    , typename T11
    , typename T12
    , typename T13
    , typename T14
    , typename T15
    , typename T16
    >
inline auto make_capture17
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    , T8 x8
    , T9 x9
    , T10 x10
    , T11 x11
    , T12 x12
    , T13 x13
    , T14 x14
    , T15 x15
    , T16 x16
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
        T8 x8;
        T9 x9;
        T10 x10;
        T11 x11;
        T12 x12;
        T13 x13;
        T14 x14;
        T15 x15;
        T16 x16;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        , x11
        , x12
        , x13
        , x14
        , x15
        , x16
        };

    return capture;
}

//| Capture 18 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    , typename T8
    , typename T9
    , typename T10
    , typename T11
    , typename T12
    , typename T13
    , typename T14
    , typename T15
    , typename T16
    , typename T17
    >
inline auto make_capture18
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    , T8 x8
    , T9 x9
    , T10 x10
    , T11 x11
    , T12 x12
    , T13 x13
    , T14 x14
    , T15 x15
    , T16 x16
    , T17 x17
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
        T8 x8;
        T9 x9;
        T10 x10;
        T11 x11;
        T12 x12;
        T13 x13;
        T14 x14;
        T15 x15;
        T16 x16;
        T17 x17;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        , x11
        , x12
        , x13
        , x14
        , x15
        , x16
        , x17
        };

    return capture;
}

//| Capture 19 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    , typename T8
    , typename T9
    , typename T10
    , typename T11
    , typename T12
    , typename T13
    , typename T14
    , typename T15
    , typename T16
    , typename T17
    , typename T18
    >
inline auto make_capture19
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    , T8 x8
    , T9 x9
    , T10 x10
    , T11 x11
    , T12 x12
    , T13 x13
    , T14 x14
    , T15 x15
    , T16 x16
    , T17 x17
    , T18 x18
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
        T8 x8;
        T9 x9;
        T10 x10;
        T11 x11;
        T12 x12;
        T13 x13;
        T14 x14;
        T15 x15;
        T16 x16;
        T17 x17;
        T18 x18;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        , x11
        , x12
        , x13
        , x14
        , x15
        , x16
        , x17
        , x18
        };

    return capture;
}

//| Capture 20 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    , typename T8
    , typename T9
    , typename T10
    , typename T11
    , typename T12
    , typename T13
    , typename T14
    , typename T15
    , typename T16
    , typename T17
    , typename T18
    , typename T19
    >
inline auto make_capture20
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    , T8 x8
    , T9 x9
    , T10 x10
    , T11 x11
    , T12 x12
    , T13 x13
    , T14 x14
    , T15 x15
    , T16 x16
    , T17 x17
    , T18 x18
    , T19 x19
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
        T8 x8;
        T9 x9;
        T10 x10;
        T11 x11;
        T12 x12;
        T13 x13;
        T14 x14;
        T15 x15;
        T16 x16;
        T17 x17;
        T18 x18;
        T19 x19;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        , x11
        , x12
        , x13
        , x14
        , x15
        , x16
        , x17
        , x18
        , x19
        };

    return capture;
}

//| Capture 21 values
template
    < typename T0
    , typename T1
    , typename T2
    , typename T3
    , typename T4
    , typename T5
    , typename T6
    , typename T7
    , typename T8
    , typename T9
    , typename T10
    , typename T11
    , typename T12
    , typename T13
    , typename T14
    , typename T15
    , typename T16
    , typename T17
    , typename T18
    , typename T19
    , typename T20
    >
inline auto make_capture21
    ( T0 x0
    , T1 x1
    , T2 x2
    , T3 x3
    , T4 x4
    , T5 x5
    , T6 x6
    , T7 x7
    , T8 x8
    , T9 x9
    , T10 x10
    , T11 x11
    , T12 x12
    , T13 x13
    , T14 x14
    , T15 x15
    , T16 x16
    , T17 x17
    , T18 x18
    , T19 x19
    , T20 x20
    )
{
    struct Capture
    {
        T0 x0;
        T1 x1;
        T2 x2;
        T3 x3;
        T4 x4;
        T5 x5;
        T6 x6;
        T7 x7;
        T8 x8;
        T9 x9;
        T10 x10;
        T11 x11;
        T12 x12;
        T13 x13;
        T14 x14;
        T15 x15;
        T16 x16;
        T17 x17;
        T18 x18;
        T19 x19;
        T20 x20;
    }

    Capture capture =
        { x0
        , x1
        , x2
        , x3
        , x4
        , x5
        , x6
        , x7
        , x8
        , x9
        , x10
        , x11
        , x12
        , x13
        , x14
        , x15
        , x16
        , x17
        , x18
        , x19
        , x20
        };

    return capture;
}