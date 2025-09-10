// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.sleep
    { sleep
    }

template <auto N>
class Sleeper
{
public:
    void impl()
    {
        stages<N>();
    }
}

template <auto N>
inline void sleep()
{
    static Sleeper<N> sleeper;
    sleeper.impl();
}
