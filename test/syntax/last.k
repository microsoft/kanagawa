// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// [[last]] on inline function
inline void main([[last]] bool end)
{
}
expected:0

// [[last]] specified twice
inline void main([[last]] bool x, [[last]] bool end)
{
}
expected:56

// [[last]] is not boolean type
inline void main(uint32 x, [[last]] uint32 end)
{
}
expected:56

class Main
{
    // [[last]] with [[transaction_size]]
    void helper([[last]] bool last)
    {
    }

public:
    void main(uint32 x)
    {
        [[transaction_size(64)]] helper(x % 64 == 1);
        // non power-of-2 transaction size
        [[transaction_size(65)]] helper(x % 64 == 1);
    }
}

expected:0

class Main
{
    // [[transaction_size]] without [[last]]
    void helper(bool last)
    {
    }
public:
    void main(uint32 x)
    {
        [[transaction_size(64)]] helper(x % 64 == 63);
        [[transaction_size(128)]] helper(x % 128 == 127);
    }
}

export Main;

expected:27

class Main
{
    // [[transaction_size(0)]]
    void helper([[last]] bool last)
    {
    }
public:

    void main(uint32 x)
    {
        [[transaction_size(0)]] helper(x % 64 == 63);
        [[transaction_size(0)]] helper(x % 128 == 127);
    }
}

export Main;

expected:27

class Main
{
    // [[transaction_size]] not specified on all call sites
    void helper([[last]] bool last)
    {
    }

public:
    void main(uint32 x)
    {
        [[transaction_size(64)]] helper(x % 64 == 1);
        helper(x % 64 == 1);
    }
}

export Main;

expected:0

class Main
{
    // [[transaction_size]] not specified on all call sites
    void helper([[last]] bool last)
    {
    }

public:
    void main(uint32 x)
    {
        helper(x % 64 == 1);
        [[transaction_size(64)]] helper(x % 64 == 1);
    }
}

export Main;

expected:0

class Main
{
    // [[transaction_size]] with [[fifo_depth]]
    void helper([[last]] bool last)
    {
    }

public:
    void main(uint32 x)
    {
        [[transaction_size(64), fifo_depth(256)]] helper(x % 64 == 1);
        [[transaction_size(128), fifo_depth(32)]] helper(x % 128 == 1);
    }
}

export Main;

expected:0

// inline
inline void helper([[last]] bool last)
{
}

inline void main(uint32 x)
{
    [[transaction_size(64)]] helper(x % 64 == 1);
    [[transaction_size(128)]] helper(x % 128 == 1);
}
expected:27

class Main
{
    // constant last values
    void helper([[last]] bool last)
    {
    }

public:
    void main(uint32 x)
    {
        [[transaction_size(64)]] helper(true);
        helper(true);
    }
}

export Main;

expected:0

class Main
{
    // constant last values
    void helper([[last]] bool last)
    {
    }

public:
    void main(uint32 x)
    {
        [[transaction_size(64)]] helper(false);
        [[transaction_size(128)]] helper(true);
    }
}

export Main;

expected:27

class Main
{
    // constant last values
    void helper([[last]] bool last)
    {
    }

public:
    void main(uint32 x)
    {
        helper(true);
        helper(false);
    }
}

export Main;

expected:27

class Main
{
    // [[transaction_size]] used with inline function
    inline void helper([[last]] bool last)
    {
    }

public:
    void main(uint32 x)
    {
        [[transaction_size(64)]] helper(x % 64 == 63);
    }
}

export Main;

expected:27
