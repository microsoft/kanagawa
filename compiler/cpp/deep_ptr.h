// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

// Wraps a pointer to a T
// Initially null
// Performs a deep copy on assignment
template <typename T> class deep_ptr
{
  public:
    deep_ptr()
    {
        // Initially null
    }

    deep_ptr(const deep_ptr& rhs)
    {
        if (rhs)
        {
            _p.reset(new T);
            *_p.get() = rhs;
        }
    }

    // Move constructor
    deep_ptr(deep_ptr&& rhs) noexcept
    {
        _p.reset(rhs._p.get());
        rhs._p.release();
    }

    deep_ptr& operator=(const deep_ptr& rhs)
    {
        // self-assignment check
        if (this != &rhs)
        {
            if (rhs)
            {
                _p.reset(new T);
                *_p.get() = rhs;
            }
            else
            {
                _p.reset(nullptr);
            }
        }

        return *this;
    }

    // Move assign
    deep_ptr& operator=(deep_ptr&& rhs) noexcept
    {
        // self-assignment check
        if (this != &rhs)
        {
            if (rhs)
            {
                _p.reset(rhs._p.get());
                rhs._p.release();
            }
            else
            {
                _p.reset(nullptr);
            }
        }

        return *this;
    }

    deep_ptr& operator=(const T& rhs)
    {
        _p.reset(new T);
        *_p.get() = rhs;

        return *this;
    }

    operator bool() const { return _p.get() != nullptr; }

    operator T&()
    {
        assert(_p.get() != nullptr);
        return *(_p.get());
    }

    operator const T&() const
    {
        assert(_p.get() != nullptr);
        return *(_p.get());
    }

    // Allocate a pointer and set the value of the pointed at data to be equal to
    // a value-initialized T
    void Initialize()
    {
        T t = {};

        *this = t;
    }

  private:
    std::unique_ptr<T> _p;
};
