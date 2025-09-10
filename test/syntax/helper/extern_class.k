// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.extern_class
  { Class
  , Class_T
  , Class_N
  }

class Class
{
public:
    void Do();

private:
    () -> void callback;
}

template <typename T>
class Class_T
{
public:
    void Do(T x);

private:
    (T x) -> void callback;
}

template <auto N>
class Class_N
{
public:
    void Do(uint<N> x);

private:
    (uint<N> x) -> void callback;
}
