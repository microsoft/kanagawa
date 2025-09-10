// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module helper.export_class
    { PublicMethodTemplate
    , PublicInlineMethod
    , PublicCallback
    , PublicData
    , PublicTypes
    , PublicHOF
    , PrivateMethodTemplate
    , PrivateInlineMethod
    , PrivateCallback
    , PrivateData
    , PrivateHOF
    , Exportable
    }

class PublicTypes
{
public:
    template <typename T>
    using X = T;

    struct S {}

    template <typename T>
    struct R {T x;}
}

class PublicHOF
{
public:
    inline void Go(() -> void Do)
    {
    }
}

class PublicMethodTemplate
{
public:
    template <typename T>
    inline void Go(){}
}

class PrivateMethodTemplate
{
    template <typename T>
    void Go(){}
}

class PublicData
{
public:
    uint32 x;
}

class PrivateHOF
{
private:
    inline void Go(() -> void Do)
    {
    }
}

class PrivateData
{
    uint32 x;
}

class PublicInlineMethod
{
private:
public:
    inline void Go(){}
}

class PrivateInlineMethod
{
private:
    inline void Go(){}
}

class PublicCallback
{
private:
public:
private:
public:
    () -> void callback;
}

class PrivateCallback
{
public:
private:
    () -> void callback;
}

class Exportable
{
public:
    void Go(){}

private:
    (uint32, uint32) -> void callback;    
}
