// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

#include <memory>
#include <stdint.h>
#include <string>

enum class ConsoleColor
{
    Default,
    Red,
    Yellow
};

void SetConsoleColor(const ConsoleColor color);

class Window
{
  public:
    virtual ~Window(){};

    virtual void BeginFrame() = 0;

    virtual void FillRectangle(const size_t x, const size_t y, const size_t width, const size_t height,
                               const uint32_t clr, const char* const label = nullptr) = 0;

    virtual void DrawLine(const size_t x1, const size_t y1, const size_t x2, const size_t y2, const uint32_t clr) = 0;

    virtual void DrawString(const size_t x, const size_t y, const std::string& str) = 0;

    virtual void EndFrame() = 0;

    static std::shared_ptr<Window> Create(const size_t width, const size_t height);
};
