// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "platform.h"
#ifdef _WIN32
#include <windows.h>
#else
#endif
#include <assert.h>
#include <map>
#include <stdint.h>
#include <vector>

#ifdef _WIN32
void SetConsoleColor(const ConsoleColor color)
{
    uint16_t windowsColor = 0;

    switch (color)
    {
    case ConsoleColor::Default:
        windowsColor = 0x0007;
        break;

    case ConsoleColor::Red:
        windowsColor = FOREGROUND_INTENSITY | FOREGROUND_RED;
        break;

    case ConsoleColor::Yellow:
        windowsColor = FOREGROUND_INTENSITY | FOREGROUND_RED | FOREGROUND_GREEN;
        break;

    default:
        assert(false);
    }

    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), windowsColor);
}

class WindowsWindow : public Window
{
  private:
    struct Label
    {
        size_t _left;
        size_t _top;
        size_t _right;
        size_t _bottom;

        std::string _label;
    };

  public:
    WindowsWindow(const size_t width, const size_t height)
    {
        _window = CreateWindow("STATIC", "Placement", WS_VISIBLE, 0, 0, static_cast<int32_t>(width),
                               static_cast<int32_t>(height), nullptr, nullptr, GetModuleHandle(nullptr), nullptr);
    }

    ~WindowsWindow()
    {
        for (const auto& p : _brushMap)
        {
            DeleteObject(p.second);
        }

        DestroyWindow(_window);
    }

    void BeginFrame() override
    {
        _labels.clear();

        RECT clientRect = {};
        GetClientRect(_window, &clientRect);

        _windowDC = GetDC(_window);

        // Create a device context that will be rendered into (off-screen)
        _dc = CreateCompatibleDC(_windowDC);

        // Create a bitmap to render into
        _bitmap = CreateCompatibleBitmap(_windowDC, clientRect.right, clientRect.bottom);

        SelectObject(_dc, _bitmap);
    }

    void FillRectangle(const size_t x, const size_t y, const size_t width, const size_t height, const uint32_t clr,
                       const char* const label) override
    {
        if (_brushMap.find(clr) == _brushMap.end())
        {
            _brushMap[clr] = CreateSolidBrush(clr & 0x00ffffff);
        }

        HGDIOBJ oldBrush = SelectObject(_dc, _brushMap[clr]);

        Rectangle(_dc, static_cast<int32_t>(x), static_cast<int32_t>(y), static_cast<int32_t>(x + width),
                  static_cast<int32_t>(y + height));

        SelectObject(_dc, oldBrush);

        if (label)
        {
            Label labelRecord = {};

            labelRecord._left = x;
            labelRecord._top = y;
            labelRecord._right = x + width;
            labelRecord._bottom = y + height;

            labelRecord._label = label;

            _labels.push_back(labelRecord);
        }
    }

    void DrawLine(const size_t x1, const size_t y1, const size_t x2, const size_t y2, const uint32_t clr)
    {
        if (_penMap.end() == _penMap.find(clr))
        {
            _penMap[clr] = CreatePen(PS_SOLID, 1, clr);
        }

        HGDIOBJ oldPen = SelectObject(_dc, _penMap[clr]);

        MoveToEx(_dc, static_cast<int32_t>(x1), static_cast<int32_t>(y1), nullptr);

        LineTo(_dc, static_cast<int32_t>(x2), static_cast<int32_t>(y2));

        SelectObject(_dc, oldPen);
    }

    void DrawString(const size_t x, const size_t y, const std::string& str) override
    {
        TextOut(_dc, static_cast<int32_t>(x), static_cast<int32_t>(y), str.c_str(), static_cast<int32_t>(str.length()));
    }

    void EndFrame() override
    {
        // See if the mouse is over a label
        POINT cursorPos = {};

        if (GetCursorPos(&cursorPos))
        {
            if (ScreenToClient(_window, &cursorPos))
            {
                for (const Label& label : _labels)
                {
                    if ((cursorPos.x >= label._left) && (cursorPos.x < label._right) && (cursorPos.y >= label._top) &&
                        (cursorPos.y < label._bottom))
                    {
                        DrawString(0, 0, label._label);
                    }
                }
            }
        }

        RECT clientRect = {};
        GetClientRect(_window, &clientRect);

        // Copy from off-screen bitmap to the window
        BitBlt(_windowDC, 0, 0, clientRect.right, clientRect.bottom, _dc, 0, 0, SRCCOPY);

        DeleteObject(_bitmap);

        DeleteDC(_dc);

        ReleaseDC(_window, _windowDC);

        // Pump window messages
        MSG msg = {};

        while (PeekMessage(&msg, _window, 0, 0, PM_REMOVE))
        {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }

  private:
    HWND _window;
    HDC _windowDC;
    HDC _dc;
    HBITMAP _bitmap;
    std::map<uint32_t, HBRUSH> _brushMap;
    std::map<uint32_t, HPEN> _penMap;
    std::vector<Label> _labels;
};

std::shared_ptr<Window> Window::Create(const size_t width, const size_t height)
{
    return std::make_shared<WindowsWindow>(width, height);
}

#else
void SetConsoleColor(const ConsoleColor color) {}

class FakeWindow : public Window
{
  public:
    FakeWindow() {}

    virtual ~FakeWindow(){};

    virtual void BeginFrame(){};

    virtual void FillRectangle(const size_t x, const size_t y, const size_t width, const size_t height,
                               const uint32_t clr, const char* const label = nullptr)
    {
    }

    virtual void DrawLine(const size_t x1, const size_t y1, const size_t x2, const size_t y2, const uint32_t clr) {}

    virtual void DrawString(const size_t x, const size_t y, const std::string& str) {}

    virtual void EndFrame(){};

    static std::shared_ptr<Window> Create(const size_t width, const size_t height);
};

std::shared_ptr<Window> Window::Create(const size_t width, const size_t height)
{
    return std::make_shared<FakeWindow>();
}

#endif