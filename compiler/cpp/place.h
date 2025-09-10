// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Simulated placement to enable the compiler to make decisions
// about which modules should be connected to each other
#pragma once

#include "options.h"

VerbosityLevel Verbosity();

class Placement
{
  public:
    Placement();

    struct Vec2
    {
        float _x;
        float _y;

        Vec2() {}

        Vec2(const float a, const float b) : _x(a), _y(b) {}

        bool operator<(const Vec2& rhs) const
        {
            if (_x < rhs._x)
            {
                return true;
            }
            else if (_x > rhs._x)
            {
                return false;
            }
            else
            {
                return _y < rhs._y;
            }
        }

        static Vec2 Add(const Vec2& a, const Vec2& b);
        static Vec2 Diff(const Vec2& a, const Vec2& b);
        static float Length(const Vec2& vec);
        static Vec2 Scale(const Vec2& a, const float f);
    };

    typedef size_t Node;

    typedef PlacementOptions Parameters;

    struct SortRecord
    {
        std::string _title;
        std::list<std::string> _names;
    };

    Node AddNode(const std::string& label = std::string(""));

    void AddEdge(const Node node1, const Node node2);

    // Returns the number of times a node position was recent
    // because it went out of bounds
    size_t Run(const Parameters& parameters);

    Vec2 GetNodePosition(const Node node);

    // Sorts a list of objects by position
    // The callback returns the position of a given object
    // Elements are arranged such that they are near each other
    template <typename T>
    static SortRecord Sort(std::list<T>& currRoute, const std::function<Vec2(T)>& getPositionCallback,
                           const std::function<std::string(T)>& getNameCallback, const bool display,
                           const std::string& name, const bool runToConvergence = true);

  private:
    size_t GetEdgeIndex(const Node node1, const Node node2);

    template <typename T>
    static float GetTotalDistance(std::list<T>& list, const std::function<Vec2(T)>& getPositionCallback);

    struct Edge
    {
        Node _node1;
        Node _node2;
    };

    size_t _nodeCount;

    std::vector<Edge> _edges;

    // _x[i] is the x coordinate of node i
    std::vector<float> _x;
    std::vector<float> _y;

    std::vector<std::string> _nodeName;
};

template <typename T>
float Placement::GetTotalDistance(std::list<T>& list, const std::function<Vec2(T)>& getPositionCallback)
{
    Vec2 previousPos = {};
    bool isFirst = true;

    float result = 0.0f;

    for (const T& obj : list)
    {
        const Vec2 pos = getPositionCallback(obj);

        if (!isFirst)
        {
            const Vec2 diff = Vec2::Diff(previousPos, pos);

            const float length = Vec2::Length(diff);

            result += length;
        }

        isFirst = false;

        previousPos = pos;
    }

    return result;
}

// 2-opt algorithm for the traveling salesmen problem
// Returns a description of the results, for logging purposes
// runToConvergence = false causes the algorithm to run quickly (but not spend much effort)
template <typename T>
Placement::SortRecord Placement::Sort(std::list<T>& routeInOut, const std::function<Vec2(T)>& getPositionCallback,
                                      const std::function<std::string(T)>& getNameCallback, const bool display,
                                      const std::string& name, const bool runToConvergence)
{
    SortRecord sortRecord = {};

    sortRecord._title = name;

    // Early-out on empty lists, to avoid popping a blank window
    // Also, skip lists of size = 1, for performance
    if (routeInOut.size() < 2)
    {
        return sortRecord;
    }

    const auto startTime = std::chrono::high_resolution_clock::now();

    // Flatten all points into a vector
    struct ItemRecord
    {
        T _item;
        Vec2 _position;
        std::string _name;
    };

    std::vector<ItemRecord> itemVec;

    for (const T& item : routeInOut)
    {
        ItemRecord itemRecord = {};

        itemRecord._item = item;
        itemRecord._position = getPositionCallback(item);
        itemRecord._name = getNameCallback(item);

        itemVec.push_back(itemRecord);
    }

    // The internal sorting algorithm uses these callbacks, not the caller-specified ones
    const auto getPositionInternal = [&](const size_t index) { return itemVec[index]._position; };

    const auto getNameInternal = [&](const size_t index) { return itemVec[index]._name; };

    // Deduplicate based on position
    // If multiple items have the same position, then only 1 will participate in the time-consuming algorithm below
    // It will be the placeholder for all items with the same position

    // Maps position to the index into sortRecords of the placeholder
    std::map<Vec2, size_t> placeholderMap;

    // Maps placeholder index to all the items that placeholder represents (including iteself)
    std::map<size_t, std::list<size_t>> placeholderLists;

    std::list<size_t> currRoute;

    for (size_t i = 0; i < itemVec.size(); i++)
    {
        const ItemRecord& itemRecord = itemVec[i];

        const auto it = placeholderMap.find(itemRecord._position);

        size_t placeholderIndex = std::numeric_limits<size_t>::max();

        if (it == placeholderMap.end())
        {
            placeholderMap[itemRecord._position] = i;

            currRoute.push_back(i);

            placeholderIndex = i;
        }
        else
        {
            placeholderIndex = it->second;
        }

        placeholderLists[placeholderIndex].push_back(i);
    }

    assert(!currRoute.empty());
    const size_t routeSize = currRoute.size();

    const size_t windowWidth = 1600;
    const size_t windowHeight = 1600;

    // Extra size added to the window dimensions to allow the text to fit
    const size_t padding = 100;

    const size_t paddedWindowWidth = windowWidth + padding;
    const size_t paddedWindowHeight = windowHeight + padding;

    const float windowWidthF = static_cast<float>(windowWidth);
    const float windowHeightF = static_cast<float>(windowHeight);

    std::shared_ptr<Window> window;

    if (display)
    {
        // Create a window to animate the process
        window = Window::Create(paddedWindowWidth, paddedWindowHeight);
    }

    bool forwardProgress = false;

    bool displayedOnce = false;

    do
    {
        forwardProgress = false;

        for (size_t i = 0; i < routeSize; i++)
        {
            for (size_t j = (i + 1); j < routeSize; j++)
            {
                // Generate a new route
                std::list<size_t> newRoute;

                // list[0, i-1] are copied exactly
                auto it = currRoute.begin();

                for (size_t x = 0; x < i; x++)
                {
                    newRoute.push_back(*it);
                    ++it;
                }

                // list[i, j-1] are added in reverse order
                std::list<size_t> routeToReverse;
                for (size_t x = i; x < j; x++)
                {
                    routeToReverse.push_front(*it);
                    ++it;
                }

                newRoute.splice(newRoute.end(), routeToReverse);

                // list[j, the end] are copied exactly
                for (size_t x = j; x < routeSize; x++)
                {
                    newRoute.push_back(*it);
                    ++it;
                }

                assert(it == currRoute.end());

                // See if the change improved things
                const float currRouteDistance = GetTotalDistance<size_t>(currRoute, getPositionInternal);
                const float newRouteDistance = GetTotalDistance<size_t>(newRoute, getPositionInternal);

                bool shouldDisplay = !displayedOnce;

                if (newRouteDistance < currRouteDistance)
                {
                    currRoute = newRoute;

                    forwardProgress = true;

                    shouldDisplay = true;
                }

                // Show new route in the window (if something has changed, or if the route has never been displayed)
                if (display && shouldDisplay)
                {
                    displayedOnce = true;

                    // Update window
                    window->BeginFrame();

                    // Clear the background
                    window->FillRectangle(0, 0, paddedWindowWidth, paddedWindowHeight, 0xffffffff);

                    // Draw the figure name
                    window->DrawString(0, 0, name);

                    auto it = currRoute.begin();

                    Placement::Vec2 previousPos = {};

                    for (size_t i = 0; i < currRoute.size(); i++)
                    {
                        const size_t& itemIndex = *it;

                        const Placement::Vec2 pos = getPositionInternal(itemIndex);

                        // Label the item
                        const std::string itemName = getNameInternal(itemIndex);

                        window->DrawString(static_cast<size_t>(pos._x * windowWidthF),
                                           static_cast<size_t>(pos._y * windowHeightF), itemName);

                        // Draw the connection
                        if (i > 0)
                        {
                            window->DrawLine(static_cast<size_t>(previousPos._x * windowWidthF),
                                             static_cast<size_t>(previousPos._y * windowHeightF),
                                             static_cast<size_t>(pos._x * windowWidthF),
                                             static_cast<size_t>(pos._y * windowHeightF), 0xff000000);
                        }

                        previousPos = pos;

                        ++it;
                    }

                    window->EndFrame();

                    // Let the user see the animation
                    std::this_thread::sleep_for(std::chrono::milliseconds(100));
                }
            }
        }
    } while (forwardProgress && runToConvergence);

    assert(currRoute.size() == routeSize);

    if (display)
    {
        // Allow some time before closing the window
        std::this_thread::sleep_for(std::chrono::seconds(10));
    }

    // Write result back into routeInOut, re-duplicating
    const size_t sizeIn = routeInOut.size();

    routeInOut.clear();

    for (const size_t i : currRoute)
    {
        const std::list<size_t>& placeholderList = placeholderLists[i];
        assert(!placeholderList.empty());

        for (const size_t itemIndex : placeholderList)
        {
            const ItemRecord& itemRecord = itemVec[itemIndex];

            routeInOut.push_back(itemRecord._item);
        }
    }

    // Return sorted list of names, for logging
    for (const T& item : routeInOut)
    {
        const std::string itemName = getNameCallback(item);

        sortRecord._names.push_back(itemName);
    }

    // Assert that no items were lost during deduplication
    assert(sizeIn == routeInOut.size());

    const auto endTime = std::chrono::high_resolution_clock::now();

    const auto elapsedSeconds = std::chrono::duration_cast<std::chrono::seconds>(endTime - startTime).count();

    // Detect sorting operations that take a long time, and log them
    if (Verbosity() > Normal && elapsedSeconds > 5)
    {
        std::cout << "Sorting for: " << name << " took " << elapsedSeconds << " seconds\n";
    }

    return sortRecord;
}
