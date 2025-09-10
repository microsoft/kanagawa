// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

// a - b
Placement::Vec2 Placement::Vec2::Add(const Placement::Vec2& a, const Placement::Vec2& b)
{
    return Placement::Vec2(a._x + b._x, a._y + b._y);
}

// a - b
Placement::Vec2 Placement::Vec2::Diff(const Placement::Vec2& a, const Placement::Vec2& b)
{
    return Placement::Vec2(a._x - b._x, a._y - b._y);
}

float Placement::Vec2::Length(const Placement::Vec2& a)
{
    const float diffSquared = (a._x * a._x) + (a._y * a._y);

    return sqrtf(diffSquared);
}

Placement::Vec2 Placement::Vec2::Scale(const Placement::Vec2& a, const float f)
{
    return Placement::Vec2(a._x * f, a._y * f);
}

Placement::Placement() : _nodeCount(0) {}

Placement::Node Placement::AddNode(const std::string& name)
{
    assert(_nodeName.size() == _nodeCount);

    const Node result = _nodeCount;

    ++_nodeCount;

    _nodeName.push_back(name);

    return result;
}

void Placement::AddEdge(const Node node1, const Node node2)
{
    assert(node1 < _nodeCount);
    assert(node2 < _nodeCount);

    const Edge edge = {node1, node2};

    _edges.push_back(edge);
}

size_t Placement::GetEdgeIndex(const Node node1, const Node node2)
{
    assert(node1 != node2);
    assert(node1 < _nodeCount);
    assert(node2 < _nodeCount);

    return node1 * _nodeCount + node2;
}

Placement::Vec2 Placement::GetNodePosition(const Node node)
{
    assert(node < _nodeCount);

    const Vec2 pos = {_x[node], _y[node]};

    return pos;
}

float ClampFloat(const float x, const float low, const float high)
{
    return std::max<float>(std::min<float>(x, high), low);
}

float InvCubeForce(const float coeff, const float val)
{
    const float result = coeff / (val * val * val);

    return result;
}

bool InvalidPosition(const float x) { return (x <= 0.0f) || (x >= 1.0f) || std::isnan(x); }

// clamps: x * bound
// to [0, bound-1]
int32_t FloatToIntClamped(const float x, const float bound)
{
    const float clamped = std::min<float>(std::max<float>(x * bound, 0.0f), bound - 1.0f);

    const int32_t result = static_cast<int32_t>(clamped);

    assert(result >= 0);
    assert(result < static_cast<int32_t>(bound));

    return result;
}

size_t Placement::Run(const Parameters& parameters)
{
    const auto startTime = std::chrono::high_resolution_clock::now();

    size_t numResets = 0;

    const size_t windowWidth = 1600;
    const size_t windowHeight = 1600;

    const float windowWidthF = static_cast<float>(windowWidth);
    const float windowHeightF = static_cast<float>(windowHeight);

    // Create a window to animate the process
    std::shared_ptr<Window> window;
    if (parameters._display)
    {
        window = Window::Create(windowWidth, windowHeight);
    }

    _x.resize(_nodeCount);
    _y.resize(_nodeCount);

    // Randomly initialize node positions
    std::mt19937 rng(static_cast<uint32_t>(parameters._seed));

    std::uniform_real_distribution<float> distribution(0.2f, 0.8f);

    for (size_t i = 0; i < _nodeCount; i++)
    {
        _x[i] = distribution(rng);
        _y[i] = distribution(rng);
    }

    // The amount to change each position on each iteration
    std::vector<float> deltaX(_nodeCount, 0.0f);
    std::vector<float> deltaY(_nodeCount, 0.0f);

    for (size_t iterationIndex = 0; iterationIndex < parameters._numIterations; iterationIndex++)
    {
        // Set deltas to zero
        for (size_t i = 0; i < _nodeCount; i++)
        {
            deltaX[i] = 0.0f;
            deltaY[i] = 0.0f;
        }

        // For each edge
        for (const Edge& edge : _edges)
        {
            const size_t i = edge._node1;

            const size_t j = edge._node2;

            const Vec2 posI = GetNodePosition(i);

            const Vec2 posJ = GetNodePosition(j);

            const Vec2 jMinusI = Vec2::Diff(posJ, posI);

            // Pull nodes together - linear force
            // The length of jMinusI is not present here, which makes the force strength linear with distance
            const Vec2 scaled = Vec2::Scale(jMinusI, parameters._pullStrength);

            deltaX[i] += scaled._x;
            deltaY[i] += scaled._y;

            deltaX[j] -= scaled._x;
            deltaY[j] -= scaled._y;
        }

        // For each pair of nodes
        for (size_t i = 0; i < _nodeCount; i++)
        {
            const Vec2 posI = GetNodePosition(i);

            for (size_t j = (i + 1); j < _nodeCount; j++)
            {
                const Vec2 posJ = GetNodePosition(j);

                const Vec2 jMinusI = Vec2::Diff(posJ, posI);

                const float jMinusILength = Vec2::Length(jMinusI);

                // Push nodes apart - inv quadratic force
                // And extra jMinusILength term is added to counteract the fact that
                // Thee scale of jMinusI already has the length factored in
                const float strength = -InvCubeForce(parameters._pushStrength, jMinusILength);

                const Vec2 scaled = Vec2::Scale(jMinusI, strength);

                deltaX[i] += scaled._x;
                deltaY[i] += scaled._y;

                deltaX[j] -= scaled._x;
                deltaY[j] -= scaled._y;
            }
        }

        // Push nodes away from walls - inv cubic force
        for (size_t i = 0; i < _nodeCount; i++)
        {
            {
                const float x = _x[i];
                const float invX = 1.0f - x;

                deltaX[i] += InvCubeForce(parameters._wallStrength, x);
                deltaX[i] -= InvCubeForce(parameters._wallStrength, invX);
            }

            {
                const float y = _y[i];
                const float invY = 1.0f - y;

                deltaY[i] += InvCubeForce(parameters._wallStrength, y);
                deltaY[i] -= InvCubeForce(parameters._wallStrength, invY);
            }
        }

        // Find the longest gradient vector
        float longestGradient = 0.0f;

        for (size_t i = 0; i < _nodeCount; i++)
        {
            const Vec2 gradient(deltaX[i], deltaY[i]);

            const float length = Vec2::Length(gradient);

            longestGradient = std::max<float>(longestGradient, length);
        }

        if (longestGradient > parameters._updateRate)
        {
            const float scale = parameters._updateRate / longestGradient;

            for (size_t i = 0; i < _nodeCount; i++)
            {
                // Scale gradients so that no update vector is longer than parameters._updateRate
                const Vec2 gradient(deltaX[i], deltaY[i]);

                const Vec2 scaledGradient = Vec2::Scale(gradient, scale);

                deltaX[i] = scaledGradient._x;
                deltaY[i] = scaledGradient._y;
            }
        }

        // Update positions
        for (size_t i = 0; i < _nodeCount; i++)
        {
            _x[i] = _x[i] + deltaX[i];
            _y[i] = _y[i] + deltaY[i];

            // Regenerate positions on out-of-bounds or NAN
            if (InvalidPosition(_x[i]) || InvalidPosition(_y[i]))
            {
                _x[i] = distribution(rng);
                _y[i] = distribution(rng);

                numResets++;
            }
        }

        if (parameters._display)
        {
            // Update window
            window->BeginFrame();

            const size_t rectSize = 10;

            const size_t halfRectSize = rectSize / 2;

            // Clear the background
            window->FillRectangle(0, 0, windowWidth, windowHeight, 0xffffffff);

            // Color each node
            for (size_t i = 0; i < _nodeCount; i++)
            {
                const Vec2 posI = GetNodePosition(i);

                const size_t x = static_cast<size_t>(posI._x * windowWidthF);
                const size_t y = static_cast<size_t>(posI._y * windowHeightF);

                window->FillRectangle(x, y, rectSize, rectSize, 0xffff0000, _nodeName[i].c_str());
            }

            // Color each edge
            for (const Edge& edge : _edges)
            {
                const Vec2 pos1 = GetNodePosition(edge._node1);
                const Vec2 pos2 = GetNodePosition(edge._node2);

                window->DrawLine(static_cast<size_t>(pos1._x * windowWidthF) + halfRectSize,
                                 static_cast<size_t>(pos1._y * windowHeightF) + halfRectSize,
                                 static_cast<size_t>(pos2._x * windowWidthF) + halfRectSize,
                                 static_cast<size_t>(pos2._y * windowHeightF) + halfRectSize, 0xff000000);
            }

            window->EndFrame();
        }
    }

    const auto endTime = std::chrono::high_resolution_clock::now();

    const auto elapsedMs = std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count();

    return numResets;
}
