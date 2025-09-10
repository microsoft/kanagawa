// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

std::ostream& operator<<(std::ostream& str, const IntegerParameterVector& paramVector)
{
    for (size_t i = 0; i < paramVector.GetParameterCount(); i++)
    {
        if (i > 0)
        {
            str << " ";
        }

        if (paramVector.IsParameterDefined(i))
        {
            const size_t value = paramVector.GetParameter(i);

            str << value;
        }
        else
        {
            str << "X";
        }
    }

    return str;
}

std::ostream& operator<<(std::ostream& str, const ObjectiveFunctionBounds& bounds)
{
    str << "[" << bounds._lowerBound << ", " << bounds._upperBound << "]";

    return str;
}

CompressedIntegerVector::CodeBook CompressedIntegerVector::CreateCodeBook(const std::vector<size_t>& ranges)
{
    CodeBook result = {};

    const size_t bitsPerWord = sizeof(word_t) * 8;

    size_t bitsRemaining = 0;

    for (const size_t range : ranges)
    {
        assert(range > 0);

        // Determine the number of bits required to represent this value
        const size_t bitCount = Log2RoundUp(range);

        // Check to see if there are enough bits remaining in the current word
        if (bitsRemaining < bitCount)
        {
            result._numWords++;

            bitsRemaining = bitsPerWord;
        }

        assert((bitCount == 0) || result._numWords > 0);

        CodeBookEntry cbe = {};

        cbe._upperBound = range;

        if (bitCount == 0)
        {
            cbe._isConstantZero = true;
        }
        else
        {
            cbe._wordIndex = result._numWords - 1;
            cbe._shift = bitsPerWord - bitsRemaining;
            cbe._mask = ((1ull << bitCount) - 1) << cbe._shift;
        }

        result._entries.push_back(cbe);

        assert(bitsRemaining >= bitCount);
        bitsRemaining -= bitCount;
    }

    return result;
}

CompressedIntegerVector::CompressedIntegerVector() : _codeBook(nullptr) {}

CompressedIntegerVector::CompressedIntegerVector(const CodeBook& codeBook)
    : _codeBook(&codeBook), _words(codeBook._numWords)
{
}

void CompressedIntegerVector::Set(const size_t index, const size_t value)
{
    const CodeBookEntry& cbe = GetCodeBookEntry(index);

    assert(value < cbe._upperBound);

    if (!cbe._isConstantZero)
    {
        word_t& word = _words[cbe._wordIndex];

        // clear old bits
        word &= ~cbe._mask;

        // write new bits
        word |= (value << cbe._shift);
    }
}

size_t CompressedIntegerVector::Get(const size_t index) const
{
    const CodeBookEntry& cbe = GetCodeBookEntry(index);

    if (cbe._isConstantZero)
    {
        return 0;
    }
    else
    {
        const word_t word = _words[cbe._wordIndex];

        return (word & cbe._mask) >> cbe._shift;
    }
}

const CompressedIntegerVector::CodeBookEntry& CompressedIntegerVector::GetCodeBookEntry(const size_t index) const
{
    assert(_codeBook);
    assert(index < _codeBook->_entries.size());

    return _codeBook->_entries[index];
}

// Returns false if maxWorkListSize is exceeded
bool IntegerBranchAndBoundImpl(const std::vector<size_t>& parameterRanges, const BoundObjectiveFunction& boundCallback,
                               const std::set<size_t>& parametersToOptimize,
                               const boost::optional<size_t> maxWorkListSize,
                               const std::map<size_t, size_t>& fixedParameterValues)
{
    const size_t parameterCount = parameterRanges.size();

    assert(parameterCount != 0);
    assert(!parametersToOptimize.empty());

    // Assert valid ranges
    for (const size_t& range : parameterRanges)
    {
        assert(range > 0);
    }

    // Determine how to encode parameter values
    const CompressedIntegerVector::CodeBook codeBook = CompressedIntegerVector::CreateCodeBook(parameterRanges);

    // Given an IntegerParameterVector with some undefined parameters
    // choose 1 parameter to define
    const auto selectParameterToDefine = [&](const IntegerParameterVector& ipv)
    {
        boost::optional<size_t> selectedParameterIndex;

        // Choose an undefined parameter with minimal range
        // This lazy defers evaluation of parameters with large ranges
        // until the end of the search, to avoid combinatorial explosion of the work list
        // Hopefully the search will terminate
        // before those parameters are reached, or the branching process
        // will eliminate large regions of the search space before those parameters are reached
        for (size_t i = 0; i < ipv.GetParameterCount(); i++)
        {
            if (!ipv.IsParameterDefined(i) && (parametersToOptimize.end() != parametersToOptimize.find(i)))
            {
                if (selectedParameterIndex)
                {
                    if (parameterRanges[i] < parameterRanges[*selectedParameterIndex])
                    {
                        selectedParameterIndex = i;
                    }
                }
                else
                {
                    selectedParameterIndex = i;
                }
            }
        }

        assert(selectedParameterIndex);
        return *selectedParameterIndex;
    };

    // Returns true if all parameters contained in parametersToOptimize are defined
    const auto allDefined = [&](const IntegerParameterVector& params)
    {
        bool result = true;

        for (const size_t parameterIndex : parametersToOptimize)
        {
            if (!params.IsParameterDefined(parameterIndex))
            {
                result = false;
                break;
            }
        }

        return result;
    };

    // Initially, there is exactly 1 parameter vector
    // and all parameter values are undefined in that vector
    assert(parameterCount > 0);

    struct WorkListEntry
    {
        IntegerParameterVector _parameters;

        ObjectiveFunctionBounds _bounds;
    };

    std::list<WorkListEntry> workList;

    // Seed the work list 1 with vector
    // containing fixed parameter values
    {
        WorkListEntry wle = {};

        wle._parameters = IntegerParameterVector(codeBook);

        for (const auto& p : fixedParameterValues)
        {
            assert(!wle._parameters.IsParameterDefined(p.first));

            wle._parameters.SetParameter(p.first, p.second);
        }

        wle._bounds = boundCallback(wle._parameters, false);

        workList.push_back(wle);
    }

    size_t bestObjectiveFunctionValue = std::numeric_limits<size_t>::max();
    IntegerParameterVector bestParameters;

    ThreadPool threadPool;

    while (!workList.empty())
    {
        // if parametersToOptimize.size() == 1, then ignore the maximum work list size
        // as it wil be impossible to reduce the work list size by removing vertices
        if (maxWorkListSize && (workList.size() > *maxWorkListSize) && (parametersToOptimize.size() > 1))
        {
            // Optimization could not find optimal value the specified work list size
            return false;
        }

        // Compute the lowest lower and upper bounds
        size_t lowestLowerBound = std::numeric_limits<size_t>::max();
        size_t lowestUpperBound = std::numeric_limits<size_t>::max();

        for (const WorkListEntry& wle : workList)
        {
            lowestLowerBound = std::min(lowestLowerBound, wle._bounds._lowerBound);
            lowestUpperBound = std::min(lowestUpperBound, wle._bounds._upperBound);
        }

        // If the lowest possible objective function value
        // in the region of the search space covered by "bounds" is
        // greater than the highest possible objective function value
        // in other part of the search space, then skip
        // searching the space covered by "bounds"
        const auto shouldFilter = [lowestUpperBound,
                                   bestObjectiveFunctionValue](const ObjectiveFunctionBounds& bounds) -> bool
        { return (bounds._lowerBound > lowestUpperBound) || (bounds._lowerBound >= bestObjectiveFunctionValue); };

        std::vector<WorkListEntry> elementsToRefine;
        std::list<WorkListEntry> newWorkList;

        for (const WorkListEntry& wle : workList)
        {
            const ObjectiveFunctionBounds& bounds = wle._bounds;

            if (shouldFilter(bounds))
            {
                // Don't bother with this portion of the search space
                continue;
            }

            if (allDefined(wle._parameters))
            {
                if (bounds._lowerBound < bestObjectiveFunctionValue)
                {
                    // Found a new best solution
                    bestObjectiveFunctionValue = bounds._lowerBound;

                    bestParameters = wle._parameters;
                }
            }
            else
            {
                if ((bounds._lowerBound == lowestLowerBound) || (bounds._upperBound == lowestUpperBound))
                {
                    // Refining this work list entry could affect
                    // the value of lowestLowerBound or lowestUpperBound
                    // during the next iteration, which could result in stopping the search
                    // or filtering more elements, so refine it.
                    elementsToRefine.push_back(wle);
                }
                else
                {
                    newWorkList.push_back(wle);
                }
            }
        }

        // process each element in elementsToRefine concurrently
        const auto refineCallback = [&](std::list<WorkListEntry>& resultList, const size_t elementIndex)
        {
            assert(elementIndex < elementsToRefine.size());

            const WorkListEntry& baseWle = elementsToRefine[elementIndex];

            // Define all possible values of one additional parameter
            const size_t newParameterIndex = selectParameterToDefine(baseWle._parameters);
            assert(newParameterIndex < parameterCount);

            const size_t newParameterRange = parameterRanges[newParameterIndex];

            for (size_t newParameterValue = 0; newParameterValue < newParameterRange; newParameterValue++)
            {
                IntegerParameterVector newParams = baseWle._parameters;

                assert(!newParams.IsParameterDefined(newParameterIndex));
                newParams.SetParameter(newParameterIndex, newParameterValue);

                const ObjectiveFunctionBounds bounds = boundCallback(newParams, false);

                assert(bounds._lowerBound <= bounds._upperBound);

                // Bounds after refinement should not be looser than before
                // Note that _upperBound does not need to always decrease
                // as _upperBound is an upper bound on the best solution in the space
                assert(bounds._lowerBound >= baseWle._bounds._lowerBound);
                assert(bounds._upperBound >= baseWle._bounds._lowerBound);

                // Do not filter here based on lowestUpperBound
                // because lowestUpperBound was based on the coarse decomposition of the search space before refinement
                WorkListEntry wle = {};

                wle._parameters = newParams;
                wle._bounds = bounds;

                resultList.push_back(wle);
            }
        };

        threadPool.MapToList<WorkListEntry>(newWorkList, elementsToRefine.size(), refineCallback);

        std::swap(workList, newWorkList);
    }

    assert(bestObjectiveFunctionValue != std::numeric_limits<size_t>::max());

    // All parameters in parametersToOptimize should have been defined
    for (const size_t paramIndex : parametersToOptimize)
    {
        assert(bestParameters.IsParameterDefined(paramIndex));
    }

    // All parameter values that match keys in parametersToOptimize should have pre-defined values
    for (const auto& p : fixedParameterValues)
    {
        assert(bestParameters.GetParameter(p.first) == p.second);
    }

    // Call the callback one last time
    boundCallback(bestParameters, true);

    return true;
}

// Returns a graph that contains vertices where predicate(vertex) = true
UndirectedGraphPtr Subgraph(const UndirectedGraph& srcGraph, const std::function<bool(size_t)>& predicate)
{
    UndirectedGraphPtr newGraphPtr = std::make_shared<UndirectedGraph>();
    UndirectedGraph& newGraph = *newGraphPtr;

    boost::property_map<UndirectedGraph, boost::vertex_color_t>::const_type srcGraphProperties =
        boost::get(boost::vertex_color_t(), srcGraph);

    boost::property_map<UndirectedGraph, boost::vertex_color_t>::type newGraphProperties =
        boost::get(boost::vertex_color_t(), newGraph);

    // Maps vertices in srcGraph to vertices in newGraph
    std::map<size_t, size_t> srcToNewVertex;

    // Transfer vertices
    UndirectedGraph::vertex_iterator currVertex, endVertex;

    for (boost::tie(currVertex, endVertex) = vertices(srcGraph); currVertex != endVertex; ++currVertex)
    {
        const size_t vertexIdInSrcGraph = *currVertex;

        if (predicate(vertexIdInSrcGraph))
        {
            const size_t vertexIdInNewGraph = boost::add_vertex(newGraph);

            SafeInsert(srcToNewVertex, vertexIdInSrcGraph, vertexIdInNewGraph);

            const size_t parameterIndex = boost::get(srcGraphProperties, vertexIdInSrcGraph);

            boost::put(newGraphProperties, vertexIdInNewGraph, parameterIndex);
        }
    }

    assert(!srcToNewVertex.empty());

    // Transfer edges
    UndirectedGraph::edge_iterator currEdge, endEdge;
    for (boost::tie(currEdge, endEdge) = edges(srcGraph); currEdge != endEdge; ++currEdge)
    {
        const size_t srcVertex = source(*currEdge, srcGraph);
        const size_t targetVertex = target(*currEdge, srcGraph);

        if (predicate(srcVertex) && predicate(targetVertex))
        {
            const size_t srcInInNewGraph = SafeLookup(srcToNewVertex, srcVertex);
            const size_t targetInInNewGraph = SafeLookup(srcToNewVertex, targetVertex);

            assert(srcInInNewGraph != targetInInNewGraph);

            boost::add_edge(srcInInNewGraph, targetInInNewGraph, newGraph);
        }
    }

    return newGraphPtr;
}

// Inserts 1 graph into multiple connected components
void DecomposeConnectedComponents(std::list<UndirectedGraphPtr>& outputList, const UndirectedGraph& srcGraph,
                                  const std::vector<size_t>& componentVector, const size_t connectedComponentCount)
{
    assert(connectedComponentCount > 1);

    boost::property_map<UndirectedGraph, boost::vertex_color_t>::const_type srcGraphProperties =
        boost::get(boost::vertex_color_t(), srcGraph);

    for (size_t componentIndex = 0; componentIndex < connectedComponentCount; componentIndex++)
    {
        const auto predicate = [&](const size_t vertexId)
        {
            assert(vertexId < componentVector.size());

            const size_t thisVertexComponentIndex = componentVector[vertexId];

            return (thisVertexComponentIndex == componentIndex);
        };

        outputList.push_back(Subgraph(srcGraph, predicate));
    }
}

size_t SelectVertexToRemove(const UndirectedGraph& srcGraph)
{
    // Check to see if the graph has articulation points
    // These are points that cause the graph to form more than 1 connected component
    // if the articulation point is removed
    std::vector<size_t> candidateVertices;
    boost::articulation_points(srcGraph, std::back_inserter(candidateVertices));

    if (candidateVertices.empty())
    {
        // No articulation point
        // Just search over all vertices
        candidateVertices.reserve(boost::num_vertices(srcGraph));

        UndirectedGraph::vertex_iterator currVertex, endVertex;

        for (boost::tie(currVertex, endVertex) = vertices(srcGraph); currVertex != endVertex; ++currVertex)
        {
            candidateVertices.push_back(*currVertex);
        }
    }

    assert(!candidateVertices.empty());

    // Choose a vertex of max degree
    boost::optional<size_t> maxDegreeVertex;
    size_t maxDegree = 0;

    for (const size_t vertex : candidateVertices)
    {
        const size_t degree = boost::degree(vertex, srcGraph);

        if (degree >= maxDegree)
        {
            maxDegreeVertex = vertex;
            maxDegree = degree;
        }
    }

    assert(maxDegreeVertex);
    return *maxDegreeVertex;
}

// Returns a graph that is srcGraph with 1 vertex removed
void RemoveVertex(std::list<UndirectedGraphPtr>& outputList, const UndirectedGraph& srcGraph,
                  const size_t vertexToRemove)
{
    // Generate a graph that contains all but vertexToRemove
    {
        const auto predicate = [&](const size_t vertexId) { return vertexId != vertexToRemove; };

        outputList.push_back(Subgraph(srcGraph, predicate));
    }

    // Generate a graph that contains only vertexToRemove
    {
        const auto predicate = [&](const size_t vertexId) { return vertexId == vertexToRemove; };

        outputList.push_back(Subgraph(srcGraph, predicate));
    }
}

std::set<size_t> GetParameterSet(const UndirectedGraph& srcGraph)
{
    std::set<size_t> result;

    boost::property_map<UndirectedGraph, boost::vertex_color_t>::const_type srcGraphProperties =
        boost::get(boost::vertex_color_t(), srcGraph);

    UndirectedGraph::vertex_iterator currVertex, endVertex;

    for (boost::tie(currVertex, endVertex) = vertices(srcGraph); currVertex != endVertex; ++currVertex)
    {
        const size_t vertexId = *currVertex;

        const size_t parameterIndex = boost::get(srcGraphProperties, vertexId);

        SafeInsert(result, parameterIndex);
    }

    return result;
}

void IntegerBranchAndBoundSparse(const std::vector<size_t>& parameterRanges,
                                 const ParameterInteractionGraph& interactions, const size_t maxWorkListSize,
                                 const BoundObjectiveFunction& boundCallback)
{
    const size_t parameterCount = parameterRanges.size();

    if (0 == parameterCount)
    {
        // Nothing to optimize
        const IntegerParameterVector emptyVec;

        boundCallback(emptyVec, true);

        return;
    }

    // Each entry in the work list is a graph
    // representing a set of parameters to optimize (and their interactions)
    std::list<UndirectedGraphPtr> workList;

    // Convert input interactions into a graph
    // This graph represents the full search space
    {
        UndirectedGraphPtr fullGraphPtr = std::make_shared<UndirectedGraph>();
        UndirectedGraph& fullGraph = *fullGraphPtr;

        auto fullGraphProperties = boost::get(boost::vertex_color_t(), fullGraph);

        for (size_t i = 0; i < parameterCount; i++)
        {
            boost::add_vertex(fullGraph);

            // vertex i is associated with parameter i
            boost::put(fullGraphProperties, i, i);
        }

        std::set<std::pair<size_t, size_t>> edges;

        for (const auto& p : interactions)
        {
            for (const size_t otherVertex : p.second)
            {
                if (otherVertex == p.first)
                {
                    continue;
                }

                // edges are always inserted with the lowest vertex specified first
                const size_t v1 = std::min(p.first, otherVertex);
                const size_t v2 = std::max(p.first, otherVertex);

                assert(v1 != v2);
                assert(v1 < v2);

                // Don't insert the same edge twice
                const auto insertResult = edges.insert(std::pair<size_t, size_t>(v1, v2));
                if (insertResult.second)
                {
                    boost::add_edge(v1, v2, fullGraph);
                }
            }
        }

        workList.push_back(fullGraphPtr);
    }

    // If (paramIndex, paramValue) exists in this amp
    // then paramValue is the optimal value for parameters[paramIndex]
    std::map<size_t, size_t> optimalValues;

    // If log(parameter space size) of a sub-graph is larger than this
    // then do not attempt to optimize the sub-graph, immediately decompose it
    size_t maxSearchSpaceSize = std::numeric_limits<size_t>::max();

    while (!workList.empty())
    {
        // Choose 1 graph from the work list
        UndirectedGraphPtr srcGraphPtr = workList.front();
        workList.pop_front();

        const UndirectedGraph& srcGraph = *srcGraphPtr;

        // See if the graph can be decomposed into subgraphs that
        // covers the same search space
        {
            std::vector<size_t> componentVector(boost::num_vertices(srcGraph));
            const size_t connectedComponentCount = boost::connected_components(srcGraph, componentVector.data());

            if (connectedComponentCount > 1)
            {
                // Replace the graph with 1 graph per connected component
                DecomposeConnectedComponents(workList, srcGraph, componentVector, connectedComponentCount);

                // Loop again
                continue;
            }
        }

        const auto boundCallbackWrapper = [&](const IntegerParameterVector& paramVector,
                                              const bool isOptimalPoint) -> ObjectiveFunctionBounds
        {
            if (isOptimalPoint)
            {
                for (size_t i = 0; i < paramVector.GetParameterCount(); i++)
                {
                    if (paramVector.IsParameterDefined(i))
                    {
                        const size_t parameterValue = paramVector.GetParameter(i);

                        SafeInsertIdempotent(optimalValues, i, parameterValue);
                    }
                }
            }

            // always pass false for 2nd parameter
            // final call to callback happens later
            return boundCallback(paramVector, false);
        };

        const std::set<size_t> parametersToOptimize = GetParameterSet(srcGraph);

        // Compute log(search space size)

        size_t logSearchSpaceSize = 0;

        for (const size_t parameterIndex : parametersToOptimize)
        {
            logSearchSpaceSize += parameterRanges[parameterIndex];
        }

        bool optimizeResult = false;

        // The second term is used to ensure that a small-enough sub-graph eventually will make it
        // to the optimization step
        if ((logSearchSpaceSize < maxSearchSpaceSize) || (1 == parametersToOptimize.size()))
        {
            optimizeResult = IntegerBranchAndBoundImpl(parameterRanges, boundCallbackWrapper, parametersToOptimize,
                                                       maxWorkListSize, optimalValues);

            if (!optimizeResult)
            {
                // Search space was too large to find a result
                // lower the upper-limit on search space size
                maxSearchSpaceSize = logSearchSpaceSize;
            }
        }

        if (optimizeResult)
        {
            // Found optimal values for this portion of the search space
            for (const size_t paramIndex : parametersToOptimize)
            {
                assert(optimalValues.end() != optimalValues.find(paramIndex));
            }
        }
        else
        {
            // Work list size was exceeded
            // Decompose this graph into multiple smaller graphs
            // This will rule out some portions of the search space

            // Select 1 vertex to remove from the search (the associated parameter will be optimized in isolation)
            const size_t vertexToRemove = SelectVertexToRemove(srcGraph);

            // Lookup the associated parameter index
            boost::property_map<UndirectedGraph, boost::vertex_color_t>::const_type srcGraphProperties =
                boost::get(boost::vertex_color_t(), srcGraph);

            const size_t parameterToRemove = boost::get(srcGraphProperties, vertexToRemove);

            // Decompose into 2 graphs
            // 1 graph only contains parameterToRemove, the other graph contains all remaining parameters
            RemoveVertex(workList, srcGraph, vertexToRemove);
        }
    }

    // Notify caller of final solution
    const CompressedIntegerVector::CodeBook codeBook = CompressedIntegerVector::CreateCodeBook(parameterRanges);

    IntegerParameterVector finalVector(codeBook);

    for (size_t i = 0; i < parameterRanges.size(); i++)
    {
        const size_t value = SafeLookup(optimalValues, i);

        finalVector.SetParameter(i, value);
    }

    const ObjectiveFunctionBounds finalBounds = boundCallback(finalVector, true);
}

void IntegerBranchAndBound(const std::vector<size_t>& parameterRanges, const BoundObjectiveFunction& boundCallback)
{
    const size_t parameterCount = parameterRanges.size();

    if (0 == parameterCount)
    {
        // Nothing to optimize
        const IntegerParameterVector emptyVec;

        boundCallback(emptyVec, true);

        return;
    }

    // Optimize all parameters
    std::set<size_t> parametersToOptimize;

    for (size_t i = 0; i < parameterCount; i++)
    {
        parametersToOptimize.insert(i);
    }

    // No values are fixed
    std::map<size_t, size_t> fixedParameterValues;

    // No max worklist size
    boost::optional<size_t> maxWorkListSize;

    const bool searchResult = IntegerBranchAndBoundImpl(parameterRanges, boundCallback, parametersToOptimize,
                                                        maxWorkListSize, fixedParameterValues);
    assert(searchResult);
}
