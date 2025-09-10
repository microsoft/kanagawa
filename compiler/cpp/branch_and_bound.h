// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

#pragma warning(push)
#pragma warning(disable : 4172)

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/biconnected_components.hpp>
#include <boost/graph/connected_components.hpp>
#include <boost/graph/graphviz.hpp>

#pragma warning(pop)

// boost re-defines assert()
// restore it here
#include "ship_assert.h"

// IntegerBranchAndBoundSparse is a generic minimizer.  It takes as input:
// * The number of parameters
// * The number of integer values that each parameter can attain [0, 1, ..., N-1]
// * A function that maps parameter vectors to [low, high] bounds.
// * An interaction graph.  Each vertex in the graph corresponds to one parameter.
//   If a pair of vertices in the graph are connected (directly or indirectly) then the corresponding parameters must be
//   optimized jointly to find the optimal value.
// * A maximum work list size, which bounds time and memory usage at the expense of not finding the optimal parameter
// vector.

// The parameter vectors passed to the callback function can be partially defined.  For example, the parameter vector:
// [ 3, X, X, 6, X ]
// Defines parameter 0 = 3 and parameter 3 = 6.

// The remaining parameter values are undefined.  In other words, this parameter vector represents a set of parameter
// vectors. The callback function returns bounds on the objective function value for that set.
// `ObjectiveFunctionBounds::_lowerBound` is an integer that is less than or equal to minimum objective function value
// for all parameter vectors in the set. `ObjectiveFunctionBounds::_upperBound` is an integer that is greater than or
// equal to objective function value of the _minimal parameter vector_ in the set. When the callback function is called
// with all parameters defined, then _lowerBound must be equal to _upperBound. The callback function must not have any
// side effects, because it can be called on multiple threads concurrently. The only exception is when the
// isOptimalPoint parameter is true, this call occurs in isolation once the optimal point has been located.

// The optimization process uses the following tricks:
// * Parameter vectors are stored in a compressed form (to reduce memory consumption)
// * The bounds returned by the callback are used to prune large regions of the search space.
// * Where possible, the interaction graph is decomposed into multiple connected components.  This divides a large
// optimization problem into several smaller optimization problems.
// * If the maximum work list size is exceeded during optimization, then vertices are removed from the interaction
// graph, to reduce the size of the search space.  The parameters corresponding to the vertices removed from the graph
// are optimized in isolation.

// Logically a vector of size_t
// Assumes a pre-computed code book to compress
// elements into smaller storage
class CompressedIntegerVector
{
  private:
    using word_t = uint64_t;

  public:
    struct CodeBookEntry
    {
        size_t _wordIndex;
        size_t _shift;
        size_t _mask;
        size_t _upperBound;
        bool _isConstantZero;
    };

    struct CodeBook
    {
        size_t _numWords;
        std::vector<CodeBookEntry> _entries;
    };

    // ranges[i] = N means that value i can take values in [0, N)
    static CodeBook CreateCodeBook(const std::vector<size_t>& ranges);

    CompressedIntegerVector();

    CompressedIntegerVector(const CodeBook& codeBook);

    void Set(const size_t index, const size_t value);

    size_t Get(const size_t index) const;

  private:
    const CodeBookEntry& GetCodeBookEntry(const size_t index) const;

    const CodeBook* _codeBook;
    std::vector<word_t> _words;
};

// A set of integer parameters
// each parameter may be defined or not
// if a parameter is defined, then it has an integer value
class IntegerParameterVector
{
  public:
    IntegerParameterVector() {}

    IntegerParameterVector(const CompressedIntegerVector::CodeBook& codeBook)
        : _data(codeBook), _valid(codeBook._entries.size(), false)
    {
    }

    inline size_t GetParameter(const size_t paramIndex) const
    {
        assert(paramIndex < _valid.size());

        assert(_valid[paramIndex]);

        return _data.Get(paramIndex);
    }

    inline bool IsParameterDefined(const size_t paramIndex) const
    {
        assert(paramIndex < _valid.size());

        return _valid[paramIndex];
    }

    inline size_t GetParameterCount() const { return _valid.size(); }

    inline void SetParameter(const size_t paramIndex, const size_t value)
    {
        assert(paramIndex < _valid.size());

        _valid[paramIndex] = true;
        _data.Set(paramIndex, value);
    }

    bool AllDefined() const
    {
        bool result = true;

        for (const bool valid : _valid)
        {
            if (!valid)
            {
                result = false;
                break;
            }
        }

        return result;
    }

  private:
    CompressedIntegerVector _data;
    std::vector<bool> _valid;
};

// Defines the possible range of objective function values
// for a given subset of the search space
// If V is the lowest objective function value in the search space
// then _lowerBound <= V <= _upperBound
struct ObjectiveFunctionBounds
{
    // Lowest possible objective function value
    size_t _lowerBound;

    // Highest possible objective function value (for the best point in the corresponding region)
    size_t _upperBound;
};

// Given values of (some) parameters
// Compute bounds of the objective function value
// at the minimal points in the search space
// defined by the parameters
// Can be called on multiple threads concurrently
// The second parameter is set to true during the final call to the function
// it is called with the parameters that produced the lowest objective function value
// The supplied integer parameter vector is valid only during the function call
// do not access it outside of the function call
//
// Note that when isOptimalPoint = false, there can be many concurrent calls to this callback (on multiple threads)
using BoundObjectiveFunction =
    std::function<ObjectiveFunctionBounds(const IntegerParameterVector&, const bool isOptimalPoint)>;

// parameterRanges[i] = N means that parameter i can take values in [0, N)
void IntegerBranchAndBound(const std::vector<size_t>& parameterRanges, const BoundObjectiveFunction& boundCallback);

// if ParameterInteractionGraph[i] = (j, k, l)
// then parameters i, j, k, l must be optimized jointly to find the optimal value
// no need to specify redundant edges like: ParameterInteractionGraph[j] = (j, k, l)
using ParameterInteractionGraph = std::map<size_t, std::set<size_t>>;

void IntegerBranchAndBoundSparse(const std::vector<size_t>& parameterRanges,
                                 const ParameterInteractionGraph& interactions, const size_t maxWorklistSize,
                                 const BoundObjectiveFunction& boundCallback);

// boost::vecS means that edges and vertices are stored in random-access vectors
// it also implies that vertex descriptors have type: size_t
// The "color" property indicates that each vertex has a size_t property associated with it called "vertex id"
// This property holds the associated parameter index
using UndirectedGraph =
    boost::adjacency_list<boost::vecS, boost::vecS, boost::undirectedS, boost::property<boost::vertex_color_t, size_t>>;
using UndirectedGraphPtr = std::shared_ptr<UndirectedGraph>;

// Inserts 1 graph into multiple connected components
void DecomposeConnectedComponents(std::list<UndirectedGraphPtr>& outputList, const UndirectedGraph& srcGraph,
                                  const std::vector<size_t>& componentVector, const size_t connectedComponentCount);