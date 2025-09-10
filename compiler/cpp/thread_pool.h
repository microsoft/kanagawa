// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#pragma once

class ThreadPool
{
  public:
    using TaskCallback = std::function<void()>;

    ThreadPool();

    // Blocks until all tasks have completed and all threads have terminated
    ~ThreadPool();

    // Block until all outstanding tasks have been executed
    void WaitForIdle();

    // Execute the specific callback (eventually)
    void Async(const TaskCallback& callback);

    // Calls the callback function "count" times
    // Each invocation of callback can append results to a list
    template <typename T>
    void MapToList(std::list<T>& resultList, const size_t count,
                   const std::function<void(std::list<T>&, const size_t)>& callback);

  private:
    void ThreadFunction();

    std::list<std::thread> _threads;

    std::mutex _lock;

    std::condition_variable _cvToThreads;

    std::condition_variable _cvFromThreads;

    bool _shouldExit;

    std::list<TaskCallback> _taskQueue;

    size_t _tasksEnqueued;

    size_t _tasksCompleted;

    boost::optional<std::string> _errorString;
};

template <typename T>
void ThreadPool::MapToList(std::list<T>& resultList, const size_t count,
                           const std::function<void(std::list<T>&, const size_t)>& callback)
{
    const size_t threadCount = _threads.size();

    const size_t elementsPerThread = (count + threadCount - 1) / threadCount;

    std::vector<std::list<T>> subLists(threadCount);

    for (size_t startElement = 0; startElement < count; startElement += elementsPerThread)
    {
        const size_t endElement = std::min(startElement + elementsPerThread, count);

        assert(endElement > startElement);

        const size_t tid = startElement / elementsPerThread;

        assert(tid < subLists.size());

        Async(
            [startElement, endElement, callback, this, tid, &subLists]()
            {
                // Accumulate into subLists[tid]
                std::list<T>& subList = subLists[tid];

                assert(subList.empty());

                for (size_t i = startElement; i < endElement; i++)
                {
                    callback(subList, i);
                }
            });
    }

    WaitForIdle();

    // Combine all lists
    for (std::list<T>& subList : subLists)
    {
        resultList.splice(resultList.end(), subList);
    }
}