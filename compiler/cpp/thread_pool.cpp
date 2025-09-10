// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#include "pch.h"

ThreadPool::ThreadPool() : _shouldExit(false), _tasksEnqueued(0), _tasksCompleted(0)
{
    const size_t threadCount = std::thread::hardware_concurrency();
    assert(threadCount > 0);

    for (size_t i = 0; i < threadCount; i++)
    {
        _threads.push_back(std::thread([this]() { ThreadFunction(); }));
    }
}

ThreadPool::~ThreadPool()
{
    // Wait for all work to complete
    WaitForIdle();

    // Notify all threads that they should exit
    {
        std::unique_lock<std::mutex> lock(_lock);

        _shouldExit = true;
    }

    _cvToThreads.notify_all();

    // Wait for all threads to terminate
    for (std::thread& thread : _threads)
    {
        thread.join();
    }
}

void ThreadPool::WaitForIdle()
{
    {
        std::unique_lock<std::mutex> lock(_lock);

        _cvFromThreads.wait(lock, [this]() { return _tasksEnqueued == _tasksCompleted; });

        assert(_taskQueue.empty());

        if (_errorString)
        {
            const std::string errorString = *_errorString;

            // Each error is only thrown once
            _errorString = boost::optional<std::string>();

            throw std::runtime_error(errorString);
        }
    }
}

void ThreadPool::ThreadFunction()
{
    while (true)
    {
        boost::optional<TaskCallback> task;

        {
            std::unique_lock<std::mutex> lock(_lock);

            if (_taskQueue.empty())
            {
                if (_shouldExit)
                {
                    // all tasks are exhausted, and the thread pool is terminating
                    return;
                }
                else
                {
                    // Wait for state to change
                    _cvToThreads.wait(lock);
                }
            }
            else
            {
                task = _taskQueue.front();

                _taskQueue.pop_front();
            }
        }

        if (task)
        {
            boost::optional<std::string> errorString;

            try
            {
                (*task)();
            }
            catch (std::exception& e)
            {
                // Record error string so that exception can be propagated back
                errorString = e.what();
            }

            {
                std::unique_lock<std::mutex> lock(_lock);

                if (errorString && !_errorString)
                {
                    _errorString = errorString;
                }

                _tasksCompleted++;
            }

            _cvFromThreads.notify_one();
        }
    }
}

void ThreadPool::Async(const TaskCallback& callback)
{
    {
        std::unique_lock<std::mutex> lock(_lock);

        _taskQueue.push_back(callback);

        _tasksEnqueued++;
    }

    _cvToThreads.notify_one();
}
