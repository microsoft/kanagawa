#!/bin/bash

# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

# This script is used to set-up a working build environment. Currently it
# supports Ubuntu 22.04 LTS.

# CMake 3.22
sudo apt install cmake

# GCC 11
sudo apt install g++

# Boost
sudo apt install libboost-all-dev

