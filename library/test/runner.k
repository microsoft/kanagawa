// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*|

The module is intended for running unit test in a simulator. It defines an
entry point `main` which is called at reset and calls user defined function
called `test_main`.

Verbosity of the test framework output is controlled by boolean value `verbose`
that can be defined via compiler `--define` command line option. With verbose
output the framework prints information about all executed tests. By default
only information about failed test case(s) is printed.

*/

module test.runner
    {
    }

class main
{
public:
    [[max_threads(1)]]
    void run()
    {
        test_main();
    }
}

[[name("_test_runner_main")]]
export main;
