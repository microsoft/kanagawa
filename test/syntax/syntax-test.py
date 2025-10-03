#!/usr/bin/env python3
import argparse
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

def main():
    parser = argparse.ArgumentParser(description='Syntax tests for Kanagawa')
    parser.add_argument('-kanagawa', required=True, help='Path to Kanagawa executable')
    parser.add_argument('-testfile', required=True, help='Test file path')
    parser.add_argument('-helper', help='Helper module name')
    parser.add_argument('-helperdir', required=True, help='Helper directory path')
    parser.add_argument('-options', default='', help='Additional options (space-separated)')

    args = parser.parse_args()

    TEMP_FILE_NAME = "syntax_test_temp_file.k"

    if not os.path.isfile(args.testfile):
        print(f"File does not exist: {args.testfile}")
        sys.exit(1)

    # Clean up files from previous runs
    test_file_base = Path(args.testfile).stem
    done_file = test_file_base + ".done"
    error_file = test_file_base + ".error"

    if os.path.exists(done_file):
        os.remove(done_file)
    if os.path.exists(error_file):
        os.remove(error_file)

    with open(args.testfile, 'r') as f:
        lines = f.readlines()

    # Create subfolder based on test file base name
    subfolder = Path(test_file_base)
    subfolder.mkdir(exist_ok=True)

    temp_file_name = subfolder / TEMP_FILE_NAME

    imports = ''
    if args.helper:
        imports = f'import helper.{args.helper}'

    with open(temp_file_name, 'w', encoding='ascii') as f:
        if imports:
            f.write(imports + '\n')

    for line in lines:
        line = line.strip()
        expected_error = 0
        expected_warning = ""

        error_match = re.search(r'expected:(\d*)', line)
        warning_match = re.search(r'warning:([^\s]*)', line)

        if error_match:
            expected_error = int(error_match.group(1))
        elif warning_match:
            expected_warning = warning_match.group(1)
        else:
            with open(temp_file_name, 'a', encoding='ascii') as f:
                f.write(line + '\n')
            continue

        internal_compiler_error = 0
        first_compile_error_code = 0
        first_compile_warning_code = ""

        # Build command
        cmd = [args.kanagawa]
        if args.options:
            # Split the options string and add to command
            option_list = args.options.split()
            cmd.extend(option_list)
        cmd.extend(['--place-iterations=1', '--import-dir', args.helperdir, TEMP_FILE_NAME])

        # Run test in subfolder
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, cwd=subfolder)
            output_lines = result.stdout.split('\n')

            for output_line in output_lines:
                print(output_line)

                if first_compile_error_code == 0:
                    error_match = re.search(r'Error (\d+)', output_line)
                    if error_match:
                        first_compile_error_code = int(error_match.group(1))

                if first_compile_warning_code == "":
                    warning_match = re.search(r'warning:.*\[--Wno-([^\]]*)\]$', output_line)
                    if warning_match:
                        first_compile_warning_code = warning_match.group(1)

                if output_line.strip() == "Call Stack:":
                    internal_compiler_error = 1

            exit_code = result.returncode

        except Exception as e:
            error_msg = f"Error running command: {e}"
            print(error_msg)

            # Write error to .error file
            error_file_name = Path(args.testfile).stem + ".error"
            with open(error_file_name, 'w') as error_file:
                error_file.write(error_msg + '\n')
                error_file.write('-' * 50 + '\n')
                if temp_file_name.exists():
                    with open(temp_file_name, 'r') as temp_file:
                        error_file.write(temp_file.read())

            # Print temp file contents
            if temp_file_name.exists():
                with open(temp_file_name, 'r') as temp_file:
                    temp_contents = temp_file.read()
                print(f"----- {TEMP_FILE_NAME} contents -----\n{temp_contents}")

            sys.exit(1)

        if internal_compiler_error != 0:
            error_msg = "Unexpected internal compiler error"
            print(error_msg)

            # Write error to .error file
            error_file_name = Path(args.testfile).stem + ".error"
            with open(error_file_name, 'w') as error_file:
                error_file.write(error_msg + '\n')
                error_file.write(f"Stdout:\n{result.stdout}\n")
                error_file.write(f"Stderr:\n{result.stderr}\n")
                error_file.write('-' * 50 + '\n')
                if temp_file_name.exists():
                    with open(temp_file_name, 'r') as temp_file:
                        error_file.write(temp_file.read())

            # Print temp file contents
            if temp_file_name.exists():
                with open(temp_file_name, 'r') as temp_file:
                    temp_contents = temp_file.read()
                print(f"----- {TEMP_FILE_NAME} contents -----\n{temp_contents}")

            sys.exit(1)

        if expected_error != first_compile_error_code:
            error_msg = f"Unexpected result. Expected error: {expected_error} Actual: {first_compile_error_code}"
            print(error_msg)

            # Write error to .error file
            error_file_name = Path(args.testfile).stem + ".error"
            with open(error_file_name, 'w') as error_file:
                error_file.write(error_msg + '\n')
                error_file.write(f"Stdout:\n{result.stdout}\n")
                error_file.write(f"Stderr:\n{result.stderr}\n")
                error_file.write('-' * 50 + '\n')
                if temp_file_name.exists():
                    with open(temp_file_name, 'r') as temp_file:
                        error_file.write(temp_file.read())

            # Print temp file contents
            if temp_file_name.exists():
                with open(temp_file_name, 'r') as temp_file:
                    temp_contents = temp_file.read()
                print(f"----- {TEMP_FILE_NAME} contents -----\n{temp_contents}")

            sys.exit(1)

        if expected_warning == "0":
            if first_compile_warning_code != "":
                error_msg = f"Unexpected result. Expected no warning. Actual: '{first_compile_warning_code}'"
                print(error_msg)

                # Write error to .error file
                error_file_name = Path(args.testfile).stem + ".error"
                with open(error_file_name, 'w') as error_file:
                    error_file.write(error_msg + '\n')
                    error_file.write(f"Stdout:\n{result.stdout}\n")
                    error_file.write(f"Stderr:\n{result.stderr}\n")
                    error_file.write('-' * 50 + '\n')
                    if temp_file_name.exists():
                        with open(temp_file_name, 'r') as temp_file:
                            error_file.write(temp_file.read())

                # Print temp file contents
                if temp_file_name.exists():
                    with open(temp_file_name, 'r') as temp_file:
                        temp_contents = temp_file.read()
                    print(f"----- {TEMP_FILE_NAME} contents -----\n{temp_contents}")

                sys.exit(1)
        elif expected_warning != "" and expected_warning != first_compile_warning_code:
            error_msg = f"Unexpected result. Expected warning: '{expected_warning}' Actual: '{first_compile_warning_code}'"
            print(error_msg)

            # Write error to .error file
            error_file_name = Path(args.testfile).stem + ".error"
            with open(error_file_name, 'w') as error_file:
                error_file.write(error_msg + '\n')
                error_file.write(f"Stdout:\n{result.stdout}\n")
                error_file.write(f"Stderr:\n{result.stderr}\n")
                error_file.write('-' * 50 + '\n')
                if temp_file_name.exists():
                    with open(temp_file_name, 'r') as temp_file:
                        error_file.write(temp_file.read())

            # Print temp file contents
            if temp_file_name.exists():
                with open(temp_file_name, 'r') as temp_file:
                    temp_contents = temp_file.read()
                print(f"----- {TEMP_FILE_NAME} contents -----\n{temp_contents}")

            sys.exit(1)

        if (expected_error != 0 and exit_code != 1) or (expected_error == 0 and exit_code != 0):
            error_msg = f"Failure! Expected compiler exit code {expected_error}, got {exit_code}"
            print(error_msg)

            # Write error to .error file
            error_file_name = Path(args.testfile).stem + ".error"
            with open(error_file_name, 'w') as error_file:
                error_file.write(error_msg + '\n')
                error_file.write(f"Stdout:\n{result.stdout}\n")
                error_file.write(f"Stderr:\n{result.stderr}\n")
                error_file.write('-' * 50 + '\n')
                if temp_file_name.exists():
                    with open(temp_file_name, 'r') as temp_file:
                        error_file.write(temp_file.read())

            # Print temp file contents
            if temp_file_name.exists():
                with open(temp_file_name, 'r') as temp_file:
                    temp_contents = temp_file.read()
                print(f"----- {TEMP_FILE_NAME} contents -----\n{temp_contents}")

            sys.exit(1)

        with open(temp_file_name, 'w', encoding='ascii') as f:
            if imports:
                f.write(imports + '\n')

    print("Success!")

    done_file = Path(args.testfile).stem + ".done"
    Path(done_file).touch()

if __name__ == "__main__":
    main()
