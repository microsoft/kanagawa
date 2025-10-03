#!/usr/bin/env python3
import argparse
import os
import re
import subprocess
import sys
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
    test_path = Path(args.testfile)

    if not test_path.is_file():
        print(f"File does not exist: {args.testfile}")
        sys.exit(1)

    # Clean up files from previous runs
    test_file_base = test_path.stem
    done_path = Path(f"{test_file_base}.done")
    error_path = Path(f"{test_file_base}.error")

    for p in (done_path, error_path):
        try:
            p.unlink(missing_ok=True)
        except Exception:
            pass

    lines = test_path.read_text(encoding='utf-8').splitlines()

    # Create subfolder based on test file base name
    subfolder = Path(test_file_base)
    subfolder.mkdir(exist_ok=True)

    temp_file_path = subfolder / TEMP_FILE_NAME

    imports = ''
    if args.helper:
        imports = f'import helper.{args.helper}'

    def write_temp_header():
        with temp_file_path.open('w', encoding='ascii') as f:
            if imports:
                f.write(imports + '\n')

    write_temp_header()

    # --- single failure path to remove duplication ---
    def fail(msg: str, *, result: subprocess.CompletedProcess | None = None, include_temp: bool = True):
        """
        Print msg, write .error file with stdout/stderr (when available) and
        include current temp file contents (when requested), then exit(1).
        """
        print(msg)

        with error_path.open('w', encoding='utf-8', newline='') as ef:
            ef.write(msg + '\n')
            if result is not None:
                ef.write(f"Stdout:\n{result.stdout}\n")
                ef.write(f"Stderr:\n{result.stderr}\n")
            ef.write('-' * 50 + '\n')
            if include_temp and temp_file_path.exists():
                ef.write(temp_file_path.read_text(encoding='ascii'))

        if include_temp and temp_file_path.exists():
            temp_contents = temp_file_path.read_text(encoding='ascii')
            print(f"----- {TEMP_FILE_NAME} contents -----\n{temp_contents}")

        sys.exit(1)
    # --------------------------------------------------

    for raw_line in lines:
        line = raw_line.strip()
        expected_error = 0
        expected_warning = ""

        error_match = re.search(r'expected:(\d*)', line)
        warning_match = re.search(r'warning:([^\s]*)', line)

        if error_match:
            expected_error = int(error_match.group(1) or 0)
        elif warning_match:
            expected_warning = warning_match.group(1)
        else:
            with temp_file_path.open('a', encoding='ascii') as f:
                f.write(line + '\n')
            continue

        internal_compiler_error = 0
        first_compile_error_code = 0
        first_compile_warning_code = ""

        # Build command
        cmd = [args.kanagawa]
        if args.options:
            cmd.extend(args.options.split())
        cmd.extend(['--place-iterations=1', '--import-dir', args.helperdir, TEMP_FILE_NAME])

        # Run test in subfolder
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, cwd=subfolder)
            output_lines = result.stdout.split('\n')

            for output_line in output_lines:
                print(output_line)

                if first_compile_error_code == 0:
                    m = re.search(r'Error (\d+)', output_line)
                    if m:
                        first_compile_error_code = int(m.group(1))

                if first_compile_warning_code == "":
                    wm = re.search(r'warning:.*\[--Wno-([^\]]*)\]$', output_line)
                    if wm:
                        first_compile_warning_code = wm.group(1)

                if output_line.strip() == "Call Stack:":
                    internal_compiler_error = 1

            exit_code = result.returncode

        except Exception as e:
            fail(f"Error running command: {e}", result=None, include_temp=True)

        if internal_compiler_error != 0:
            fail("Unexpected internal compiler error", result=result, include_temp=True)

        if expected_error != first_compile_error_code:
            fail(
                f"Unexpected result. Expected error: {expected_error} Actual: {first_compile_error_code}",
                result=result,
                include_temp=True
            )

        if expected_warning == "0":
            if first_compile_warning_code != "":
                fail(
                    f"Unexpected result. Expected no warning. Actual: '{first_compile_warning_code}'",
                    result=result,
                    include_temp=True
                )
        elif expected_warning != "" and expected_warning != first_compile_warning_code:
            fail(
                f"Unexpected result. Expected warning: '{expected_warning}' Actual: '{first_compile_warning_code}'",
                result=result,
                include_temp=True
            )

        if (expected_error != 0 and exit_code != 1) or (expected_error == 0 and exit_code != 0):
            fail(
                f"Failure! Expected compiler exit code {expected_error}, got {exit_code}",
                result=result,
                include_temp=True
            )

        # Reset temp file to header for the next block
        write_temp_header()

    print("Success!")
    done_path.touch()

if __name__ == "__main__":
    main()
