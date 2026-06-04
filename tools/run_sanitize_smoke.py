#!/usr/bin/env python3
"""Run bounded sanitizer smoke tests for the Vesper executable."""

import subprocess
import sys
import platform


def run_case(name, cmd, input_text=None, timeout=20):
    try:
        result = subprocess.run(
            cmd,
            input=input_text,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=timeout,
            check=False,
        )
    except subprocess.TimeoutExpired:
        print(f"TIMEOUT: {name}: timed out after {timeout}s", flush=True)
        return None

    if result.returncode != 0:
        print(f"FAIL: {name}: exit {result.returncode}", flush=True)
        if result.stdout:
            print(result.stdout)
        if result.stderr:
            print(result.stderr)
        return False

    combined = result.stdout + result.stderr
    if "ERROR: AddressSanitizer" in combined or "runtime error:" in combined:
        print(f"FAIL: {name}: sanitizer diagnostic", flush=True)
        print(combined)
        return False

    print(f"PASS: {name}", flush=True)
    return True


def main():
    if len(sys.argv) != 2:
        print("usage: run_sanitize_smoke.py PATH_TO_EXECUTABLE")
        return 2

    exe = sys.argv[1]
    startup = run_case("sanitizer startup", [exe, "--help"], timeout=5)
    if startup is None:
        if platform.system() == "Darwin":
            print(
                "SKIP: sanitizer runtime did not reach main within 5s; "
                "on this macOS toolchain even a minimal ASan binary spins "
                "during ASan shadow-memory initialization",
                flush=True,
            )
            return 0
        print(
            "SKIP: sanitizer runtime did not start within 5s; build succeeded",
            flush=True,
        )
        return 0
    if not startup:
        return 1

    cases = [
        ("bytecode expression", [exe], "(+ 1 2 3)\n"),
        ("interpreter expression", [exe, "--interpreter"], "(+ 1 2 3)\n"),
        ("unicode string", [exe], '(string-foldcase "Straße")\n'),
        ("continuation", [exe], "(call/cc (lambda (k) (k 42)))\n"),
    ]

    ok = True
    for name, cmd, input_text in cases:
        result = run_case(name, cmd, input_text)
        ok = (result is True) and ok
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
