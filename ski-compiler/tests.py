#!/usr/bin/env python3
import subprocess
import os

# Directorio base del proyecto
BASEDIR = "../"

TESTS = [
    {
        "language": "lambda",
        "source": os.path.join(BASEDIR, "program-samples/lambda/saludo.lam"),
        "cases": [
            {"args": ["/Juan"], "expected": "Hola Juan como estás?"},
            {"args": ["/Maria"], "expected": "Hola Maria como estás?"}
        ]
    },
    {
        "language": "ski",
        "source": os.path.join(BASEDIR, "program-samples/ski/second.ski"),
        "cases": [
            {"args": ["/first", "/second"], "expected": "second"}
        ]
    },
    {
        "language": "subs",
        "source": os.path.join(BASEDIR, "program-samples/subs/numbers.subs"),
        "cases": [
            {
                "args": ["2", "/a", "/b"],
                "expected": "a" * (2 ** 10) + "b"
            }
        ]
    },
    {
        "language": "subs",
        "source": os.path.join(BASEDIR, "program-samples/subs/twenty.subs"),
        "cases": [
            {
                "args": ["/a", "/b"],
                "expected": "a"*20 + "b"
            }
        ]
    }
]

MODES = ["strict", "lazy"]
BUILD_DIR = "compiler-build"
TIMEOUT = 5

def run_cmd(cmd, timeout=None, capture=True):
    print(f"\n[EJECUTANDO] {cmd}")
    try:
        if capture:
            result = subprocess.run(
                cmd,
                text=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                shell=True,
                timeout=timeout
            )
            if result.stdout:
                print("[STDOUT]", result.stdout)
            if result.stderr:
                print("[STDERR]", result.stderr)
            return result.stdout.strip(), result.stderr.strip(), result.returncode
        else:
            result = subprocess.run(
                cmd,
                text=True,
                shell=True,
                timeout=timeout
            )
            return "", "", result.returncode
    except subprocess.TimeoutExpired:
        print("[TIMEOUT]")
        return "", "Timeout", -1

def cabal_build():
    _, _, code = run_cmd("cabal build", capture=False)
    return code == 0

def compile_program(language, mode, source, output_name):
    cmd = f"cabal run {language}-compiler-{mode} -- -o {output_name} < {source}"
    _, _, code = run_cmd(cmd, capture=False)
    return code == 0

def run_program(executable, args):
    exe_path = os.path.join(BUILD_DIR, executable)
    cmd = f"{exe_path} {' '.join(args)}"
    out, _, code = run_cmd(cmd, timeout=TIMEOUT, capture=True)
    return out, code

def main():
    results = []

    cabal_ok = cabal_build()
    for test in TESTS:
        language = test["language"]
        source = test["source"]
        for mode in MODES:
            exe_name = f"{language}-{mode}-test"
            label = f"{os.path.basename(source)} [{mode}]"

            cabal_mark = "✅" if cabal_ok else "❌"

            if cabal_ok and compile_program(language, mode, source, exe_name):
                compile_mark = "✅"
            else:
                compile_mark = "❌"
                results.append((label, cabal_mark, compile_mark, ""))
                continue

            exec_mark = "✅"
            for case in test["cases"]:
                out, code = run_program(exe_name, case["args"])
                if code == -1:  # timeout
                    exec_mark = "⏱️"
                    break
                if case["expected"] is not None:
                    if code != 0:
                        exec_mark = "💥"  # error exit
                        break
                    if out != case["expected"]:
                        exec_mark = "❌"  # wrong output
                        break
                else:
                    if code != 0:
                        exec_mark = "💥"
                        break

            results.append((label, cabal_mark, compile_mark, exec_mark))

    # Print summary
    print("\n=== RESUMEN DE PRUEBA ===")
    print("Programa [modo]           | Cabal build  |   Compilación   | Ejecución")
    print("--------------------------+--------------+-----------------+-----------")
    for label, cabal_mark, compile_mark, exec_mark in results:
        print(f"{label:<25} | {cabal_mark:^11} | {compile_mark:^14} | {exec_mark:^9}")

if __name__ == "__main__":
    main()
