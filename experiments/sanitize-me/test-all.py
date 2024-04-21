import os
import subprocess
import os
import subprocess

exe = 'sanitize-me-sm_75.exe'

def run_process(exe, args):
  print(f'Running: {exe} {" ".join(args)}')
  result = subprocess.run([exe] + args, capture_output=True, text=True)
  return result

if __name__ == '__main__':
  # Run foo.exe with the argument --list-tests
  result = subprocess.run([exe, '--list-tests'], capture_output=True, text=True)

  # Convert the lines of stdout to a list of strings
  tests = result.stdout.splitlines()
  print(tests)

  # For each test t in tests, run sanitize-me-sm_75.exe with the argument t
  test_results = []
  for t in tests:
#    result = subprocess.run([exe, t], capture_output=True, text=True)
    result = run_process("compute-sanitizer.bat", ["--tool", "memcheck", t])
    test_results.append((t, result.returncode))
#    print(t)
#    print(result.stdout)
#    print(result.stderr)
#    print(result.returncode)

  for (t, code) in test_results:
    print(f'{t:<24}{code:>8}')

