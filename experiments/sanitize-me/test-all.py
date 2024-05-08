import argparse
import colorama
import concurrent.futures
import os
import subprocess
import time
from colorama import Fore, Style
from concurrent.futures import ThreadPoolExecutor

EXE = 'sanitize-me-sm_75.exe'

colorama.init()

#
# TODO:
#  * match specific errors in test list
#
#  * passing is exiting non-zero
#  * why is uninit-glb failing
#  * why is leak-glb failing


ALL_TESTS = [
  # (test-name, tool-to-use, string-to-match)
  # (t,tool,match)
  ##################
  # memcheck
  ("oob-glb-rd", "memcheck", [], "Invalid __global__ read"),
  ("oob-glb-wr", "memcheck", [], "Invalid __global__ write"),
  ("oob-shm-rd", "memcheck", [], "Invalid __shared__ read"),
  ("oob-shm-wr", "memcheck", [], "Invalid __shared__ write"),
  #
  ##################
  # memcheck
  # https://docs.nvidia.com/compute-sanitizer/ComputeSanitizer/index.html#device-side-allocation-checking
  ("leak-glb",   "memcheck", ["--leak-check","full"], "Leaked 256 bytes"),
  ##################
  # initcheck
  ("uninit-glb", "initcheck", [], "Uninitialized __global__ memory read"),
  # ("uninit-shm", "initcheck", [], "Uninitialized __shared__ memory read"), # ESCAPING
  #
  # ("race-glm", "racecheck", [], "Race reported"), # ESCAPING
  ("race-shm", "racecheck", [], "Race reported"),
  ]


def matches_filter(t):
  if not opts.filters:
    return True
  for f in opts.filters:
    if f in t:
      return True
  return False


def run_process_expect_pass(exe, args):
  if opts.verbosity >= 1:
    print(f'  % {exe} {" ".join(args)}')
  result = subprocess.run([exe] + args, capture_output=True, text=True)
  if opts.verbosity >= 1:
    # ...
    if opts.verbosity >= 2:
      print(result.stdout)
      print(result.stderr)
    if result.returncode != 0:
      print(f'  << exited {Fore.RED}{result.returncode}{Style.RESET_ALL}')
    else:
      print(f'  << exited {result.returncode}')
  return result


def parse_args():
  # https://docs.python.org/3/library/argparse.html
  parser = argparse.ArgumentParser(description='Runs sanitizer tests')
  parser.add_argument('-v', '-v1', action='store_const', const=1, dest='verbosity', default=0,
                      help='enables verbose output')
  parser.add_argument('-v2', action='store_const', const=2, dest='verbosity',
                      help='enables debug output')
  parser.add_argument('-j', action='store_const', const=8, default=1, dest='parallelism',
                      help='enables parallelism')
  parser.add_argument('-j1', action='store_const', const=1, dest='parallelism',
                      help='enables parallelism')
  parser.add_argument('filters', nargs='*', help='filters for tests to run')
  return parser.parse_args()

class TestResult:
  def __init__(self, status: str, message: str = ''):
    self.status = status
    self.message = message
  def passed(self): return self.status == 'passed'
  def failed(self): return self.status == 'failed'
  def skipped(self): return self.status == 'skipped'
  def str(self):
    if self.status == 'passed':
      return f'{Fore.GREEN}{self.status}{Style.RESET_ALL}'
    elif self.status == 'skipped':
      return f'{Fore.YELLOW}{self.status}{Style.RESET_ALL}'
    else:
      return f'{Fore.RED}{self.status}{Style.RESET_ALL} ({self.message})'
  def __str__(self): return str()

Passed = TestResult('passed')
Skipped = TestResult('skipped')
Failed = lambda message: TestResult('failed', message)

# returns (test-name, cmdline, deferred messages, result)
# This can run a separate thread
def run_test(t, tool, tool_args, match):
  args = ["--tool", tool] + tool_args + [EXE, t]
  cmdline = f'  % compute-sanitizer {" ".join(args)}'
  if matches_filter(t):
    vout = ""
    result = subprocess.run(["compute-sanitizer.bat"] + args, capture_output=True, text=True)
    # I now think compute saniziater exits whatever the victim exits,
    # In some cases memcheck causes enables sanitize-me to exit non-zero
    # My CUDA_API(..) macro detects the error and exits 1.
    # So we will stop checking the exit code and only match output.
    if match in result.stdout:
      res = Passed
    else:
      res = Failed("match not found in stdout")
    if res.failed() or opts.verbosity >= 2:
      vout += "ERR:\n" + result.stderr + "\n" + "OUT:\n" + result.stdout
    return (t, cmdline, vout, res)
  else:
    return (t, cmdline, "", Skipped)

def run_tests():
  test_results = []
  if opts.parallelism == 1:
    for (t,tool,tool_args,match) in ALL_TESTS:
      r = run_test(t, tool, tool_args, match)
      test_results.append(r)
  else:
    # run in parallel
    with ThreadPoolExecutor(max_workers=opts.parallelism) as executor:
      futures = {executor.submit(run_test, t, tool, tool_args, match): (t, tool, tool_args, match) for (t, tool, tool_args, match) in ALL_TESTS}
      for future in concurrent.futures.as_completed(futures):
        test_results.append(future.result())
  return test_results

if __name__ == '__main__':
  opts = parse_args()
  if opts.verbosity >= 2:
    print(opts)

  # Run foo.exe with the argument --list-tests
  # result = subprocess.run([EXE, '--list-tests'], capture_output=True, text=True)
  # tests = result.stdout.splitlines()
  # print(tests)

  # For each test t in tests, run sanitize-me-sm_75.exe with the argument t
  start_time = time.time()
  test_results = run_tests()
  elapsed_time = time.time() - start_time
  print(f"run_tests took {elapsed_time:.3f} seconds")

  # print(test_results)
  all_skipped = True
  for (t, cmdline, vout, result) in test_results:
    all_skipped &= result.skipped()
    print(f'{Fore.CYAN}{t:<24}{Style.RESET_ALL}  ', result.str())
    if result.failed():
      print(f'{Fore.RED}{cmdline}{Style.RESET_ALL}\n')
    elif opts.verbosity >= 1:
      print(Fore.LIGHTBLACK_EX + cmdline + Style.RESET_ALL, "\n")
    if opts.verbosity >= 2:
      print(Fore.LIGHTBLACK_EX + vout + Style.RESET_ALL, "\n")

  if all_skipped:
    print(f'{Fore.YELLOW}WARNING: patterns did not match any tests{Style.RESET_ALL}')
