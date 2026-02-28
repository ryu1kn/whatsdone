#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
SCRIPT="$ROOT_DIR/tools/agent-loop/run.sh"

assert_contains() {
  local file="$1"
  local needle="$2"
  if ! grep -Fq "$needle" "$file"; then
    echo "Assertion failed: '$needle' not found in $file" >&2
    exit 1
  fi
}

make_fake_bin() {
  local fake_bin="$1"
  mkdir -p "$fake_bin"

  cat >"$fake_bin/codex" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
calls_file="${TEST_TMP_DIR}/codex_calls"
count=0
if [[ -f "$calls_file" ]]; then
  count="$(cat "$calls_file")"
fi
count=$((count+1))
printf "%s" "$count" >"$calls_file"

idx=0
prompt=""
for arg in "$@"; do
  idx=$((idx+1))
  printf "arg%d=%s\n" "$idx" "$arg" >>"${TEST_TMP_DIR}/codex_args_${count}.log"
  prompt="$arg"
done
printf "%s\n" "$prompt" >"${TEST_TMP_DIR}/prompt_${count}.txt"

if [[ "${TEST_SCENARIO}" == "success_first" ]]; then
  echo "RESULT: OK"
  echo "<promise>COMPLETE</promise>"
  exit 0
fi

if [[ "${TEST_SCENARIO}" == "gate_fail_then_pass" ]]; then
  if [[ "$count" -eq 1 ]]; then
    echo "RESULT: OK"
    exit 0
  fi
  echo "RESULT: OK"
  echo "<promise>COMPLETE</promise>"
  exit 0
fi

echo "Unsupported TEST_SCENARIO=${TEST_SCENARIO}" >&2
exit 1
EOF

  cat >"$fake_bin/mise" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
if [[ "$1" != "run" || "$2" != "gate" ]]; then
  echo "Unexpected mise invocation: $*" >&2
  exit 1
fi

calls_file="${TEST_TMP_DIR}/gate_calls"
count=0
if [[ -f "$calls_file" ]]; then
  count="$(cat "$calls_file")"
fi
count=$((count+1))
printf "%s" "$count" >"$calls_file"

if [[ "${TEST_SCENARIO}" == "success_first" ]]; then
  echo "gate ok"
  exit 0
fi

if [[ "${TEST_SCENARIO}" == "gate_fail_then_pass" ]]; then
  if [[ "$count" -eq 1 ]]; then
    echo "GATE BROKEN"
    exit 1
  fi
  echo "gate ok"
  exit 0
fi

echo "Unsupported TEST_SCENARIO=${TEST_SCENARIO}" >&2
exit 1
EOF

  cat >"$fake_bin/git" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
if [[ "$1" == "status" && "${2-}" == "--porcelain" ]]; then
  if [[ "${TEST_SCENARIO}" == "success_first" || "${TEST_SCENARIO}" == "gate_fail_then_pass" ]]; then
    exit 0
  fi
fi
echo "Unexpected git invocation: $*" >&2
exit 1
EOF

  chmod +x "$fake_bin/codex" "$fake_bin/mise" "$fake_bin/git"
}

run_case_success_first() {
  local case_tmp
  case_tmp="$(mktemp -d)"
  TEST_TMP_DIR="$case_tmp"
  TEST_SCENARIO="success_first"
  make_fake_bin "$case_tmp/bin"

  PATH="$case_tmp/bin:$PATH" TEST_TMP_DIR="$TEST_TMP_DIR" TEST_SCENARIO="$TEST_SCENARIO" \
    "$SCRIPT" --max-iterations 3 --model gpt-5.3-codex \
    >"$case_tmp/stdout.log" 2>"$case_tmp/stderr.log"

  assert_contains "$case_tmp/codex_args_1.log" "arg1=exec"
  assert_contains "$case_tmp/codex_args_1.log" "arg2=--model"
  assert_contains "$case_tmp/codex_args_1.log" "arg3=gpt-5.3-codex"
  assert_contains "$case_tmp/prompt_1.txt" 'Run $work-on-backlog now'
  assert_contains "$case_tmp/stdout.log" "<promise>COMPLETE</promise>"
}

run_case_success_without_model() {
  local case_tmp
  case_tmp="$(mktemp -d)"
  TEST_TMP_DIR="$case_tmp"
  TEST_SCENARIO="success_first"
  make_fake_bin "$case_tmp/bin"

  PATH="$case_tmp/bin:$PATH" TEST_TMP_DIR="$TEST_TMP_DIR" TEST_SCENARIO="$TEST_SCENARIO" \
    "$SCRIPT" --max-iterations 3 \
    >"$case_tmp/stdout.log" 2>"$case_tmp/stderr.log"

  assert_contains "$case_tmp/codex_args_1.log" "arg1=exec"
  if grep -Fq "arg2=--model" "$case_tmp/codex_args_1.log"; then
    echo "Assertion failed: --model should not be passed when omitted" >&2
    exit 1
  fi
  assert_contains "$case_tmp/prompt_1.txt" 'Run $work-on-backlog now'
  assert_contains "$case_tmp/stdout.log" "<promise>COMPLETE</promise>"
}

run_case_gate_failure_propagates() {
  local case_tmp
  case_tmp="$(mktemp -d)"
  TEST_TMP_DIR="$case_tmp"
  TEST_SCENARIO="gate_fail_then_pass"
  make_fake_bin "$case_tmp/bin"

  PATH="$case_tmp/bin:$PATH" TEST_TMP_DIR="$TEST_TMP_DIR" TEST_SCENARIO="$TEST_SCENARIO" \
    "$SCRIPT" --max-iterations 2 --model gpt-5.3-codex \
    >"$case_tmp/stdout.log" 2>"$case_tmp/stderr.log"

  assert_contains "$case_tmp/prompt_2.txt" "gate_failed_output:"
  assert_contains "$case_tmp/prompt_2.txt" "GATE BROKEN"
  assert_contains "$case_tmp/stdout.log" "<promise>COMPLETE</promise>"
}

run_case_extra_instruction_is_appended() {
  local case_tmp
  case_tmp="$(mktemp -d)"
  TEST_TMP_DIR="$case_tmp"
  TEST_SCENARIO="success_first"
  make_fake_bin "$case_tmp/bin"

  PATH="$case_tmp/bin:$PATH" TEST_TMP_DIR="$TEST_TMP_DIR" TEST_SCENARIO="$TEST_SCENARIO" \
    "$SCRIPT" --max-iterations 1 --extra-instruction "Focus on task splitting only when blocked." \
    >"$case_tmp/stdout.log" 2>"$case_tmp/stderr.log"

  assert_contains "$case_tmp/prompt_1.txt" "Extra instruction:"
  assert_contains "$case_tmp/prompt_1.txt" "Focus on task splitting only when blocked."
  assert_contains "$case_tmp/stdout.log" "<promise>COMPLETE</promise>"
}

run_case_success_first
run_case_success_without_model
run_case_gate_failure_propagates
run_case_extra_instruction_is_appended

echo "tools/agent-loop/run_test.sh: PASS"
