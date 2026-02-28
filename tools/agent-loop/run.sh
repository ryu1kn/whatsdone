#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  ./tools/agent-loop/run.sh --max-iterations <n> [--model <model>] [--extra-instruction "<text>"]

Flags:
  --max-iterations   Required. Positive integer iteration cap.
  --model            Optional. Model name passed to codex exec.
  --extra-instruction Optional. Extra one-off instruction appended to the prompt.
EOF
}

MAX_ITERATIONS=""
MODEL=""
EXTRA_INSTRUCTION=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --max-iterations)
      MAX_ITERATIONS="${2-}"
      shift 2
      ;;
    --model)
      MODEL="${2-}"
      shift 2
      ;;
    --extra-instruction)
      EXTRA_INSTRUCTION="${2-}"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [[ -z "$MAX_ITERATIONS" ]]; then
  echo "Missing required flags." >&2
  usage >&2
  exit 2
fi

if ! [[ "$MAX_ITERATIONS" =~ ^[1-9][0-9]*$ ]]; then
  echo "--max-iterations must be a positive integer." >&2
  exit 2
fi

build_prompt() {
  local iteration="$1"
  local max_iterations="$2"
  local diagnostics="$3"
  local extra_instruction="$4"

  cat <<EOF
You are running a deterministic backlog loop iteration.
Run \$work-on-backlog now and execute exactly one safe backlog iteration.

Iteration:
$iteration of $max_iterations

If the previous iteration had failures, address them first.

Previous iteration diagnostics (must fix before new scope):
$diagnostics
EOF

  if [[ -n "$extra_instruction" ]]; then
    cat <<EOF

Extra instruction:
$extra_instruction
EOF
  fi
}

diagnostics="none"
tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

for ((i=1; i<=MAX_ITERATIONS; i++)); do
  prompt="$(build_prompt "$i" "$MAX_ITERATIONS" "$diagnostics" "$EXTRA_INSTRUCTION")"

  codex_output_file="$tmp_dir/codex-${i}.log"
  set +e
  if [[ -n "$MODEL" ]]; then
    codex exec --model "$MODEL" "$prompt" >"$codex_output_file" 2>&1
  else
    codex exec "$prompt" >"$codex_output_file" 2>&1
  fi
  codex_exit=$?
  set -e
  codex_output="$(cat "$codex_output_file")"

  gate_output_file="$tmp_dir/gate-${i}.log"
  set +e
  mise run gate >"$gate_output_file" 2>&1
  gate_exit=$?
  set -e
  gate_output="$(cat "$gate_output_file")"

  git_status_output="$(git status --porcelain || true)"

  has_complete_promise=1
  if grep -Fq "<promise>COMPLETE</promise>" "$codex_output_file"; then
    has_complete_promise=0
  fi

  repo_clean=1
  if [[ -z "$git_status_output" ]]; then
    repo_clean=0
  fi

  if [[ $has_complete_promise -eq 0 && $gate_exit -eq 0 && $repo_clean -eq 0 ]]; then
    cat "$codex_output_file"
    exit 0
  fi

  diagnostics="codex_exit=$codex_exit"$'\n'
  diagnostics+="codex_output:"$'\n'"$codex_output"$'\n'
  if [[ $gate_exit -ne 0 ]]; then
    diagnostics+="gate_failed_output:"$'\n'"$gate_output"$'\n'
  fi
  if [[ $repo_clean -ne 0 ]]; then
    diagnostics+="git_status_porcelain:"$'\n'"$git_status_output"$'\n'
  fi
done

echo "Reached max iterations (${MAX_ITERATIONS}) without satisfying completion criteria." >&2
exit 1
