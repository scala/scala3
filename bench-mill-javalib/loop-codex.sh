while true; do git checkout accepted && codex --dangerously-bypass-approvals-and-sandbox -c model_reasoning_effort=xhigh exec "$(cat bench-mill-javalib/prompt.md)"; done
