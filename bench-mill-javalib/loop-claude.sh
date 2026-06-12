while true; do git checkout accepted &&  ~/.local/bin/claude --effort max --dangerously-skip-permissions --verbose --output-format stream-json -p "$(cat bench-mill-javalib/prompt.md)"; done
