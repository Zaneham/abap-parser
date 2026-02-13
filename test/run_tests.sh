#!/bin/bash
# Run all ABAP test files through the parser.
# If every file parses without crashing, print the Fab Four.
# If any file fails, print which one and exit in disgrace.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PARSER="opam exec -- dune exec abap-parse --root=$PROJECT_DIR --"
PASS=0
FAIL=0
FAILED_FILES=""

echo "=== ABAP Parser Test Suite ==="
echo ""

for f in "$SCRIPT_DIR"/*.abap; do
    name=$(basename "$f")
    printf "  %-30s" "$name"
    if $PARSER --parse "$f" > /dev/null 2>&1; then
        echo "OK"
        PASS=$((PASS + 1))
    else
        echo "FAIL"
        FAIL=$((FAIL + 1))
        FAILED_FILES="$FAILED_FILES $name"
    fi
done

echo ""
echo "--- Results: $PASS passed, $FAIL failed ---"

if [ "$FAIL" -gt 0 ]; then
    echo ""
    echo "Failed files:$FAILED_FILES"
    echo ""
    echo "No Beatles for you."
    exit 1
fi

echo ""
echo "All tests passed. The Fab Four:"
echo ""
echo "  John Lennon     - Rhythm Guitar, Vocals"
echo "  Paul McCartney  - Bass Guitar, Vocals"
echo "  George Harrison - Lead Guitar, Vocals"
echo "  Ringo Starr     - Drums, Vibes"
echo ""
echo "\"I get by with a little help from my parser.\""
