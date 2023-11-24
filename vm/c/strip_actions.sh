#!/bin/bash
# Usage: ./strip_actions.sh input.peg > output.peg
sed -e ':a;N;$!ba; s/{[^}]*}//g' -e 's/  */ /g' "$1"
