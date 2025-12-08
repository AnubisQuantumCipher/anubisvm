#!/bin/bash
# fix_rpath.sh: Remove duplicate RPATH entries from macOS binaries
#
# Usage: ./fix_rpath.sh <binary_path>
#
# This script removes duplicate LC_RPATH entries that cause dyld warnings
# on macOS. The GNAT/Alire toolchain sometimes adds duplicate RPATH entries
# during linking, which causes runtime failures.

set -e

if [ $# -ne 1 ]; then
    echo "Usage: $0 <binary_path>"
    exit 1
fi

BINARY="$1"

if [ ! -f "$BINARY" ]; then
    echo "Error: Binary not found: $BINARY"
    exit 1
fi

echo "Fixing RPATH for: $BINARY"

# Get all RPATH entries
RPATHS=$(otool -l "$BINARY" | grep -A 2 "LC_RPATH" | grep "path" | awk '{print $2}' || true)

if [ -z "$RPATHS" ]; then
    echo "No RPATH entries found"
    exit 0
fi

# Create temp file to track seen RPATHs
SEEN_FILE=$(mktemp)
trap "rm -f $SEEN_FILE" EXIT

# Remove duplicates (keep first occurrence)
echo "$RPATHS" | while IFS= read -r rpath; do
    if [ -n "$rpath" ]; then
        if grep -Fxq "$rpath" "$SEEN_FILE" 2>/dev/null; then
            echo "Removing duplicate RPATH: $rpath"
            install_name_tool -delete_rpath "$rpath" "$BINARY" 2>/dev/null || true
        else
            echo "$rpath" >> "$SEEN_FILE"
        fi
    fi
done

echo "RPATH fixed successfully"
