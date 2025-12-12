#!/bin/bash
# fix_rpath.sh: Remove duplicate RPATH entries from macOS binaries
#
# Usage: ./fix_rpath.sh <binary_path|directory>
#
# This script removes duplicate LC_RPATH entries that cause dyld failures
# on macOS 15.4+. The GNAT/Alire toolchain sometimes adds duplicate RPATH
# entries during linking, which causes runtime failures (exit code 134).

set -e

fix_binary() {
    local BINARY="$1"

    if [ ! -f "$BINARY" ]; then
        return 0
    fi

    # Get all RPATH entries
    local RPATHS=$(otool -l "$BINARY" 2>/dev/null | grep -A 2 "LC_RPATH" | grep "path" | awk '{print $2}' || true)

    if [ -z "$RPATHS" ]; then
        return 0
    fi

    # Create temp file to track seen RPATHs
    local SEEN_FILE=$(mktemp)
    local FIXED=0

    # Remove duplicates (keep first occurrence)
    echo "$RPATHS" | while IFS= read -r rpath; do
        if [ -n "$rpath" ]; then
            if grep -Fxq "$rpath" "$SEEN_FILE" 2>/dev/null; then
                echo "  Removing duplicate RPATH: $rpath"
                install_name_tool -delete_rpath "$rpath" "$BINARY" 2>/dev/null || true
                FIXED=1
            else
                echo "$rpath" >> "$SEEN_FILE"
            fi
        fi
    done

    rm -f "$SEEN_FILE"

    # Re-sign the binary (required on macOS)
    if [ $FIXED -eq 1 ]; then
        codesign -f -s - "$BINARY" 2>/dev/null || true
        echo "  Fixed and re-signed: $(basename "$BINARY")"
    fi
}

if [ $# -eq 0 ]; then
    # Default: fix all binaries in core/bin
    SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
    TARGET_DIR="$SCRIPT_DIR/../core/bin"
elif [ -d "$1" ]; then
    TARGET_DIR="$1"
elif [ -f "$1" ]; then
    echo "Fixing RPATH for: $1"
    fix_binary "$1"
    echo "Done."
    exit 0
else
    echo "Error: Not found: $1"
    exit 1
fi

echo "Fixing RPATH entries in: $TARGET_DIR"

for binary in "$TARGET_DIR"/*; do
    if [ -x "$binary" ] && [ ! -d "$binary" ]; then
        fix_binary "$binary"
    fi
done

echo "Done."
