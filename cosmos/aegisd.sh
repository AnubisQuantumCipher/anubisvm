#!/bin/bash
# AegisVM Daemon Wrapper Script
# Sets required library paths for SPARK/Ada runtime

export DYLD_LIBRARY_PATH=/Users/sicarii/anubisvm/core/lib:/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib

exec /Users/sicarii/anubisvm/cosmos/aegisd "$@"
