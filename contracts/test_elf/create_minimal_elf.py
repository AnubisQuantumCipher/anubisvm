#!/usr/bin/env python3
"""
Create a minimal ELF64 binary for testing SPHINX native execution.

This creates a valid ELF64 AArch64 binary with:
- Proper ELF header
- Section headers (.text, .data, .shstrtab)
- Minimal executable code (just returns 0)

The binary passes ELF validation but won't execute natively on macOS
(since macOS uses Mach-O). This is for testing the ELF loading path.
"""

import struct
import sys

# ELF64 Constants
ELFMAG = b'\x7fELF'
ELFCLASS64 = 2
ELFDATA2LSB = 1  # Little endian
EV_CURRENT = 1
ELFOSABI_NONE = 0
ET_EXEC = 2  # Executable
EM_AARCH64 = 0xB7  # ARM64
EM_X86_64 = 0x3E
EM_RISCV = 0xF3

# Section types
SHT_NULL = 0
SHT_PROGBITS = 1
SHT_STRTAB = 3
SHT_NOBITS = 8

# Section flags
SHF_WRITE = 1
SHF_ALLOC = 2
SHF_EXECINSTR = 4

def create_elf64_arm64():
    """Create a minimal ELF64 binary for ARM64."""

    # Code section: just RET instruction (0xD65F03C0)
    code = bytes([0xC0, 0x03, 0x5F, 0xD6])  # ARM64: RET

    # Data section
    data = bytes([0x48, 0x45, 0x4C, 0x4C, 0x4F])  # "HELLO"

    # Section header string table
    shstrtab = b'\x00.text\x00.data\x00.shstrtab\x00'

    # Calculate offsets
    ehdr_size = 64   # ELF64 header size
    shdr_size = 64   # Section header entry size
    num_sections = 4  # NULL + .text + .data + .shstrtab

    # Sections start after header
    text_offset = ehdr_size
    data_offset = text_offset + len(code)
    shstrtab_offset = data_offset + len(data)

    # Pad to alignment for section headers
    shdr_offset = (shstrtab_offset + len(shstrtab) + 7) & ~7

    # Entry point (virtual address where code starts)
    entry_point = 0x400000 + text_offset

    # Build ELF header (64 bytes)
    ehdr = struct.pack('<4sBBBBBxxxxxxx',
        ELFMAG,          # e_ident[0:4] - magic
        ELFCLASS64,      # e_ident[4] - 64-bit
        ELFDATA2LSB,     # e_ident[5] - little endian
        EV_CURRENT,      # e_ident[6] - version
        ELFOSABI_NONE,   # e_ident[7] - OS/ABI
        0                # e_ident[8] - ABI version
    )
    ehdr += struct.pack('<HHIQQQIHHHHHH',
        ET_EXEC,         # e_type - executable
        EM_AARCH64,      # e_machine - ARM64
        EV_CURRENT,      # e_version
        entry_point,     # e_entry - entry point
        0,               # e_phoff - no program headers (simplified)
        shdr_offset,     # e_shoff - section header offset
        0,               # e_flags
        ehdr_size,       # e_ehsize
        0,               # e_phentsize
        0,               # e_phnum
        shdr_size,       # e_shentsize
        num_sections,    # e_shnum
        3                # e_shstrndx - index of .shstrtab
    )

    # Build section headers
    shdrs = b''

    # Section 0: NULL section (required)
    shdrs += struct.pack('<IIQQQQIIQQ',
        0, SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0)

    # Section 1: .text (code)
    text_name_offset = 1  # Index into shstrtab
    shdrs += struct.pack('<IIQQQQIIQQ',
        text_name_offset,        # sh_name
        SHT_PROGBITS,            # sh_type
        SHF_ALLOC | SHF_EXECINSTR,  # sh_flags
        entry_point,             # sh_addr
        text_offset,             # sh_offset
        len(code),               # sh_size
        0,                       # sh_link
        0,                       # sh_info
        4,                       # sh_addralign
        0                        # sh_entsize
    )

    # Section 2: .data
    data_name_offset = 7  # Index into shstrtab
    shdrs += struct.pack('<IIQQQQIIQQ',
        data_name_offset,        # sh_name
        SHT_PROGBITS,            # sh_type
        SHF_ALLOC | SHF_WRITE,   # sh_flags
        0x500000,                # sh_addr
        data_offset,             # sh_offset
        len(data),               # sh_size
        0,                       # sh_link
        0,                       # sh_info
        1,                       # sh_addralign
        0                        # sh_entsize
    )

    # Section 3: .shstrtab
    shstrtab_name_offset = 13  # Index into shstrtab
    shdrs += struct.pack('<IIQQQQIIQQ',
        shstrtab_name_offset,    # sh_name
        SHT_STRTAB,              # sh_type
        0,                       # sh_flags
        0,                       # sh_addr
        shstrtab_offset,         # sh_offset
        len(shstrtab),           # sh_size
        0,                       # sh_link
        0,                       # sh_info
        1,                       # sh_addralign
        0                        # sh_entsize
    )

    # Assemble the binary
    binary = ehdr + code + data + shstrtab

    # Pad to section header offset
    while len(binary) < shdr_offset:
        binary += b'\x00'

    binary += shdrs

    return binary


def main():
    elf = create_elf64_arm64()

    # Write binary
    with open('/Users/sicarii/anubisvm/contracts/test_elf/minimal_contract.elf', 'wb') as f:
        f.write(elf)

    # Write hex version for deployment
    hex_str = elf.hex()
    with open('/Users/sicarii/anubisvm/contracts/test_elf/minimal_contract.hex', 'w') as f:
        f.write(hex_str)

    print(f"Created minimal ELF binary:")
    print(f"  Size: {len(elf)} bytes")
    print(f"  Hex length: {len(hex_str)} chars")
    print(f"  Architecture: ARM64 (AArch64)")
    print(f"  Entry point: 0x{0x400000 + 64:x}")
    print(f"")
    print(f"Files created:")
    print(f"  /Users/sicarii/anubisvm/contracts/test_elf/minimal_contract.elf")
    print(f"  /Users/sicarii/anubisvm/contracts/test_elf/minimal_contract.hex")


if __name__ == '__main__':
    main()
