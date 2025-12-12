/* RISC-V Linker Script for AnubisVM Embedded Contracts */

/* Memory layout - customize per target board */
MEMORY
{
    FLASH (rx)  : ORIGIN = 0x20000000, LENGTH = 256K
    RAM (rwx)   : ORIGIN = 0x80000000, LENGTH = 64K
}

/* Entry point */
ENTRY(_start)

SECTIONS
{
    /* Code section at start of flash */
    .text :
    {
        . = ALIGN(4);
        *(.text.init)
        *(.text.trap)
        *(.text)
        *(.text*)
        *(.rodata)
        *(.rodata*)
        . = ALIGN(4);
        _etext = .;
    } > FLASH

    /* Initialized data */
    .data : AT(_etext)
    {
        . = ALIGN(4);
        _sdata = .;
        *(.sdata)
        *(.sdata*)
        *(.data)
        *(.data*)
        . = ALIGN(4);
        _edata = .;
    } > RAM

    /* Small BSS */
    .sbss :
    {
        . = ALIGN(4);
        _ssbss = .;
        *(.sbss)
        *(.sbss*)
        . = ALIGN(4);
        _esbss = .;
    } > RAM

    /* BSS */
    .bss :
    {
        . = ALIGN(4);
        _sbss = .;
        *(.bss)
        *(.bss*)
        *(COMMON)
        . = ALIGN(4);
        _ebss = .;
    } > RAM

    /* Stack - grows down from top of RAM */
    .stack (NOLOAD) :
    {
        . = ALIGN(16);
        _sstack = .;
        . = . + 8K;
        _estack = .;
    } > RAM

    /* Heap */
    .heap (NOLOAD) :
    {
        . = ALIGN(8);
        _sheap = .;
        . = . + 16K;
        _eheap = .;
    } > RAM

    /DISCARD/ :
    {
        *(.comment)
        *(.debug*)
        *(.riscv.attributes)
    }
}

/* Symbols for startup and runtime */
PROVIDE(_stack_start = _estack);
PROVIDE(_heap_start = _sheap);
PROVIDE(_heap_end = _eheap);
PROVIDE(_global_pointer = _sdata + 0x800);
