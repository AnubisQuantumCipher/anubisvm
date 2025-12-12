/* ARM Cortex-M Linker Script for AnubisVM Embedded Contracts */

/* Memory layout - customize per target board */
MEMORY
{
    FLASH (rx)  : ORIGIN = 0x08000000, LENGTH = 256K
    RAM (rwx)   : ORIGIN = 0x20000000, LENGTH = 64K
}

/* Entry point */
ENTRY(Reset_Handler)

SECTIONS
{
    /* Vector table at start of flash */
    .isr_vector :
    {
        . = ALIGN(4);
        KEEP(*(.isr_vector))
        . = ALIGN(4);
    } > FLASH

    /* Code section */
    .text :
    {
        . = ALIGN(4);
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
        *(.data)
        *(.data*)
        . = ALIGN(4);
        _edata = .;
    } > RAM

    /* Uninitialized data */
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

    /* Stack */
    .stack (NOLOAD) :
    {
        . = ALIGN(8);
        _sstack = .;
        . = . + 8K;  /* 8KB stack */
        _estack = .;
    } > RAM

    /* Heap */
    .heap (NOLOAD) :
    {
        . = ALIGN(8);
        _sheap = .;
        . = . + 16K;  /* 16KB heap */
        _eheap = .;
    } > RAM

    /* Discard debug sections to reduce size */
    /DISCARD/ :
    {
        *(.comment)
        *(.debug*)
    }
}

/* Provide symbols for startup code */
PROVIDE(_stack_start = _estack);
PROVIDE(_heap_start = _sheap);
PROVIDE(_heap_end = _eheap);
