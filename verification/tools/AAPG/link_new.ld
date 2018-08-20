OUTPUT_ARCH( "riscv" )
ENTRY(_start)
SECTIONS
{

  /* text: test code section */
  . = 0x80000000;
  .text.init : { *(.text.init) }

  . = ALIGN(0x1000);
  .tohost : { *(.tohost) }

  .text : { *(.text) }

  /* data segment */
  .data : { *(.data) }

  /* End of uninitalized data segement */
  _end = .;
}

