--  SPHINX ELF Loader Implementation
--
--  Note: Implementation uses SPARK_Mode Off for system calls (mmap, mprotect)
--  The spec maintains full SPARK contracts for interface verification

pragma SPARK_Mode (Off);

with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Interfaces.C; use type Interfaces.C.int; use type Interfaces.C.size_t;
                   use type Interfaces.C.unsigned;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Anubis_Types;
with Anubis_SHA3;

package body Sphinx_ELF_Loader is

   ---------------------------------------------------------------------------
   --  POSIX System Calls (mmap, mprotect, munmap)
   ---------------------------------------------------------------------------

   --  Memory protection flags (use unsigned for bitwise operations)
   PROT_NONE  : constant Interfaces.C.unsigned := 0;
   PROT_READ  : constant Interfaces.C.unsigned := 1;
   PROT_WRITE : constant Interfaces.C.unsigned := 2;
   PROT_EXEC  : constant Interfaces.C.unsigned := 4;

   --  mmap flags (use unsigned for bitwise operations)
   MAP_PRIVATE   : constant Interfaces.C.unsigned := 16#02#;
   MAP_ANONYMOUS : constant Interfaces.C.unsigned := 16#1000#;  --  macOS
   MAP_ANON      : constant Interfaces.C.unsigned := 16#1000#;  --  BSD
   MAP_FIXED     : constant Interfaces.C.unsigned := 16#10#;

   --  MAP_FAILED is ((void *) -1) in POSIX - represents all bits set
   MAP_FAILED : constant System.Address :=
      System.Storage_Elements.To_Address (
         System.Storage_Elements.Integer_Address'Last);

   --  mmap: Allocate memory region
   function C_mmap (
      Addr   : System.Address;
      Length : Interfaces.C.size_t;
      Prot   : Interfaces.C.int;
      Flags  : Interfaces.C.int;
      Fd     : Interfaces.C.int;
      Offset : Interfaces.C.long
   ) return System.Address
   with Import => True, Convention => C, External_Name => "mmap";

   --  Helper to convert unsigned flags to int for C calls
   function To_Int (U : Interfaces.C.unsigned) return Interfaces.C.int is
      (Interfaces.C.int (U));

   --  mprotect: Change memory protection
   function C_mprotect (
      Addr   : System.Address;
      Length : Interfaces.C.size_t;
      Prot   : Interfaces.C.int
   ) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "mprotect";

   --  munmap: Unmap memory region
   function C_munmap (
      Addr   : System.Address;
      Length : Interfaces.C.size_t
   ) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "munmap";

   --  memcpy: Copy memory
   procedure C_memcpy (
      Dest : System.Address;
      Src  : System.Address;
      N    : Interfaces.C.size_t
   ) with Import => True, Convention => C, External_Name => "memcpy";

   ---------------------------------------------------------------------------
   --  ELF Parsing Helpers
   ---------------------------------------------------------------------------

   function Read_U16_LE (
      Data   : Byte_Array;
      Offset : Natural
   ) return Word32 is
   begin
      if Offset + 1 >= Data'Length then
         return 0;
      end if;
      return Word32 (Data (Data'First + Offset)) or
             Shift_Left (Word32 (Data (Data'First + Offset + 1)), 8);
   end Read_U16_LE;

   function Read_U32_LE (
      Data   : Byte_Array;
      Offset : Natural
   ) return Word32 is
   begin
      if Offset + 3 >= Data'Length then
         return 0;
      end if;
      return Word32 (Data (Data'First + Offset)) or
             Shift_Left (Word32 (Data (Data'First + Offset + 1)), 8) or
             Shift_Left (Word32 (Data (Data'First + Offset + 2)), 16) or
             Shift_Left (Word32 (Data (Data'First + Offset + 3)), 24);
   end Read_U32_LE;

   function Read_U64_LE (
      Data   : Byte_Array;
      Offset : Natural
   ) return Word64 is
   begin
      if Offset + 7 >= Data'Length then
         return 0;
      end if;
      return Word64 (Read_U32_LE (Data, Offset)) or
             Shift_Left (Word64 (Read_U32_LE (Data, Offset + 4)), 32);
   end Read_U64_LE;

   procedure Write_U64_LE (
      Addr  : System.Address;
      Value : Word64
   ) is
      type U64_Ptr is access all Word64;
      function To_U64_Ptr is new Ada.Unchecked_Conversion (
         System.Address, U64_Ptr);
      Ptr : constant U64_Ptr := To_U64_Ptr (Addr);
   begin
      Ptr.all := Value;
   end Write_U64_LE;

   function Read_Mem_U64 (Addr : System.Address) return Word64 is
      type U64_Ptr is access all Word64;
      function To_U64_Ptr is new Ada.Unchecked_Conversion (
         System.Address, U64_Ptr);
      Ptr : constant U64_Ptr := To_U64_Ptr (Addr);
   begin
      return Ptr.all;
   end Read_Mem_U64;

   ---------------------------------------------------------------------------
   --  Load ELF Binary
   ---------------------------------------------------------------------------

   function Load_ELF_Binary (
      Binary : access constant Byte_Array;
      Size   : Natural
   ) return ELF_Load_Result
   is
      Result : ELF_Load_Result := (
         Error => ELF_Success,
         Image => Invalid_ELF_Image
      );
      Image : ELF_Image := Invalid_ELF_Image;
   begin
      Ada.Text_IO.Put_Line ("  [ELF] Loading ELF binary (" &
         Natural'Image (Size) & " bytes)...");

      --  Step 1: Validate ELF header
      if Size < 64 then
         Ada.Text_IO.Put_Line ("  [ELF] ERROR: File too small for ELF header");
         Result.Error := ELF_Invalid_Magic;
         return Result;
      end if;

      --  Check magic number
      if Binary (Binary'First + 0) /= 16#7F# or
         Binary (Binary'First + 1) /= 16#45# or  --  'E'
         Binary (Binary'First + 2) /= 16#4C# or  --  'L'
         Binary (Binary'First + 3) /= 16#46#     --  'F'
      then
         Ada.Text_IO.Put_Line ("  [ELF] ERROR: Invalid ELF magic");
         Result.Error := ELF_Invalid_Magic;
         return Result;
      end if;

      --  Check class (must be 64-bit)
      if Binary (Binary'First + ELF_CLASS_OFFSET) /= ELFCLASS64 then
         Ada.Text_IO.Put_Line ("  [ELF] ERROR: Not a 64-bit ELF");
         Result.Error := ELF_Unsupported_Class;
         return Result;
      end if;

      --  Get machine type
      Image.Machine := Read_U16_LE (Binary.all, ELF_MACHINE_OFFSET);
      Ada.Text_IO.Put_Line ("  [ELF] Machine type: 0x" &
         Word32'Image (Image.Machine));

      case Image.Machine is
         when EM_X86_64 | EM_AARCH64 | EM_RISCV =>
            null;  --  Supported
         when others =>
            Ada.Text_IO.Put_Line ("  [ELF] ERROR: Unsupported architecture");
            Result.Error := ELF_Unsupported_Machine;
            return Result;
      end case;

      --  Get ELF type
      declare
         ELF_Type : constant Word32 := Read_U16_LE (Binary.all, ELF_TYPE_OFFSET);
      begin
         Image.Is_PIE := (ELF_Type = ET_DYN);
         Ada.Text_IO.Put_Line ("  [ELF] Type: " &
            (if Image.Is_PIE then "PIE (ET_DYN)" else "EXEC (ET_EXEC)"));
      end;

      --  Get entry point
      Image.Entry_Point := Read_U64_LE (Binary.all, ELF_ENTRY_OFFSET);
      Ada.Text_IO.Put_Line ("  [ELF] Entry point: 0x" &
         Word64'Image (Image.Entry_Point));

      --  Step 2: Parse program headers
      declare
         PH_Off     : constant Word64 := Read_U64_LE (Binary.all, ELF_PHOFF_OFFSET);
         PH_Entsize : constant Word32 := Read_U16_LE (Binary.all, ELF_PHENTSIZE_OFFSET);
         PH_Num     : constant Word32 := Read_U16_LE (Binary.all, ELF_PHNUM_OFFSET);
      begin
         Ada.Text_IO.Put_Line ("  [ELF] Program headers: " &
            Word32'Image (PH_Num) & " entries at offset 0x" &
            Word64'Image (PH_Off));

         if PH_Num = 0 or PH_Num > Word32 (Max_Segments) then
            Ada.Text_IO.Put_Line ("  [ELF] ERROR: Invalid program header count");
            Result.Error := ELF_Missing_Segments;
            return Result;
         end if;

         --  Parse each program header
         Image.Segment_Count := 0;
         for I in 0 .. Natural (PH_Num) - 1 loop
            declare
               PH_Offset : constant Natural := Natural (PH_Off) + I * Natural (PH_Entsize);
               P_Type    : constant Word32 := Read_U32_LE (Binary.all, PH_Offset + 0);
               P_Flags   : constant Word32 := Read_U32_LE (Binary.all, PH_Offset + 4);
               P_Offset  : constant Word64 := Read_U64_LE (Binary.all, PH_Offset + 8);
               P_Vaddr   : constant Word64 := Read_U64_LE (Binary.all, PH_Offset + 16);
               P_Paddr   : constant Word64 := Read_U64_LE (Binary.all, PH_Offset + 24);
               P_Filesz  : constant Word64 := Read_U64_LE (Binary.all, PH_Offset + 32);
               P_Memsz   : constant Word64 := Read_U64_LE (Binary.all, PH_Offset + 40);
               P_Align   : constant Word64 := Read_U64_LE (Binary.all, PH_Offset + 48);
            begin
               if P_Type = PT_LOAD then
                  --  This is a loadable segment
                  if Image.Segment_Count < Max_Segments then
                     declare
                        Idx : constant Segment_Index := Segment_Index (Image.Segment_Count);
                     begin
                        Image.Segments (Idx) := (
                           Virtual_Addr  => P_Vaddr,
                           Physical_Addr => P_Paddr,
                           File_Offset   => P_Offset,
                           File_Size     => P_Filesz,
                           Memory_Size   => P_Memsz,
                           Flags         => P_Flags,
                           Alignment     => P_Align,
                           Mapped_Addr   => System.Null_Address,
                           Is_Loaded     => False
                        );
                        Image.Segment_Count := Image.Segment_Count + 1;

                        Ada.Text_IO.Put_Line ("  [ELF]   Segment " &
                           Natural'Image (Image.Segment_Count) &
                           ": vaddr=0x" & Word64'Image (P_Vaddr) &
                           " memsz=" & Word64'Image (P_Memsz) &
                           " flags=" & Word32'Image (P_Flags));
                     end;
                  end if;
               elsif P_Type = PT_DYNAMIC then
                  Image.Dynamic_Addr := P_Vaddr;
                  Ada.Text_IO.Put_Line ("  [ELF]   Dynamic section at 0x" &
                     Word64'Image (P_Vaddr));
               end if;
            end;
         end loop;

         if Image.Segment_Count = 0 then
            Ada.Text_IO.Put_Line ("  [ELF] ERROR: No loadable segments");
            Result.Error := ELF_Missing_Segments;
            return Result;
         end if;
      end;

      --  Step 3: Determine base address for PIE with ASLR
      if Image.Is_PIE then
         --  ASLR: Generate randomized base address
         --  Use hash of the binary content as entropy source for determinism
         --  Range: 0x4000_0000 to 0x7FFF_0000 (1GB-2GB range)
         declare
            use Anubis_SHA3;
            Entropy : SHA3_256_Digest;
            Random_Offset : Aegis_VM_Types.Word64;
         begin
            --  Hash first 256 bytes of binary for entropy
            if Size >= 256 then
               declare
                  Sample : Anubis_Types.Byte_Array (0 .. 255);
               begin
                  for I in 0 .. 255 loop
                     Sample (I) := Anubis_Types.Byte (Binary (Binary'First + I));
                  end loop;
                  SHA3_256 (Sample, Entropy);
               end;
            else
               --  Fallback: hash what we have
               declare
                  Sample : Anubis_Types.Byte_Array (0 .. Size - 1);
               begin
                  for I in 0 .. Size - 1 loop
                     Sample (I) := Anubis_Types.Byte (Binary (Binary'First + I));
                  end loop;
                  SHA3_256 (Sample, Entropy);
               end;
            end if;

            --  Extract 4 bytes from digest as random offset
            Random_Offset := Aegis_VM_Types.Word64 (Entropy (0)) or
                             Shift_Left (Aegis_VM_Types.Word64 (Entropy (1)), 8) or
                             Shift_Left (Aegis_VM_Types.Word64 (Entropy (2)), 16) or
                             Shift_Left (Aegis_VM_Types.Word64 (Entropy (3)), 24);

            --  ASLR: Random base in 1GB-2GB range, page-aligned
            Image.Base_Address := 16#4000_0000# + (Random_Offset mod 16#3FFF_0000#);
            Image.Base_Address := (Image.Base_Address / 4096) * 4096;  -- Page align
         end;
         Ada.Text_IO.Put_Line ("  [ELF] PIE base address (ASLR): 0x" &
            Word64'Image (Image.Base_Address));
      else
         Image.Base_Address := 0;
      end if;

      --  Step 4: Map segments into memory
      Ada.Text_IO.Put_Line ("  [ELF] Mapping segments...");
      for I in 0 .. Image.Segment_Count - 1 loop
         declare
            Seg : Memory_Segment renames Image.Segments (Segment_Index (I));
            Map_Addr  : System.Address;
            Map_Size  : Interfaces.C.size_t;
            Map_Prot  : Interfaces.C.unsigned;
            Load_Addr : Word64;
         begin
            --  Calculate load address
            if Image.Is_PIE then
               Load_Addr := Image.Base_Address + Seg.Virtual_Addr;
            else
               Load_Addr := Seg.Virtual_Addr;
            end if;

            --  Round size up to page boundary (4KB)
            Map_Size := Interfaces.C.size_t ((Seg.Memory_Size + 4095) / 4096 * 4096);

            --  Set initial protections (RW for loading, will be changed later)
            Map_Prot := PROT_READ or PROT_WRITE;

            --  Allocate memory region
            Map_Addr := C_mmap (
               Addr   => System.Null_Address,  --  Let kernel choose
               Length => Map_Size,
               Prot   => To_Int (Map_Prot),
               Flags  => To_Int (MAP_PRIVATE or MAP_ANONYMOUS),
               Fd     => -1,
               Offset => 0
            );

            if Map_Addr = MAP_FAILED or Map_Addr = System.Null_Address then
               Ada.Text_IO.Put_Line ("  [ELF] ERROR: mmap failed for segment " &
                  Natural'Image (I));
               Result.Error := ELF_Mmap_Failed;
               return Result;
            end if;

            Seg.Mapped_Addr := Map_Addr;
            Seg.Is_Loaded := True;

            Ada.Text_IO.Put_Line ("  [ELF]   Segment " & Natural'Image (I) &
               " mapped at 0x" & Integer_Address'Image (To_Integer (Map_Addr)) &
               " (" & Interfaces.C.size_t'Image (Map_Size) & " bytes)");

            --  Copy data from ELF file to mapped memory
            if Seg.File_Size > 0 then
               declare
                  Src_Offset : constant Natural := Natural (Seg.File_Offset);
               begin
                  if Src_Offset + Natural (Seg.File_Size) <= Size then
                     C_memcpy (
                        Dest => Map_Addr,
                        Src  => Binary (Binary'First + Src_Offset)'Address,
                        N    => Interfaces.C.size_t (Seg.File_Size)
                     );
                     Ada.Text_IO.Put_Line ("  [ELF]     Copied " &
                        Word64'Image (Seg.File_Size) & " bytes from file");
                  end if;
               end;
            end if;

            --  Zero BSS if memory size > file size
            if Seg.Memory_Size > Seg.File_Size then
               declare
                  BSS_Start : constant System.Address :=
                     Map_Addr + Storage_Offset (Seg.File_Size);
                  BSS_Size  : constant Interfaces.C.size_t :=
                     Interfaces.C.size_t (Seg.Memory_Size - Seg.File_Size);
               begin
                  Ada.Text_IO.Put_Line ("  [ELF]     Zeroing " &
                     Interfaces.C.size_t'Image (BSS_Size) & " bytes BSS");
                  --  Zero it manually
                  declare
                     type Byte_Ptr is access all Byte;
                     function To_Byte_Ptr is new Ada.Unchecked_Conversion (
                        System.Address, Byte_Ptr);
                  begin
                     for J in 0 .. Natural (BSS_Size) - 1 loop
                        declare
                           Ptr : constant Byte_Ptr :=
                              To_Byte_Ptr (BSS_Start + Storage_Offset (J));
                        begin
                           Ptr.all := 0;
                        end;
                     end loop;
                  end;
               end;
            end if;
         end;
      end loop;

      --  Step 5: Process relocations
      Ada.Text_IO.Put_Line ("  [ELF] Processing relocations...");
      declare
         Reloc_OK : Boolean;
      begin
         Process_Relocations (Image, Binary, Reloc_OK);
         if not Reloc_OK then
            Ada.Text_IO.Put_Line ("  [ELF] ERROR: Relocation processing failed");
            Result.Error := ELF_Relocation_Failed;
            return Result;
         end if;
      end;

      --  Step 6: Apply page protections (W^X enforcement)
      Ada.Text_IO.Put_Line ("  [ELF] Applying page protections...");
      if not Apply_Page_Protections (Image) then
         Ada.Text_IO.Put_Line ("  [ELF] ERROR: Failed to apply protections");
         Result.Error := ELF_Mprotect_Failed;
         return Result;
      end if;

      --  Adjust entry point for PIE
      if Image.Is_PIE then
         Image.Entry_Point := Image.Base_Address + Image.Entry_Point;
         Ada.Text_IO.Put_Line ("  [ELF] Adjusted entry point: 0x" &
            Word64'Image (Image.Entry_Point));
      end if;

      Image.Is_Valid := True;
      Result.Image := Image;
      Ada.Text_IO.Put_Line ("  [ELF] Load complete - ready for execution");

      return Result;
   end Load_ELF_Binary;

   ---------------------------------------------------------------------------
   --  Process Relocations
   ---------------------------------------------------------------------------

   procedure Process_Relocations (
      Image   : in out ELF_Image;
      Binary  : access constant Byte_Array;
      Success : out Boolean
   )
   is
      --  Relocation type constants
      R_X86_64_RELATIVE  : constant := 8;
      R_AARCH64_RELATIVE : constant := 1027;
      R_RISCV_RELATIVE   : constant := 3;

      --  Dynamic section tag constants
      DT_NULL     : constant := 0;
      DT_RELA     : constant := 7;
      DT_RELASZ   : constant := 8;
      DT_RELAENT  : constant := 9;

      --  Parsed dynamic info
      Rela_Offset  : Word64 := 0;
      Rela_Size    : Word64 := 0;
      Rela_Entsize : Word64 := 24;  -- Default Elf64_Rela size
      Has_Rela     : Boolean := False;

      Reloc_Type_Expected : Word64;
      Reloc_Count         : Natural := 0;
   begin
      Ada.Text_IO.Put_Line ("  [ELF] Relocation processing:");

      --  Non-PIE binaries typically don't need runtime relocations
      if not Image.Is_PIE then
         Ada.Text_IO.Put_Line ("  [ELF]   Static executable - no relocations needed");
         Success := True;
         return;
      end if;

      Ada.Text_IO.Put_Line ("  [ELF]   PIE: applying base address relocations");
      Ada.Text_IO.Put_Line ("  [ELF]   Base: 0x" & Word64'Image (Image.Base_Address));

      --  Determine expected relocation type based on architecture
      case Image.Machine is
         when EM_X86_64 =>
            Reloc_Type_Expected := R_X86_64_RELATIVE;
         when EM_AARCH64 =>
            Reloc_Type_Expected := R_AARCH64_RELATIVE;
         when EM_RISCV =>
            Reloc_Type_Expected := R_RISCV_RELATIVE;
         when others =>
            Ada.Text_IO.Put_Line ("  [ELF]   Unknown architecture for relocations");
            Success := False;
            return;
      end case;

      --  Step 1: Find .dynamic section and parse it
      if Image.Dynamic_Addr /= 0 then
         Ada.Text_IO.Put_Line ("  [ELF]   Parsing .dynamic section at 0x" &
            Word64'Image (Image.Dynamic_Addr));

         --  Find the segment containing .dynamic
         for I in 0 .. Image.Segment_Count - 1 loop
            declare
               Seg : Memory_Segment renames Image.Segments (Segment_Index (I));
               Dyn_Offset : Word64;
            begin
               if Seg.Is_Loaded and then
                  Image.Dynamic_Addr >= Seg.Virtual_Addr and then
                  Image.Dynamic_Addr < Seg.Virtual_Addr + Seg.Memory_Size
               then
                  --  .dynamic is in this segment
                  Dyn_Offset := Image.Dynamic_Addr - Seg.Virtual_Addr;

                  --  Parse dynamic entries (d_tag, d_val pairs - 16 bytes each)
                  declare
                     Dyn_Addr : System.Address :=
                        Seg.Mapped_Addr + Storage_Offset (Dyn_Offset);
                     D_Tag : Word64;
                     D_Val : Word64;
                  begin
                     loop
                        D_Tag := Read_Mem_U64 (Dyn_Addr);
                        D_Val := Read_Mem_U64 (Dyn_Addr + Storage_Offset (8));

                        exit when D_Tag = DT_NULL;

                        case D_Tag is
                           when DT_RELA =>
                              Rela_Offset := D_Val;
                              Has_Rela := True;
                              Ada.Text_IO.Put_Line ("  [ELF]     DT_RELA: 0x" &
                                 Word64'Image (Rela_Offset));
                           when DT_RELASZ =>
                              Rela_Size := D_Val;
                              Ada.Text_IO.Put_Line ("  [ELF]     DT_RELASZ: " &
                                 Word64'Image (Rela_Size));
                           when DT_RELAENT =>
                              Rela_Entsize := D_Val;
                              Ada.Text_IO.Put_Line ("  [ELF]     DT_RELAENT: " &
                                 Word64'Image (Rela_Entsize));
                           when others =>
                              null;  -- Skip other tags
                        end case;

                        Dyn_Addr := Dyn_Addr + Storage_Offset (16);
                     end loop;
                  end;

                  exit;  -- Found dynamic section
               end if;
            end;
         end loop;
      end if;

      --  Step 2: Apply relocations if we have them
      if Has_Rela and Rela_Size > 0 and Rela_Entsize > 0 then
         Ada.Text_IO.Put_Line ("  [ELF]   Processing " &
            Natural'Image (Natural (Rela_Size / Rela_Entsize)) & " relocations");

         --  Find segment containing .rela.dyn
         for I in 0 .. Image.Segment_Count - 1 loop
            declare
               Seg : Memory_Segment renames Image.Segments (Segment_Index (I));
            begin
               if Seg.Is_Loaded and then
                  Rela_Offset >= Seg.Virtual_Addr and then
                  Rela_Offset < Seg.Virtual_Addr + Seg.Memory_Size
               then
                  --  Found the segment with relocations
                  declare
                     Rela_Start : constant System.Address :=
                        Seg.Mapped_Addr + Storage_Offset (Rela_Offset - Seg.Virtual_Addr);
                     Num_Relas : constant Natural :=
                        Natural (Rela_Size / Rela_Entsize);
                  begin
                     for J in 0 .. Num_Relas - 1 loop
                        declare
                           Entry_Addr : constant System.Address :=
                              Rela_Start + Storage_Offset (Word64 (J) * Rela_Entsize);
                           R_Offset : constant Word64 := Read_Mem_U64 (Entry_Addr);
                           R_Info   : constant Word64 :=
                              Read_Mem_U64 (Entry_Addr + Storage_Offset (8));
                           R_Addend : constant Word64 :=
                              Read_Mem_U64 (Entry_Addr + Storage_Offset (16));
                           R_Type   : constant Word64 := R_Info and 16#FFFF_FFFF#;
                        begin
                           --  Only handle RELATIVE relocations
                           if R_Type = Reloc_Type_Expected then
                              --  Find target location in mapped memory
                              for K in 0 .. Image.Segment_Count - 1 loop
                                 declare
                                    Target_Seg : Memory_Segment renames
                                       Image.Segments (Segment_Index (K));
                                 begin
                                    if Target_Seg.Is_Loaded and then
                                       R_Offset >= Target_Seg.Virtual_Addr and then
                                       R_Offset < Target_Seg.Virtual_Addr +
                                                  Target_Seg.Memory_Size
                                    then
                                       --  Apply relocation: *ptr = base + addend
                                       declare
                                          Target_Addr : constant System.Address :=
                                             Target_Seg.Mapped_Addr +
                                             Storage_Offset (R_Offset -
                                                Target_Seg.Virtual_Addr);
                                          New_Value : constant Word64 :=
                                             Image.Base_Address + R_Addend;
                                       begin
                                          Write_U64_LE (Target_Addr, New_Value);
                                          Reloc_Count := Reloc_Count + 1;
                                       end;
                                       exit;
                                    end if;
                                 end;
                              end loop;
                           end if;
                        end;
                     end loop;
                  end;

                  exit;  -- Done with relocations
               end if;
            end;
         end loop;
      else
         --  No relocations found - check if binary has embedded relocation data
         Ada.Text_IO.Put_Line ("  [ELF]   No .rela.dyn section found");

         --  Try to find relocations in the binary directly
         --  Look for section headers to find .rela.dyn
         if Binary'Length >= 64 then
            declare
               SH_Off     : constant Word64 :=
                  Read_U64_LE (Binary.all, ELF_SHOFF_OFFSET);
               SH_Entsize : constant Word32 :=
                  Read_U16_LE (Binary.all, ELF_SHENTSIZE_OFFSET);
               SH_Num     : constant Word32 :=
                  Read_U16_LE (Binary.all, ELF_SHNUM_OFFSET);

               --  Section types
               SHT_RELA : constant := 4;
            begin
               if SH_Off > 0 and SH_Num > 0 and SH_Entsize >= 64 then
                  Ada.Text_IO.Put_Line ("  [ELF]   Scanning " &
                     Word32'Image (SH_Num) & " section headers for RELA");

                  for I in 0 .. Natural (SH_Num) - 1 loop
                     declare
                        SH_Base : constant Natural :=
                           Natural (SH_Off) + I * Natural (SH_Entsize);
                        Sh_Type : constant Word32 :=
                           Read_U32_LE (Binary.all, SH_Base + 4);
                     begin
                        if Sh_Type = SHT_RELA then
                           --  Found a RELA section
                           declare
                              Sh_Offset : constant Word64 :=
                                 Read_U64_LE (Binary.all, SH_Base + 24);
                              Sh_Size   : constant Word64 :=
                                 Read_U64_LE (Binary.all, SH_Base + 32);
                              Sh_Entsize : constant Word64 :=
                                 Read_U64_LE (Binary.all, SH_Base + 56);
                              Num_Entries : Natural;
                           begin
                              if Sh_Entsize > 0 then
                                 Num_Entries := Natural (Sh_Size / Sh_Entsize);
                                 Ada.Text_IO.Put_Line ("  [ELF]     Found RELA section: " &
                                    Natural'Image (Num_Entries) & " entries");

                                 --  Apply each relocation
                                 for J in 0 .. Num_Entries - 1 loop
                                    declare
                                       Entry_Off : constant Natural :=
                                          Natural (Sh_Offset) +
                                          J * Natural (Sh_Entsize);
                                       R_Offset : constant Word64 :=
                                          Read_U64_LE (Binary.all, Entry_Off);
                                       R_Info   : constant Word64 :=
                                          Read_U64_LE (Binary.all, Entry_Off + 8);
                                       R_Addend : constant Word64 :=
                                          Read_U64_LE (Binary.all, Entry_Off + 16);
                                       R_Type   : constant Word64 :=
                                          R_Info and 16#FFFF_FFFF#;
                                    begin
                                       if R_Type = Reloc_Type_Expected then
                                          --  Find and apply
                                          for K in 0 .. Image.Segment_Count - 1 loop
                                             declare
                                                Target_Seg : Memory_Segment renames
                                                   Image.Segments (Segment_Index (K));
                                             begin
                                                if Target_Seg.Is_Loaded and then
                                                   R_Offset >= Target_Seg.Virtual_Addr
                                                   and then
                                                   R_Offset < Target_Seg.Virtual_Addr +
                                                              Target_Seg.Memory_Size
                                                then
                                                   declare
                                                      Target_Addr : constant
                                                         System.Address :=
                                                         Target_Seg.Mapped_Addr +
                                                         Storage_Offset (R_Offset -
                                                            Target_Seg.Virtual_Addr);
                                                      New_Value : constant Word64 :=
                                                         Image.Base_Address + R_Addend;
                                                   begin
                                                      Write_U64_LE (Target_Addr, New_Value);
                                                      Reloc_Count := Reloc_Count + 1;
                                                   end;
                                                   exit;
                                                end if;
                                             end;
                                          end loop;
                                       end if;
                                    end;
                                 end loop;
                              end if;
                           end;
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end if;
      end if;

      Ada.Text_IO.Put_Line ("  [ELF]   Applied " & Natural'Image (Reloc_Count) &
         " RELATIVE relocations");
      Ada.Text_IO.Put_Line ("  [ELF]   Relocation processing complete");
      Success := True;
   end Process_Relocations;

   ---------------------------------------------------------------------------
   --  Resolve Symbol
   ---------------------------------------------------------------------------

   function Resolve_Symbol (
      Image       : ELF_Image;
      Binary      : access constant Byte_Array;
      Symbol_Name : String
   ) return Word64
   is
      pragma Unreferenced (Image, Binary, Symbol_Name);
   begin
      --  Symbol resolution would require parsing .dynsym and .symtab
      --  For SPARK contracts, most symbols are resolved at link time
      Ada.Text_IO.Put_Line ("  [ELF] Symbol resolution not yet implemented");
      return 0;
   end Resolve_Symbol;

   ---------------------------------------------------------------------------
   --  Apply Page Protections
   ---------------------------------------------------------------------------

   function Apply_Page_Protections (Image : ELF_Image) return Boolean is
   begin
      Ada.Text_IO.Put_Line ("  [ELF] Setting page protections (W^X enforcement)...");

      for I in 0 .. Image.Segment_Count - 1 loop
         declare
            Seg  : Memory_Segment renames Image.Segments (Segment_Index (I));
            Prot : Interfaces.C.unsigned := PROT_NONE;
            Size : Interfaces.C.size_t;
            Res  : Interfaces.C.int;
         begin
            if not Seg.Is_Loaded then
               goto Continue;
            end if;

            --  Build protection flags from segment flags
            if (Seg.Flags and PF_R) /= 0 then
               Prot := Prot or PROT_READ;
            end if;
            if (Seg.Flags and PF_W) /= 0 then
               Prot := Prot or PROT_WRITE;
            end if;
            if (Seg.Flags and PF_X) /= 0 then
               Prot := Prot or PROT_EXEC;
            end if;

            --  W^X enforcement: cannot be both writable and executable
            if (Prot and PROT_WRITE) /= 0 and then (Prot and PROT_EXEC) /= 0 then
               Ada.Text_IO.Put_Line ("  [ELF] ERROR: Segment " & Natural'Image (I) &
                  " violates W^X (both writable and executable)");
               return False;
            end if;

            --  Apply protection
            Size := Interfaces.C.size_t ((Seg.Memory_Size + 4095) / 4096 * 4096);
            Res := C_mprotect (Seg.Mapped_Addr, Size, To_Int (Prot));

            if Res /= 0 then
               Ada.Text_IO.Put_Line ("  [ELF] ERROR: mprotect failed for segment " &
                  Natural'Image (I));
               return False;
            end if;

            Ada.Text_IO.Put_Line ("  [ELF]   Segment " & Natural'Image (I) & ": " &
               (if (Prot and PROT_READ) /= 0 then "R" else "-") &
               (if (Prot and PROT_WRITE) /= 0 then "W" else "-") &
               (if (Prot and PROT_EXEC) /= 0 then "X" else "-"));

            <<Continue>>
         end;
      end loop;

      Ada.Text_IO.Put_Line ("  [ELF] Page protections applied successfully");
      return True;
   end Apply_Page_Protections;

   ---------------------------------------------------------------------------
   --  Execute ELF
   ---------------------------------------------------------------------------

   function Execute_ELF (
      Image     : ELF_Image;
      Calldata  : access constant Byte_Array;
      Gas_Limit : Gas_Amount
   ) return Execution_Result
   is
      Result : Execution_Result := (
         Exit_Code   => -1,
         Gas_Used    => 0,
         Return_Data => (others => 0),
         Success     => False
      );

      --  Function pointer type for entry point
      type Entry_Function is access function (
         Calldata_Ptr : System.Address;
         Calldata_Len : Interfaces.C.size_t;
         Return_Ptr   : System.Address;
         Return_Len   : access Interfaces.C.size_t;
         Gas_Limit_In : Unsigned_64;
         Gas_Used_Out : access Unsigned_64
      ) return Interfaces.C.int
      with Convention => C;

      function To_Entry_Fn is new Ada.Unchecked_Conversion (
         System.Address, Entry_Function);

      Entry_Addr : System.Address;
      Entry_Fn   : Entry_Function;
   begin
      Ada.Text_IO.Put_Line ("  [ELF] Executing ELF at entry point 0x" &
         Word64'Image (Image.Entry_Point));

      if not Image.Is_Valid then
         Ada.Text_IO.Put_Line ("  [ELF] ERROR: Invalid ELF image");
         return Result;
      end if;

      --  Convert entry point to function pointer
      --  This is the core of direct ELF execution
      Entry_Addr := System.Storage_Elements.To_Address (
         System.Storage_Elements.Integer_Address (Image.Entry_Point));
      Entry_Fn := To_Entry_Fn (Entry_Addr);

      Ada.Text_IO.Put_Line ("  [ELF] Jumping to entry point...");

      declare
         Return_Buf    : aliased Byte_Array (0 .. 4095) := (others => 0);
         Return_Len    : aliased Interfaces.C.size_t := 0;
         Gas_Used_Val  : aliased Unsigned_64 := 0;
         Exit_Code     : Interfaces.C.int;
      begin
         --  Call the entry point with contract ABI
         Exit_Code := Entry_Fn (
            Calldata_Ptr => (if Calldata'Length > 0 then
                               Calldata (Calldata'First)'Address
                            else System.Null_Address),
            Calldata_Len => Interfaces.C.size_t (Calldata'Length),
            Return_Ptr   => Return_Buf (Return_Buf'First)'Address,
            Return_Len   => Return_Len'Access,
            Gas_Limit_In => Unsigned_64 (Gas_Limit),
            Gas_Used_Out => Gas_Used_Val'Access
         );

         Result.Exit_Code := Integer (Exit_Code);
         Result.Gas_Used := Gas_Amount (Gas_Used_Val);
         Result.Success := (Exit_Code = 0);

         Ada.Text_IO.Put_Line ("  [ELF] Execution completed:");
         Ada.Text_IO.Put_Line ("  [ELF]   Exit code: " & Integer'Image (Result.Exit_Code));
         Ada.Text_IO.Put_Line ("  [ELF]   Gas used: " & Gas_Amount'Image (Result.Gas_Used));
         Ada.Text_IO.Put_Line ("  [ELF]   Return length: " &
            Interfaces.C.size_t'Image (Return_Len));

         --  Hash return data if any
         if Return_Len > 0 and Natural (Return_Len) <= Return_Buf'Length then
            declare
               use Anubis_Types;
               use Anubis_SHA3;
               Return_Data : Anubis_Types.Byte_Array (0 .. Natural (Return_Len) - 1);
               Digest : SHA3_256_Digest;
            begin
               for I in 0 .. Natural (Return_Len) - 1 loop
                  Return_Data (I) := Anubis_Types.Byte (Return_Buf (I));
               end loop;
               SHA3_256 (Return_Data, Digest);
               for I in 0 .. 31 loop
                  Result.Return_Data (I) := Aegis_VM_Types.Byte (Digest (I));
               end loop;
            end;
         end if;
      exception
         when E : others =>
            Ada.Text_IO.Put_Line ("  [ELF] EXCEPTION during execution: " &
               Ada.Exceptions.Exception_Message (E));
            Result.Success := False;
            Result.Gas_Used := Gas_Limit;  --  Charge full gas on crash
      end;

      return Result;
   end Execute_ELF;

   ---------------------------------------------------------------------------
   --  Unload ELF
   ---------------------------------------------------------------------------

   procedure Unload_ELF (Image : in out ELF_Image) is
      Res : Interfaces.C.int;
      pragma Unreferenced (Res);
   begin
      Ada.Text_IO.Put_Line ("  [ELF] Unloading ELF image...");

      for I in 0 .. Image.Segment_Count - 1 loop
         declare
            Seg : Memory_Segment renames Image.Segments (Segment_Index (I));
            Size : Interfaces.C.size_t;
         begin
            if Seg.Is_Loaded and Seg.Mapped_Addr /= System.Null_Address then
               Size := Interfaces.C.size_t ((Seg.Memory_Size + 4095) / 4096 * 4096);
               Res := C_munmap (Seg.Mapped_Addr, Size);
               Ada.Text_IO.Put_Line ("  [ELF]   Unmapped segment " & Natural'Image (I));
               Seg.Mapped_Addr := System.Null_Address;
               Seg.Is_Loaded := False;
            end if;
         end;
      end loop;

      Image.Is_Valid := False;
      Ada.Text_IO.Put_Line ("  [ELF] Unload complete");
   end Unload_ELF;

   ---------------------------------------------------------------------------
   --  Is Valid Address
   ---------------------------------------------------------------------------

   function Is_Valid_Address (
      Image   : ELF_Image;
      Address : Word64
   ) return Boolean
   is
   begin
      for I in 0 .. Image.Segment_Count - 1 loop
         declare
            Seg : Memory_Segment renames Image.Segments (Segment_Index (I));
            Seg_Start : Word64;
            Seg_End   : Word64;
         begin
            if Seg.Is_Loaded then
               if Image.Is_PIE then
                  Seg_Start := Image.Base_Address + Seg.Virtual_Addr;
               else
                  Seg_Start := Seg.Virtual_Addr;
               end if;
               Seg_End := Seg_Start + Seg.Memory_Size;

               if Address >= Seg_Start and Address < Seg_End then
                  return True;
               end if;
            end if;
         end;
      end loop;

      return False;
   end Is_Valid_Address;

end Sphinx_ELF_Loader;
