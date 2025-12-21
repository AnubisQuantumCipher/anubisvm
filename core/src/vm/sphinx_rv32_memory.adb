--  SPHINX RV32 Memory: Implementation

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Sphinx_RV32_Memory with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Helper: Check bounds
   ---------------------------------------------------------------------------

   function Check_Bounds (Addr : Word; Size : Positive) return Boolean is
   begin
      return Natural (Addr) <= Max_Memory_Size - Size;
   end Check_Bounds;

   ---------------------------------------------------------------------------
   --  Byte Load Operations
   ---------------------------------------------------------------------------

   procedure Load_Byte (
      Memory  : in     Contract_Memory;
      Addr    : in     Word;
      Value   : out    Word;
      Success : out    Boolean
   ) is
   begin
      if Check_Bounds (Addr, 1) then
         Value := Word (Memory (Natural (Addr)));
         Success := True;
      else
         Value := 0;
         Success := False;
      end if;
   end Load_Byte;

   procedure Load_Byte_Signed (
      Memory  : in     Contract_Memory;
      Addr    : in     Word;
      Value   : out    SWord;
      Success : out    Boolean
   ) is
      Raw : RV_Byte;
   begin
      if Check_Bounds (Addr, 1) then
         Raw := Memory (Natural (Addr));
         --  Sign extend from bit 7
         if Raw >= 128 then
            Value := SWord (Raw) - 256;
         else
            Value := SWord (Raw);
         end if;
         Success := True;
      else
         Value := 0;
         Success := False;
      end if;
   end Load_Byte_Signed;

   ---------------------------------------------------------------------------
   --  Half-Word Load Operations
   ---------------------------------------------------------------------------

   procedure Load_Half (
      Memory  : in     Contract_Memory;
      Addr    : in     Word;
      Value   : out    Word;
      Success : out    Boolean
   ) is
      Lo, Hi : RV_Byte;
   begin
      --  Check alignment and bounds
      if Addr mod 2 /= 0 or not Check_Bounds (Addr, 2) then
         Value := 0;
         Success := False;
         return;
      end if;

      --  Little-endian load
      Lo := Memory (Natural (Addr));
      Hi := Memory (Natural (Addr) + 1);
      Value := Word (Lo) or (Word (Hi) * 256);
      Success := True;
   end Load_Half;

   procedure Load_Half_Signed (
      Memory  : in     Contract_Memory;
      Addr    : in     Word;
      Value   : out    SWord;
      Success : out    Boolean
   ) is
      Raw_Value : Word;
   begin
      Load_Half (Memory, Addr, Raw_Value, Success);
      if Success then
         --  Sign extend from bit 15
         if Raw_Value >= 32768 then
            Value := SWord (Raw_Value) - 65536;
         else
            Value := SWord (Raw_Value);
         end if;
      else
         Value := 0;
      end if;
   end Load_Half_Signed;

   ---------------------------------------------------------------------------
   --  Word Load Operations
   ---------------------------------------------------------------------------

   procedure Load_Word (
      Memory  : in     Contract_Memory;
      Addr    : in     Word;
      Value   : out    Word;
      Success : out    Boolean
   ) is
      B0, B1, B2, B3 : RV_Byte;
   begin
      --  Check alignment and bounds
      if Addr mod 4 /= 0 or not Check_Bounds (Addr, 4) then
         Value := 0;
         Success := False;
         return;
      end if;

      --  Little-endian load
      B0 := Memory (Natural (Addr));
      B1 := Memory (Natural (Addr) + 1);
      B2 := Memory (Natural (Addr) + 2);
      B3 := Memory (Natural (Addr) + 3);

      Value := Word (B0) or
               (Word (B1) * 256) or
               (Word (B2) * 65536) or
               (Word (B3) * 16777216);
      Success := True;
   end Load_Word;

   ---------------------------------------------------------------------------
   --  Byte Store Operations
   ---------------------------------------------------------------------------

   procedure Store_Byte (
      Memory  : in Out Contract_Memory;
      Addr    : in     Word;
      Value   : in     Word;
      Success : out    Boolean
   ) is
   begin
      if Check_Bounds (Addr, 1) then
         Memory (Natural (Addr)) := RV_Byte (Value and 16#FF#);
         Success := True;
      else
         Success := False;
      end if;
   end Store_Byte;

   ---------------------------------------------------------------------------
   --  Half-Word Store Operations
   ---------------------------------------------------------------------------

   procedure Store_Half (
      Memory  : in Out Contract_Memory;
      Addr    : in     Word;
      Value   : in     Word;
      Success : out    Boolean
   ) is
   begin
      --  Check alignment and bounds
      if Addr mod 2 /= 0 or not Check_Bounds (Addr, 2) then
         Success := False;
         return;
      end if;

      --  Little-endian store
      Memory (Natural (Addr))     := RV_Byte (Value and 16#FF#);
      Memory (Natural (Addr) + 1) := RV_Byte ((Value / 256) and 16#FF#);
      Success := True;
   end Store_Half;

   ---------------------------------------------------------------------------
   --  Word Store Operations
   ---------------------------------------------------------------------------

   procedure Store_Word (
      Memory  : in Out Contract_Memory;
      Addr    : in     Word;
      Value   : in     Word;
      Success : out    Boolean
   ) is
   begin
      --  Check alignment and bounds
      if Addr mod 4 /= 0 or not Check_Bounds (Addr, 4) then
         Success := False;
         return;
      end if;

      --  Little-endian store
      Memory (Natural (Addr))     := RV_Byte (Value and 16#FF#);
      Memory (Natural (Addr) + 1) := RV_Byte ((Value / 256) and 16#FF#);
      Memory (Natural (Addr) + 2) := RV_Byte ((Value / 65536) and 16#FF#);
      Memory (Natural (Addr) + 3) := RV_Byte ((Value / 16777216) and 16#FF#);
      Success := True;
   end Store_Word;

   ---------------------------------------------------------------------------
   --  Instruction Fetch
   ---------------------------------------------------------------------------

   procedure Fetch_Instruction (
      Memory  : in     Contract_Memory;
      PC      : in     Program_Counter;
      Bounds  : in     Memory_Bounds;
      Instr   : out    Word;
      Success : out    Boolean
   ) is
   begin
      --  Check PC is in code section and aligned
      if PC < Bounds.Code_Start or PC >= Bounds.Code_End then
         Instr := 0;
         Success := False;
         return;
      end if;

      --  Fetch 4-byte instruction
      Load_Word (Memory, PC, Instr, Success);
   end Fetch_Instruction;

   ---------------------------------------------------------------------------
   --  Block Memory Operations
   ---------------------------------------------------------------------------

   procedure Memory_Read_Block (
      Memory  : in     Contract_Memory;
      Addr    : in     Word;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   ) is
      Base : Natural;
   begin
      if not Check_Bounds (Addr, Buffer'Length) then
         Buffer := (others => 0);
         Success := False;
         return;
      end if;

      Base := Natural (Addr);
      for I in Buffer'Range loop
         Buffer (I) := Memory (Base + I - Buffer'First);
      end loop;
      Success := True;
   end Memory_Read_Block;

   procedure Memory_Write_Block (
      Memory  : in Out Contract_Memory;
      Addr    : in     Word;
      Buffer  : in     Byte_Array;
      Success : out    Boolean
   ) is
      Base : Natural;
   begin
      if not Check_Bounds (Addr, Buffer'Length) then
         Success := False;
         return;
      end if;

      Base := Natural (Addr);
      for I in Buffer'Range loop
         Memory (Base + I - Buffer'First) := Buffer (I);
      end loop;
      Success := True;
   end Memory_Write_Block;

   ---------------------------------------------------------------------------
   --  Memory Initialization
   ---------------------------------------------------------------------------

   procedure Zero_Memory (
      Memory  : in Out Contract_Memory;
      Addr    : in     Word;
      Size    : in     Natural;
      Success : out    Boolean
   ) is
      Base : Natural;
   begin
      if Size = 0 then
         Success := True;
         return;
      end if;

      if not Check_Bounds (Addr, Size) then
         Success := False;
         return;
      end if;

      Base := Natural (Addr);
      for I in Base .. Base + Size - 1 loop
         Memory (I) := 0;
      end loop;
      Success := True;
   end Zero_Memory;

   procedure Initialize_Memory (
      Memory    : out    Contract_Memory;
      Code      : in     Byte_Array;
      Code_Base : in     Word;
      Data      : in     Byte_Array;
      Data_Base : in     Word;
      Success   : out    Boolean
   ) is
      Code_OK, Data_OK : Boolean;
   begin
      --  Zero entire memory first
      Memory := (others => 0);

      --  Check bounds for code section
      if not Check_Bounds (Code_Base, Code'Length) then
         Success := False;
         return;
      end if;

      --  Check bounds for data section
      if not Check_Bounds (Data_Base, Data'Length) then
         Success := False;
         return;
      end if;

      --  Copy code section
      Memory_Write_Block (Memory, Code_Base, Code, Code_OK);
      if not Code_OK then
         Success := False;
         return;
      end if;

      --  Copy data section
      Memory_Write_Block (Memory, Data_Base, Data, Data_OK);
      Success := Data_OK;
   end Initialize_Memory;

   ---------------------------------------------------------------------------
   --  Stack Operations
   ---------------------------------------------------------------------------

   procedure Push (
      Memory  : in Out Contract_Memory;
      SP      : in Out Word;
      Value   : in     Word;
      Bounds  : in     Memory_Bounds;
      Success : out    Boolean
   ) is
      New_SP : Word;
   begin
      --  Check stack underflow
      if SP < 4 then
         Success := False;
         return;
      end if;

      New_SP := SP - 4;

      --  Check stack bounds
      if New_SP < Bounds.Stack_Start then
         Success := False;
         return;
      end if;

      --  Store value at new SP
      Store_Word (Memory, New_SP, Value, Success);
      if Success then
         SP := New_SP;
      end if;
   end Push;

   procedure Pop (
      Memory  : in     Contract_Memory;
      SP      : in Out Word;
      Value   : out    Word;
      Bounds  : in     Memory_Bounds;
      Success : out    Boolean
   ) is
   begin
      --  Check stack overflow
      if SP > Bounds.Stack_End - 4 then
         Value := 0;
         Success := False;
         return;
      end if;

      --  Load value from SP
      Load_Word (Memory, SP, Value, Success);
      if Success then
         SP := SP + 4;
      end if;
   end Pop;

end Sphinx_RV32_Memory;
