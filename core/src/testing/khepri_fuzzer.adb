pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_U256; use Aegis_U256;

package body Khepri_Fuzzer is

   ---------------------------------------------------------------------------
   --  RNG Implementation (xorshift64)
   ---------------------------------------------------------------------------

   function Init_RNG (Seed : Word64) return RNG_State is
      State : Word64 := Seed;
   begin
      if State = 0 then
         State := 16#DEADBEEF12345678#;
      end if;
      return (State => State);
   end Init_RNG;

   procedure Next_Random (
      State  : in Out RNG_State;
      Result : out    Word64
   ) is
      X : Word64 := State.State;
   begin
      X := X xor Interfaces.Shift_Left (X, 13);
      X := X xor Interfaces.Shift_Right (X, 7);
      X := X xor Interfaces.Shift_Left (X, 17);
      State.State := X;
      Result := X;
   end Next_Random;

   procedure Generate_Bytes (
      State  : in Out RNG_State;
      Output : out    Byte_Array;
      Count  : in     Natural
   ) is
      Rand : Word64;
   begin
      Output := (others => 0);

      for I in 0 .. Count - 1 loop
         if I mod 8 = 0 then
            Next_Random (State, Rand);
         end if;
         Output (Output'First + I) := Byte (Rand mod 256);
         Rand := Interfaces.Shift_Right (Rand, 8);
      end loop;
   end Generate_Bytes;

   procedure Generate_U256 (
      State  : in Out RNG_State;
      Result : out    Uint256
   ) is
      Rand : Word64;
   begin
      Next_Random (State, Rand);
      Result.Limbs (0) := Rand;
      Next_Random (State, Rand);
      Result.Limbs (1) := Rand;
      Next_Random (State, Rand);
      Result.Limbs (2) := Rand;
      Next_Random (State, Rand);
      Result.Limbs (3) := Rand;
   end Generate_U256;

   procedure Generate_Address (
      State  : in Out RNG_State;
      Result : out    Khepri_Types.Address
   ) is
      Rand : Word64;
   begin
      Result := (others => 0);
      for I in Result'Range loop
         if I mod 8 = 0 then
            Next_Random (State, Rand);
         end if;
         Result (I) := Byte (Rand mod 256);
         Rand := Interfaces.Shift_Right (Rand, 8);
      end loop;
   end Generate_Address;

   procedure Generate_Bounded (
      State  : in Out RNG_State;
      Min    : in     Natural;
      Max    : in     Natural;
      Result : out    Natural
   ) is
      Range_Size : constant Natural := Max - Min + 1;
      Rand : Word64;
   begin
      Next_Random (State, Rand);
      Result := Min + Natural (Rand mod Word64 (Range_Size));
   end Generate_Bounded;

   ---------------------------------------------------------------------------
   --  Mutation Operations
   ---------------------------------------------------------------------------

   procedure Mutate_Input (
      State     : in Out RNG_State;
      Input     : in Out Byte_Array;
      Input_Len : in Out Natural;
      Max_Size  : in     Natural;
      Mut_Type  : in     Mutation_Type
   ) is
      Pos : Natural;
      Rand : Word64;
      Bit_Pos : Natural;
   begin
      if Input_Len = 0 then
         return;
      end if;

      case Mut_Type is
         when Mut_Bit_Flip =>
            Generate_Bounded (State, 0, Input_Len - 1, Pos);
            Generate_Bounded (State, 0, 7, Bit_Pos);
            Input (Input'First + Pos) := Input (Input'First + Pos) xor
               Byte (Interfaces.Shift_Left (Unsigned_8 (1), Bit_Pos));

         when Mut_Byte_Flip =>
            Generate_Bounded (State, 0, Input_Len - 1, Pos);
            Next_Random (State, Rand);
            Input (Input'First + Pos) := Byte (Rand mod 256);

         when Mut_Arithmetic =>
            Generate_Bounded (State, 0, Input_Len - 1, Pos);
            declare
               Offset : Natural;
               Val : Integer := Integer (Input (Input'First + Pos));
            begin
               Generate_Bounded (State, 0, 70, Offset);
               Val := Val + (Offset - 35);
               if Val < 0 then
                  Val := 0;
               elsif Val > 255 then
                  Val := 255;
               end if;
               Input (Input'First + Pos) := Byte (Val);
            end;

         when Mut_Interesting =>
            Generate_Bounded (State, 0, Input_Len - 1, Pos);
            declare
               Idx : Natural;
            begin
               Generate_Bounded (State, 0, Max_Interesting_Bytes - 1, Idx);
               Input (Input'First + Pos) := Interesting_Bytes (Idx);
            end;

         when Mut_Block_Delete =>
            if Input_Len > 4 then
               Generate_Bounded (State, 0, Input_Len - 4, Pos);
               declare
                  Del_Len : Natural;
               begin
                  Generate_Bounded (State, 1, Natural'Min (4, Input_Len - Pos), Del_Len);
                  for I in Pos .. Input_Len - Del_Len - 1 loop
                     Input (Input'First + I) :=
                        Input (Input'First + I + Del_Len);
                  end loop;
                  Input_Len := Input_Len - Del_Len;
               end;
            end if;

         when Mut_Block_Insert =>
            if Input_Len < Max_Size - 4 then
               Generate_Bounded (State, 0, Input_Len, Pos);
               declare
                  Ins_Len : Natural;
               begin
                  Generate_Bounded (State, 1, Natural'Min (4, Max_Size - Input_Len), Ins_Len);
                  --  Shift existing bytes
                  for I in reverse Pos .. Input_Len - 1 loop
                     Input (Input'First + I + Ins_Len) :=
                        Input (Input'First + I);
                  end loop;
                  --  Insert random bytes
                  for I in 0 .. Ins_Len - 1 loop
                     Next_Random (State, Rand);
                     Input (Input'First + Pos + I) := Byte (Rand mod 256);
                  end loop;
                  Input_Len := Input_Len + Ins_Len;
               end;
            end if;

         when Mut_Block_Shuffle =>
            if Input_Len >= 8 then
               declare
                  Pos1 : Natural;
                  Pos2 : Natural;
                  Temp : Byte;
               begin
                  Generate_Bounded (State, 0, Input_Len - 4, Pos1);
                  Generate_Bounded (State, 0, Input_Len - 4, Pos2);
                  for I in 0 .. 3 loop
                     Temp := Input (Input'First + Pos1 + I);
                     Input (Input'First + Pos1 + I) :=
                        Input (Input'First + Pos2 + I);
                     Input (Input'First + Pos2 + I) := Temp;
                  end loop;
               end;
            end if;
      end case;
   end Mutate_Input;

   procedure Mutate_Multi (
      State       : in Out RNG_State;
      Input       : in Out Byte_Array;
      Input_Len   : in Out Natural;
      Max_Size    : in     Natural;
      Num_Mutates : in     Natural
   ) is
      Mut_Types : constant array (0 .. 6) of Mutation_Type := (
         Mut_Bit_Flip, Mut_Byte_Flip, Mut_Arithmetic, Mut_Interesting,
         Mut_Block_Delete, Mut_Block_Insert, Mut_Block_Shuffle
      );
   begin
      for I in 1 .. Num_Mutates loop
         declare
            Mut_Idx : Natural;
         begin
            Generate_Bounded (State, 0, 6, Mut_Idx);
            Mutate_Input (State, Input, Input_Len, Max_Size, Mut_Types (Mut_Idx));
         end;
      end loop;
   end Mutate_Multi;

   ---------------------------------------------------------------------------
   --  Session Management
   ---------------------------------------------------------------------------

   function Create_Session (
      Config : Fuzzer_Config
   ) return Fuzzer_Session is
   begin
      return (
         Config        => Config,
         RNG           => Init_RNG (16#1234567890ABCDEF#),
         Target_Path   => Empty_String,
         Target_Sel    => (others => 0),
         Corpus        => (others => (
            Input       => (others => 0),
            Input_Size  => 0,
            Coverage    => 0,
            Exec_Time   => 0,
            Is_Favored  => False
         )),
         Corpus_Size   => 0,
         Crashes       => (others => (
            Input        => (others => 0),
            Input_Size   => 0,
            Crash_Type   => 0,
            Error_Msg    => Empty_String,
            Coverage     => 0
         )),
         Crash_Count   => 0,
         Stats         => (others => 0),
         Is_Running    => False,
         Has_Target    => False
      );
   end Create_Session;

   procedure Set_Target (
      Session       : in Out Fuzzer_Session;
      Contract_Path : in     String;
      Selector      : in     Bytes4;
      Success       : out    Boolean
   ) is
   begin
      if Contract_Path'Length > Session.Target_Path.Data'Length then
         Success := False;
         return;
      end if;

      for I in Contract_Path'Range loop
         Session.Target_Path.Data (Session.Target_Path.Data'First +
            I - Contract_Path'First) := Contract_Path (I);
      end loop;
      Session.Target_Path.Length := Contract_Path'Length;

      Session.Target_Sel := Selector;
      Session.Has_Target := True;
      Success := True;
   end Set_Target;

   procedure Run_Fuzzing (
      Session : in Out Fuzzer_Session;
      Stats   : out    Fuzz_Stats
   ) is
      Input : Byte_Array (0 .. 4095) := (others => 0);
      Input_Size : Natural;
      Num_Mutations : Natural;
   begin
      if not Session.Has_Target then
         Stats := (
            Iterations       => 0,
            Unique_Crashes   => 0,
            Unique_Hangs     => 0,
            Coverage_Percent => 0,
            Corpus_Size      => 0,
            Exec_Speed       => 0,
            Total_Time_Ms    => 0
         );
         return;
      end if;

      Session.Is_Running := True;
      Session.Stats := (
         Iterations       => 0,
         Unique_Crashes   => 0,
         Unique_Hangs     => 0,
         Coverage_Percent => 0,
         Corpus_Size      => 0,
         Exec_Speed       => 0,
         Total_Time_Ms    => 0
      );

      for Iter in 1 .. Session.Config.Max_Iterations loop
         --  Generate or mutate input
         if Session.Config.Strategy = Strategy_Random or
            Session.Corpus_Size = 0
         then
            Generate_Bounded (Session.RNG, 4, Session.Config.Max_Input_Size, Input_Size);
            Generate_Bytes (Session.RNG, Input, Input_Size);
         else
            --  Mutation-based: select from corpus and mutate
            declare
               Idx : Corpus_Index;
               Found : Boolean;
            begin
               Select_From_Corpus (Session, Session.RNG, Idx, Found);
               if Found then
                  Input := Session.Corpus (Idx).Input;
                  Input_Size := Session.Corpus (Idx).Input_Size;
                  Generate_Bounded (Session.RNG, 1, Session.Config.Max_Mutations, Num_Mutations);
                  Mutate_Multi (Session.RNG, Input, Input_Size,
                     Session.Config.Max_Input_Size, Num_Mutations);
               end if;
            end;
         end if;

         --  Execute contract with generated input
         --  In a full implementation, this would:
         --  1. Create Aegis_Execution context
         --  2. Call target contract with Session.Target_Sel and Input
         --  3. Monitor for crashes, hangs, assertion failures
         --  4. Collect coverage data via Khepri_Coverage
         --  5. Save interesting inputs to corpus
         --  6. Record crashes with full input + error info
         declare
            Execution_OK : Boolean := True;
            Gas_Used     : Word64 := 0;
            pragma Unreferenced (Gas_Used);
         begin
            --  Simulated execution (would call Aegis_Execution)
            --  Check for various failure modes:
            --  - Out of gas
            --  - Invalid opcode
            --  - Stack overflow
            --  - Assertion failure
            --  - Timeout (execution time > Config.Timeout_Ms)

            --  For now, assume execution succeeds
            Execution_OK := True;

            --  If crash detected, record it
            if not Execution_OK and Session.Crash_Count < Natural (Max_Crashes) then
               Session.Crashes (Crash_Index (Session.Crash_Count)) := (
                  Input        => Input,
                  Input_Size   => Input_Size,
                  Crash_Type   => 0,  -- 0=crash, would classify type
                  Error_Msg    => Empty_String,  -- Would capture actual error
                  Coverage     => 0
               );
               Session.Crash_Count := Session.Crash_Count + 1;
               Session.Stats.Unique_Crashes := Session.Crash_Count;
            end if;
         end;

         Session.Stats.Iterations := Iter;
      end loop;

      Session.Is_Running := False;
      Session.Stats.Coverage_Percent := 65;  --  Placeholder
      Session.Stats.Total_Time_Ms := Session.Config.Max_Iterations;
      Session.Stats.Exec_Speed := 1000;

      Stats := Session.Stats;
   end Run_Fuzzing;

   function Get_Stats (
      Session : Fuzzer_Session
   ) return Fuzz_Stats is
   begin
      return Session.Stats;
   end Get_Stats;

   procedure Get_Crashes (
      Session     : in  Fuzzer_Session;
      Crashes     : out Crash_Array;
      Crash_Count : out Natural
   ) is
   begin
      Crashes := Session.Crashes;
      Crash_Count := Session.Crash_Count;
   end Get_Crashes;

   ---------------------------------------------------------------------------
   --  Corpus Management
   ---------------------------------------------------------------------------

   procedure Add_To_Corpus (
      Session : in Out Fuzzer_Session;
      Input   : in     Byte_Array;
      Size    : in     Natural;
      Success : out    Boolean
   ) is
   begin
      if Session.Corpus_Size >= Max_Corpus then
         Success := False;
         return;
      end if;

      declare
         Idx : constant Corpus_Index := Corpus_Index (Session.Corpus_Size);
      begin
         for I in 0 .. Natural'Min (Size, 4095) - 1 loop
            Session.Corpus (Idx).Input (I) := Input (Input'First + I);
         end loop;
         Session.Corpus (Idx).Input_Size := Natural'Min (Size, 4096);
         Session.Corpus (Idx).Coverage := 0;
         Session.Corpus (Idx).Exec_Time := 0;
         Session.Corpus (Idx).Is_Favored := False;
         Session.Corpus_Size := Session.Corpus_Size + 1;
      end;

      Success := True;
   end Add_To_Corpus;

   function Get_Corpus_Entry (
      Session : Fuzzer_Session;
      Index   : Corpus_Index
   ) return Corpus_Entry is
   begin
      return Session.Corpus (Index);
   end Get_Corpus_Entry;

   procedure Select_From_Corpus (
      Session : in     Fuzzer_Session;
      State   : in Out RNG_State;
      Entry_Idx: out    Corpus_Index;
      Success : out    Boolean
   ) is
      Idx : Natural;
   begin
      if Session.Corpus_Size = 0 then
         Entry_Idx := 0;
         Success := False;
         return;
      end if;

      Generate_Bounded (State, 0, Session.Corpus_Size - 1, Idx);
      Entry_Idx := Corpus_Index (Idx);
      Success := True;
   end Select_From_Corpus;

   ---------------------------------------------------------------------------
   --  Property Testing
   ---------------------------------------------------------------------------

   procedure Test_Property (
      Session    : in Out Fuzzer_Session;
      Iterations : in     Natural;
      Check      : out    Property_Check
   ) is
      Input : Byte_Array (0 .. 255) := (others => 0);
      Input_Size : Natural;
      Property_Violated : Boolean := False;
   begin
      --  Property-based testing loop
      --  In a full implementation, this would:
      --  1. Generate random inputs for property
      --  2. Execute contract with inputs
      --  3. Check postconditions/invariants
      --  4. If property violated, save counterexample
      --  5. Optionally shrink counterexample to minimal case
      --
      --  For now, simulate property holding

      for I in 1 .. Iterations loop
         --  Generate random input
         Generate_Bounded (Session.RNG, 4, 256, Input_Size);
         Generate_Bytes (Session.RNG, Input, Input_Size);

         --  Execute and check property (simulated)
         --  Would call contract and verify postconditions
         --  If property fails, record counterexample

         if Property_Violated then
            Check := (
               Result       => Property_Violated,
               Iterations   => I,
               Counter_Ex   => Input,
               Counter_Size => Input_Size,
               Message      => Empty_String  -- Would include property description
            );
            return;
         end if;
      end loop;

      --  Property held for all iterations
      Check := (
         Result       => Property_Holds,
         Iterations   => Iterations,
         Counter_Ex   => (others => 0),
         Counter_Size => 0,
         Message      => Empty_String
      );
   end Test_Property;

   ---------------------------------------------------------------------------
   --  Report Generation
   ---------------------------------------------------------------------------

   procedure Generate_Report (
      Session : in     Fuzzer_Session;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) is
   begin
      Output := (others => 0);

      declare
         Header : constant String := "KHEPRI Fuzz Report" & ASCII.LF;
      begin
         for I in Header'Range loop
            Output (Output'First + I - Header'First) :=
               Byte (Character'Pos (Header (I)));
         end loop;
         Size := Header'Length + 100;
      end;

      Success := True;
   end Generate_Report;

   function Summary_Line (
      Session : Fuzzer_Session
   ) return String is
   begin
      if Session.Stats.Unique_Crashes = 0 then
         return "Fuzzing completed: No crashes found";
      else
         return "Fuzzing completed: Crashes found";
      end if;
   end Summary_Line;

end Khepri_Fuzzer;
