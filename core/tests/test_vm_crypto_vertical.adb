--  Vertical Slice Test: SHA3 Syscall Through VM Execution Context
--
--  This test verifies the full path from VM execution context through
--  the crypto API to the SPARK-verified SHA3 implementation.
--
--  Path tested:
--  1. Execution_Context → Aegis_Crypto_API.SHA3_256_Hash
--  2. Aegis_Crypto_API → Anubis_SHA3.SHA3_256
--  3. Result verified against NIST FIPS 202 test vectors

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces;            use Interfaces;
with Aegis_VM_Types;        use Aegis_VM_Types;
with Aegis_Execution;       use Aegis_Execution;
with Aegis_Crypto_API;      use Aegis_Crypto_API;

procedure Test_VM_Crypto_Vertical is

   ---------------------------------------------------------------------------
   --  Test Result Tracking
   ---------------------------------------------------------------------------

   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;
   Failed_Tests : Natural := 0;

   procedure Report (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("  [PASS] " & Name);
      else
         Failed_Tests := Failed_Tests + 1;
         Put_Line ("  [FAIL] " & Name);
      end if;
   end Report;

   ---------------------------------------------------------------------------
   --  Hex Utilities
   ---------------------------------------------------------------------------

   function Hex_Digit (C : Character) return Byte is
   begin
      case C is
         when '0' .. '9' => return Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' => return Character'Pos (C) - Character'Pos ('a') + 10;
         when 'A' .. 'F' => return Character'Pos (C) - Character'Pos ('A') + 10;
         when others => return 0;
      end case;
   end Hex_Digit;

   function Hex_To_Byte (C1, C2 : Character) return Byte is
   begin
      return Shift_Left (Hex_Digit (C1), 4) or Hex_Digit (C2);
   end Hex_To_Byte;

   function Bytes_To_Hex (Bytes : Hash256) return String is
      Hex_Chars : constant String := "0123456789abcdef";
      Result : String (1 .. 64);
      Pos : Natural := 1;
   begin
      for I in Bytes'Range loop
         Result (Pos) := Hex_Chars (Natural (Shift_Right (Bytes (I), 4)) + 1);
         Result (Pos + 1) := Hex_Chars (Natural (Bytes (I) and 16#0F#) + 1);
         Pos := Pos + 2;
      end loop;
      return Result;
   end Bytes_To_Hex;

   ---------------------------------------------------------------------------
   --  Test: SHA3-256 via Execution Context
   ---------------------------------------------------------------------------

   procedure Test_SHA3_256_Via_Context is
      --  Create execution context with 10M gas
      Ctx : Execution_Context := Create_Context (
         Origin       => Address_Zero,
         Gas_Limit    => 10_000_000,
         Gas_Price    => U256_Zero,
         Block_Number => U256_Zero,
         Timestamp    => U256_Zero,
         Chain_ID     => U256_One,
         Certification => Gold
      );

      Input_Buffer : Hash_Input_Buffer := (others => 0);
      Output       : Hash256;
      Result       : Crypto_Result;

      --  NIST FIPS 202 test vectors
      --  SHA3-256("") = a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a
      Empty_Expected : constant Hash256 := (
         16#a7#, 16#ff#, 16#c6#, 16#f8#, 16#bf#, 16#1e#, 16#d7#, 16#66#,
         16#51#, 16#c1#, 16#47#, 16#56#, 16#a0#, 16#61#, 16#d6#, 16#62#,
         16#f5#, 16#80#, 16#ff#, 16#4d#, 16#e4#, 16#3b#, 16#49#, 16#fa#,
         16#82#, 16#d8#, 16#0a#, 16#4b#, 16#80#, 16#f8#, 16#43#, 16#4a#
      );

      --  SHA3-256("abc") = 3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532
      ABC_Expected : constant Hash256 := (
         16#3a#, 16#98#, 16#5d#, 16#a7#, 16#4f#, 16#e2#, 16#25#, 16#b2#,
         16#04#, 16#5c#, 16#17#, 16#2d#, 16#6b#, 16#d3#, 16#90#, 16#bd#,
         16#85#, 16#5f#, 16#08#, 16#6e#, 16#3e#, 16#9d#, 16#52#, 16#5b#,
         16#46#, 16#bf#, 16#e2#, 16#45#, 16#11#, 16#43#, 16#15#, 16#32#
      );

   begin
      Put_Line ("Testing SHA3-256 via Execution Context...");

      --  Test 1: Empty message
      SHA3_256_Hash (Ctx, Input_Buffer, 0, Output, Result);
      Report ("SHA3-256 empty message returns OK",
              Result = Crypto_OK);
      Report ("SHA3-256 empty message correct hash",
              Output = Empty_Expected);

      if Output /= Empty_Expected then
         Put_Line ("    Expected: " & Bytes_To_Hex (Empty_Expected));
         Put_Line ("    Got:      " & Bytes_To_Hex (Output));
      end if;

      --  Test 2: "abc" (0x616263)
      Input_Buffer (0) := 16#61#;  -- 'a'
      Input_Buffer (1) := 16#62#;  -- 'b'
      Input_Buffer (2) := 16#63#;  -- 'c'

      SHA3_256_Hash (Ctx, Input_Buffer, 3, Output, Result);
      Report ("SHA3-256 'abc' returns OK",
              Result = Crypto_OK);
      Report ("SHA3-256 'abc' correct hash",
              Output = ABC_Expected);

      if Output /= ABC_Expected then
         Put_Line ("    Expected: " & Bytes_To_Hex (ABC_Expected));
         Put_Line ("    Got:      " & Bytes_To_Hex (Output));
      end if;

      --  Test 3: Gas was consumed
      declare
         Remaining : constant Gas_Amount := Gas_Remaining (Ctx);
      begin
         Report ("Gas was consumed during hashing",
                 Remaining < 10_000_000);
         Put_Line ("    Gas remaining: " & Gas_Amount'Image (Remaining));
      end;

   end Test_SHA3_256_Via_Context;

   ---------------------------------------------------------------------------
   --  Test: SHA3-512 via Execution Context
   ---------------------------------------------------------------------------

   procedure Test_SHA3_512_Via_Context is
      Ctx : Execution_Context := Create_Context (
         Origin       => Address_Zero,
         Gas_Limit    => 10_000_000,
         Gas_Price    => U256_Zero,
         Block_Number => U256_Zero,
         Timestamp    => U256_Zero,
         Chain_ID     => U256_One,
         Certification => Gold
      );

      Input_Buffer : Hash_Input_Buffer := (others => 0);
      Output       : Hash512;
      Result       : Crypto_Result;

      --  SHA3-512("") = a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a6
      --                 15b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26
      Empty_Expected : constant Hash512 := (
         16#a6#, 16#9f#, 16#73#, 16#cc#, 16#a2#, 16#3a#, 16#9a#, 16#c5#,
         16#c8#, 16#b5#, 16#67#, 16#dc#, 16#18#, 16#5a#, 16#75#, 16#6e#,
         16#97#, 16#c9#, 16#82#, 16#16#, 16#4f#, 16#e2#, 16#58#, 16#59#,
         16#e0#, 16#d1#, 16#dc#, 16#c1#, 16#47#, 16#5c#, 16#80#, 16#a6#,
         16#15#, 16#b2#, 16#12#, 16#3a#, 16#f1#, 16#f5#, 16#f9#, 16#4c#,
         16#11#, 16#e3#, 16#e9#, 16#40#, 16#2c#, 16#3a#, 16#c5#, 16#58#,
         16#f5#, 16#00#, 16#19#, 16#9d#, 16#95#, 16#b6#, 16#d3#, 16#e3#,
         16#01#, 16#75#, 16#85#, 16#86#, 16#28#, 16#1d#, 16#cd#, 16#26#
      );

   begin
      Put_Line ("Testing SHA3-512 via Execution Context...");

      --  Test: Empty message
      SHA3_512_Hash (Ctx, Input_Buffer, 0, Output, Result);
      Report ("SHA3-512 empty message returns OK",
              Result = Crypto_OK);
      Report ("SHA3-512 empty message correct hash",
              Output = Empty_Expected);

   end Test_SHA3_512_Via_Context;

   ---------------------------------------------------------------------------
   --  Test: Gas Exhaustion
   ---------------------------------------------------------------------------

   procedure Test_Gas_Exhaustion is
      --  Create context with minimal gas
      Ctx : Execution_Context := Create_Context (
         Origin       => Address_Zero,
         Gas_Limit    => 1,  -- Just 1 gas unit
         Gas_Price    => U256_Zero,
         Block_Number => U256_Zero,
         Timestamp    => U256_Zero,
         Chain_ID     => U256_One,
         Certification => Bronze
      );

      Input_Buffer : Hash_Input_Buffer := (others => 0);
      Output       : Hash256;
      Result       : Crypto_Result;

   begin
      Put_Line ("Testing gas exhaustion...");

      --  Try to hash with insufficient gas
      SHA3_256_Hash (Ctx, Input_Buffer, 0, Output, Result);
      Report ("SHA3-256 with 1 gas returns out of gas",
              Result = Crypto_Out_Of_Gas);

   end Test_Gas_Exhaustion;

   ---------------------------------------------------------------------------
   --  Test: Certification Discount
   ---------------------------------------------------------------------------

   procedure Test_Certification_Discount is
      --  Bronze: no discount (100%)
      Ctx_Bronze : Execution_Context := Create_Context (
         Origin       => Address_Zero,
         Gas_Limit    => 1_000_000,
         Gas_Price    => U256_Zero,
         Block_Number => U256_Zero,
         Timestamp    => U256_Zero,
         Chain_ID     => U256_One,
         Certification => Bronze
      );

      --  Gold: 20% discount (80%)
      Ctx_Gold : Execution_Context := Create_Context (
         Origin       => Address_Zero,
         Gas_Limit    => 1_000_000,
         Gas_Price    => U256_Zero,
         Block_Number => U256_Zero,
         Timestamp    => U256_Zero,
         Chain_ID     => U256_One,
         Certification => Gold
      );

      Input_Buffer : Hash_Input_Buffer := (others => 0);
      Output       : Hash256;
      Result       : Crypto_Result;

      Gas_Bronze, Gas_Gold : Gas_Amount;

   begin
      Put_Line ("Testing certification discount...");

      --  Hash with Bronze
      SHA3_256_Hash (Ctx_Bronze, Input_Buffer, 100, Output, Result);
      Gas_Bronze := 1_000_000 - Gas_Remaining (Ctx_Bronze);

      --  Hash with Gold (should use less gas)
      SHA3_256_Hash (Ctx_Gold, Input_Buffer, 100, Output, Result);
      Gas_Gold := 1_000_000 - Gas_Remaining (Ctx_Gold);

      Put_Line ("    Bronze gas used: " & Gas_Amount'Image (Gas_Bronze));
      Put_Line ("    Gold gas used:   " & Gas_Amount'Image (Gas_Gold));

      --  Gold should use less gas (20% discount)
      Report ("Gold uses less gas than Bronze",
              Gas_Gold < Gas_Bronze);

   end Test_Certification_Discount;

   ---------------------------------------------------------------------------
   --  Main
   ---------------------------------------------------------------------------

begin
   Put_Line ("===========================================");
   Put_Line ("  Vertical Slice Test: VM Crypto Path");
   Put_Line ("===========================================");
   Put_Line ("  Testing: Execution_Context -> Aegis_Crypto_API -> Anubis_SHA3");
   New_Line;

   Test_SHA3_256_Via_Context;
   New_Line;

   Test_SHA3_512_Via_Context;
   New_Line;

   Test_Gas_Exhaustion;
   New_Line;

   Test_Certification_Discount;
   New_Line;

   --  Summary
   Put_Line ("===========================================");
   Put_Line ("  Test Summary");
   Put_Line ("===========================================");
   Put_Line ("  Total:  " & Natural'Image (Total_Tests));
   Put_Line ("  Passed: " & Natural'Image (Passed_Tests));
   Put_Line ("  Failed: " & Natural'Image (Failed_Tests));
   New_Line;

   if Failed_Tests > 0 then
      Put_Line ("  SOME TESTS FAILED!");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("  ALL TESTS PASSED!");
      Put_Line ("  Vertical slice verified: VM context -> Crypto API -> SPARK crypto");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;

end Test_VM_Crypto_Vertical;
