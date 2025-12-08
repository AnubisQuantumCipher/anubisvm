pragma SPARK_Mode (Off);  --  Test code doesn't need SPARK

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces; use Interfaces;

with Anubis_Address; use Anubis_Address;
with Anubis_Address_Types; use Anubis_Address_Types;
with Anubis_Address_Derive; use Anubis_Address_Derive;

--  Test_Address: Test suite for ANUBIS Address Standard (AAS-001 v3.1)
--
--  Tests:
--  1. Address creation from public key
--  2. Address formatting to string
--  3. Address parsing from string
--  4. Checksum validation
--  5. Round-trip (create → format → parse → validate)

procedure Test_Address is

   --  Test counter
   Tests_Run    : Natural := 0;
   Tests_Passed : Natural := 0;

   --  Report test result
   procedure Report_Test (Name : String; Passed : Boolean) is
   begin
      Tests_Run := Tests_Run + 1;
      if Passed then
         Tests_Passed := Tests_Passed + 1;
         Put_Line ("[PASS] " & Name);
      else
         Put_Line ("[FAIL] " & Name);
      end if;
   end Report_Test;

   --  Test 1: Create address from mock public key
   procedure Test_Create_Address is
      --  Mock ML-DSA-87 public key (32 bytes for simplicity)
      Public_Key : Public_Key_Bytes (0 .. 31) := (others => 16#42#);
      Addr       : Address;
   begin
      Addr := Create_Address (ML_DSA_87, Main, User, Public_Key);
      Report_Test ("Create address from public key", Addr.Valid);
   end Test_Create_Address;

   --  Test 2: Format address to string
   procedure Test_Format_Address is
      Public_Key : Public_Key_Bytes (0 .. 31) := (others => 16#42#);
      Addr       : Address;
      Output     : Address_String;
      Length     : Natural;
   begin
      Addr := Create_Address (ML_DSA_87, Main, User, Public_Key);
      Format_Address (Addr, Output, Length);

      Put_Line ("Formatted address (" & Natural'Image (Length) & " chars):");
      Put_Line (Output (1 .. Length));

      --  Check expected format components
      Report_Test ("Format address to string",
                   Length > 70 and then
                   Output (1 .. 7) = "mldsa87" and then
                   Output (8) = ':');
   end Test_Format_Address;

   --  Test 3: Round-trip (create → format → parse)
   procedure Test_Round_Trip is
      Public_Key : Public_Key_Bytes (0 .. 31) := (others => 16#99#);
      Addr1      : Address;
      Addr2      : Address;
      Output     : Address_String;
      Length     : Natural;
      Match      : Boolean := True;
   begin
      --  Create and format
      Addr1 := Create_Address (ML_DSA_87, Test, Contract, Public_Key);
      Format_Address (Addr1, Output, Length);

      --  Parse back
      Parse_Address (Output (1 .. Length), Addr2);

      --  Verify parsed address matches
      if not Addr2.Valid then
         Match := False;
      elsif Addr2.Algorithm /= Addr1.Algorithm then
         Match := False;
      elsif Addr2.Network /= Addr1.Network then
         Match := False;
      elsif Addr2.Entity /= Addr1.Entity then
         Match := False;
      else
         --  Check account ID bytes match
         for I in Account_ID_Index loop
            if Addr2.Account (I) /= Addr1.Account (I) then
               Match := False;
               exit;
            end if;
         end loop;
      end if;

      Report_Test ("Round-trip (create→format→parse)", Match);
   end Test_Round_Trip;

   --  Test 4: Validate address
   procedure Test_Validate_Address is
      Public_Key : Public_Key_Bytes (0 .. 31) := (others => 16#AA#);
      Addr       : Address;
      Valid      : Boolean;
   begin
      Addr := Create_Address (ML_DSA_65, Dev, Validator, Public_Key);
      Valid := Validate_Address (Addr);
      Report_Test ("Validate address checksum", Valid);
   end Test_Validate_Address;

   --  Test 5: Parse invalid address (bad checksum)
   procedure Test_Invalid_Checksum is
      --  Valid format but with intentionally wrong checksum (99999 instead of computed)
      Invalid_Addr : constant String :=
         "mldsa87:main:u:01234567-89ABCDEF-01234567-89ABCDEF-01234567-89ABCDEF-01234567-89AB-99999";
      Addr : Address;
   begin
      Parse_Address (Invalid_Addr, Addr);
      Report_Test ("Reject invalid checksum", not Addr.Valid);
   end Test_Invalid_Checksum;

   --  Test 6: Different networks
   procedure Test_Different_Networks is
      Public_Key : Public_Key_Bytes (0 .. 31) := (others => 16#FF#);
      Addr_Main    : Address;
      Addr_Test    : Address;
      Addr_Dev     : Address;
      Addr_Lab     : Address;
      Addr_Staging : Address;
      Output       : Address_String;
      Length       : Natural;
   begin
      Addr_Main := Create_Address (ML_DSA_87, Main, User, Public_Key);
      Addr_Test := Create_Address (ML_DSA_87, Test, User, Public_Key);
      Addr_Dev := Create_Address (ML_DSA_87, Dev, User, Public_Key);
      Addr_Lab := Create_Address (ML_DSA_87, Lab, User, Public_Key);
      Addr_Staging := Create_Address (ML_DSA_87, Staging, User, Public_Key);

      Format_Address (Addr_Main, Output, Length);
      Put_Line ("Main network: " & Output (1 .. Length));

      Format_Address (Addr_Test, Output, Length);
      Put_Line ("Test network: " & Output (1 .. Length));

      Report_Test ("Different networks",
                   Addr_Main.Valid and Addr_Test.Valid and Addr_Dev.Valid and
                   Addr_Lab.Valid and Addr_Staging.Valid);
   end Test_Different_Networks;

   --  Test 7: Different entity types
   procedure Test_Different_Entities is
      Public_Key : Public_Key_Bytes (0 .. 31) := (others => 16#CC#);
      Addr_User      : Address;
      Addr_Contract  : Address;
      Addr_Validator : Address;
      Addr_System    : Address;
   begin
      Addr_User := Create_Address (ML_DSA_87, Main, User, Public_Key);
      Addr_Contract := Create_Address (ML_DSA_87, Main, Contract, Public_Key);
      Addr_Validator := Create_Address (ML_DSA_87, Main, Validator, Public_Key);
      Addr_System := Create_Address (ML_DSA_87, Main, System, Public_Key);

      --  Domain separation ensures different account IDs for different entity types
      --  Check that account IDs are NOT identical (at least one byte differs)
      declare
         Same : Boolean := True;
      begin
         for I in Account_ID_Index loop
            if Addr_User.Account (I) /= Addr_Contract.Account (I) then
               Same := False;  --  Found a differing byte
               exit;
            end if;
         end loop;

         Report_Test ("Domain separation (different entities)", not Same);
      end;
   end Test_Different_Entities;

begin
   Put_Line ("========================================");
   Put_Line ("  ANUBIS Address Standard Test Suite   ");
   Put_Line ("  AAS-001 v3.1                          ");
   Put_Line ("========================================");
   New_Line;

   Test_Create_Address;
   Test_Format_Address;
   Test_Round_Trip;
   Test_Validate_Address;
   Test_Invalid_Checksum;
   Test_Different_Networks;
   Test_Different_Entities;

   New_Line;
   Put_Line ("========================================");
   Put ("Tests run: "); Put (Tests_Run, 0); New_Line;
   Put ("Tests passed: "); Put (Tests_Passed, 0); New_Line;
   Put ("Tests failed: "); Put (Tests_Run - Tests_Passed, 0); New_Line;
   Put_Line ("========================================");

   if Tests_Passed = Tests_Run then
      Put_Line ("SUCCESS: All tests passed!");
   else
      Put_Line ("FAILURE: Some tests failed.");
   end if;

end Test_Address;
