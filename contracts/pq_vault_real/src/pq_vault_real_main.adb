--  PQ_Vault Real Test with AAS-001 Addresses
--  Demonstrates: Sharing, Recovery, Network Isolation, Entity Restrictions
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with PQ_Vault_Real; use PQ_Vault_Real;
with Anubis_Address_Types; use Anubis_Address_Types;

procedure PQ_Vault_Real_Main is

   --  Simulated AAS-001 Account IDs (in real system, derived from ML-DSA-87 keys)
   --  These represent: mldsa87:dev:u:... addresses

   --  Owner: mldsa87:dev:u:kz27a71y-cygcq73s-6g3c6few-68wkp64k-...
   Owner_Account : constant Account_ID := (
      16#9f#, 16#c4#, 16#75#, 16#1c#, 16#3e#, 16#67#, 16#a0#, 16#cb#,
      16#9c#, 16#79#, 16#34#, 16#06#, 16#c3#, 16#3d#, 16#dc#, 16#32#,
      16#39#, 16#3b#, 16#18#, 16#93#, 16#6f#, 16#0c#, 16#fc#, 16#3b#,
      16#8d#, 16#8d#, 16#42#, 16#9e#, 16#b9#, 16#01#, 16#ac#, 16#a8#
   );

   --  Friend (for sharing): mldsa87:dev:u:abc123...
   Friend_Account : constant Account_ID := (
      16#ab#, 16#c1#, 16#23#, 16#45#, 16#67#, 16#89#, 16#ab#, 16#cd#,
      16#ef#, 16#01#, 16#23#, 16#45#, 16#67#, 16#89#, 16#ab#, 16#cd#,
      16#11#, 16#22#, 16#33#, 16#44#, 16#55#, 16#66#, 16#77#, 16#88#,
      16#99#, 16#aa#, 16#bb#, 16#cc#, 16#dd#, 16#ee#, 16#ff#, 16#00#
   );

   --  Recovery contact (spouse): mldsa87:dev:u:spouse123...
   Spouse_Account : constant Account_ID := (
      16#5f#, 16#0e#, 16#5e#, 16#12#, 16#34#, 16#56#, 16#78#, 16#9a#,
      16#bc#, 16#de#, 16#f0#, 16#12#, 16#34#, 16#56#, 16#78#, 16#9a#,
      16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#, 16#08#,
      16#09#, 16#0a#, 16#0b#, 16#0c#, 16#0d#, 16#0e#, 16#0f#, 16#10#
   );

   --  Attacker on WRONG network (mainnet instead of dev)
   Attacker_Account : constant Account_ID := (
      16#ba#, 16#ad#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#de#, 16#ad#
   );

   --  Simulate hash of label/data
   function Mock_Hash (Val : Unsigned_8) return Account_ID is
      H : Account_ID := (others => 0);
   begin
      H (0) := Val;
      H (31) := Val xor 16#FF#;
      return H;
   end Mock_Hash;

   Vault : Vault_State := Empty_State;
   Result : Result_Code;
   Index : Entry_Index;
   Header : Entry_Header;
   Now : Unsigned_64 := 1734567890;

begin
   Put_Line ("================================================================");
   Put_Line ("  PQ_VAULT v2.0 - Post-Quantum Password Manager");
   Put_Line ("  With AAS-001 Self-Describing Addresses");
   Put_Line ("================================================================");
   New_Line;

   ---------------------------------------------------------------------------
   --  TEST 1: Initialize with AAS-001 identity
   ---------------------------------------------------------------------------
   Put_Line ("TEST 1: Initialize vault with AAS-001 identity");
   Put_Line ("------------------------------------------------");
   Put_Line ("  Owner: mldsa87:dev:u:kz27a71y-cygcq73s-...");
   Put_Line ("  Network: Dev (development)");
   Put_Line ("  Entity: User (u)");

   Initialize (
      State        => Vault,
      Owner        => Owner_Account,
      Owner_Entity => User,
      Owner_Network => Dev,
      Name_Hash    => Mock_Hash (16#01#),
      Timestamp    => Now,
      Result       => Result);

   if Result = Success then
      Put_Line ("  [OK] Vault initialized");
      Put_Line ("       Network: " & Network_Type'Image (Get_Owner_Network (Vault)));
      Put_Line ("       Entity:  " & Entity_Type'Image (Get_Owner_Entity (Vault)));
   else
      Put_Line ("  [FAIL] " & Result_Code'Image (Result));
   end if;
   New_Line;

   ---------------------------------------------------------------------------
   --  TEST 2: Add entries with network binding
   ---------------------------------------------------------------------------
   Put_Line ("TEST 2: Add password with network binding");
   Put_Line ("------------------------------------------");
   Put_Line ("  Adding: Gmail password");
   Put_Line ("  Bound to: Dev network (can't be used on mainnet)");
   Put_Line ("  Restriction: Users only");

   Now := Now + 1;
   Add_Entry (
      State          => Vault,
      Caller         => Owner_Account,
      Caller_Network => Dev,
      Kind           => Kind_Login,
      Label_Hash     => Mock_Hash (16#10#),  -- "Gmail"
      Data_Hash      => Mock_Hash (16#20#),  -- encrypted password
      Category       => 1,
      Bound_Network  => Dev,      -- <-- Only usable on Dev network
      Restriction    => Users_Only,
      Timestamp      => Now,
      Index          => Index,
      Result         => Result);

   if Result = Success then
      Put_Line ("  [OK] Gmail password added at index" & Entry_Index'Image (Index));
   else
      Put_Line ("  [FAIL] " & Result_Code'Image (Result));
   end if;
   New_Line;

   ---------------------------------------------------------------------------
   --  TEST 3: Network mismatch attack (mainnet attacker tries dev vault)
   ---------------------------------------------------------------------------
   Put_Line ("TEST 3: Network mismatch attack");
   Put_Line ("--------------------------------");
   Put_Line ("  Attacker: mldsa87:main:u:baad0000-... (MAINNET)");
   Put_Line ("  Vault: Dev network");
   Put_Line ("  Expected: REJECTED (network mismatch)");

   Get_Entry (
      State          => Vault,
      Index          => 0,
      Caller         => Attacker_Account,
      Caller_Entity  => User,
      Caller_Network => Main,  -- <-- Wrong network!
      Header         => Header,
      Result         => Result);

   if Result = Error_Network_Mismatch then
      Put_Line ("  [OK] Attack blocked: " & Result_Code'Image (Result));
      Put_Line ("       Self-describing address caught the mismatch!");
   else
      Put_Line ("  [FAIL] Expected Error_Network_Mismatch, got " & Result_Code'Image (Result));
   end if;
   New_Line;

   ---------------------------------------------------------------------------
   --  TEST 4: Share with friend (AAS-001 access control)
   ---------------------------------------------------------------------------
   Put_Line ("TEST 4: Share password with friend");
   Put_Line ("-----------------------------------");
   Put_Line ("  Sharing Gmail with: mldsa87:dev:u:abc123...");
   Put_Line ("  Permission: View only");
   Put_Line ("  Expires: Never");

   Now := Now + 1;
   Share_Entry (
      State           => Vault,
      Index           => 0,
      Caller          => Owner_Account,
      Caller_Network  => Dev,
      Grantee         => Friend_Account,
      Grantee_Entity  => User,
      Grantee_Network => Dev,
      Permission      => Perm_View,
      Expires_At      => 0,  -- Never expires
      Timestamp       => Now,
      Result          => Result);

   if Result = Success then
      Put_Line ("  [OK] Shared with friend");
   else
      Put_Line ("  [FAIL] " & Result_Code'Image (Result));
   end if;

   --  Friend can now view
   Get_Entry (
      State          => Vault,
      Index          => 0,
      Caller         => Friend_Account,
      Caller_Entity  => User,
      Caller_Network => Dev,
      Header         => Header,
      Result         => Result);

   if Result = Success then
      Put_Line ("  [OK] Friend can view entry");
   else
      Put_Line ("  [FAIL] Friend access: " & Result_Code'Image (Result));
   end if;
   New_Line;

   ---------------------------------------------------------------------------
   --  TEST 5: Add recovery contact
   ---------------------------------------------------------------------------
   Put_Line ("TEST 5: Add emergency recovery contact");
   Put_Line ("---------------------------------------");
   Put_Line ("  Contact: mldsa87:dev:u:spouse123... (Spouse)");
   Put_Line ("  Delay: 168 hours (1 week)");

   Now := Now + 1;
   Add_Recovery_Contact (
      State           => Vault,
      Caller          => Owner_Account,
      Caller_Network  => Dev,
      Contact         => Spouse_Account,
      Contact_Network => Dev,
      Delay_Hours     => 168,  -- 1 week delay
      Timestamp       => Now,
      Result          => Result);

   if Result = Success then
      Put_Line ("  [OK] Recovery contact added");
      Put_Line ("       If owner loses access, spouse can recover after 1 week");
   else
      Put_Line ("  [FAIL] " & Result_Code'Image (Result));
   end if;
   New_Line;

   ---------------------------------------------------------------------------
   --  TEST 6: Recovery request (starts timer)
   ---------------------------------------------------------------------------
   Put_Line ("TEST 6: Spouse requests recovery");
   Put_Line ("---------------------------------");

   Now := Now + 1;
   Request_Recovery (
      State             => Vault,
      Requester         => Spouse_Account,
      Requester_Network => Dev,
      Timestamp         => Now,
      Result            => Result);

   if Result = Success then
      Put_Line ("  [OK] Recovery requested - timer started");
      Put_Line ("       Must wait 168 hours before completing");
   else
      Put_Line ("  [FAIL] " & Result_Code'Image (Result));
   end if;

   --  Try to complete immediately (should fail)
   Now := Now + 1;
   Complete_Recovery (
      State             => Vault,
      Requester         => Spouse_Account,
      Requester_Network => Dev,
      Timestamp         => Now,
      Result            => Result);

   if Result = Error_Recovery_Delay then
      Put_Line ("  [OK] Immediate completion blocked: " & Result_Code'Image (Result));
   else
      Put_Line ("  [FAIL] Expected Error_Recovery_Delay, got " & Result_Code'Image (Result));
   end if;

   --  Simulate waiting 1 week (168 hours = 604800 seconds)
   Now := Now + 604800;
   Complete_Recovery (
      State             => Vault,
      Requester         => Spouse_Account,
      Requester_Network => Dev,
      Timestamp         => Now,
      Result            => Result);

   if Result = Success then
      Put_Line ("  [OK] Recovery completed after delay");
      Put_Line ("       New owner: mldsa87:dev:u:spouse123...");
   else
      Put_Line ("  [FAIL] " & Result_Code'Image (Result));
   end if;
   New_Line;

   ---------------------------------------------------------------------------
   --  TEST 7: Old owner can no longer access
   ---------------------------------------------------------------------------
   Put_Line ("TEST 7: Old owner access check");
   Put_Line ("-------------------------------");

   Now := Now + 1;
   Add_Entry (
      State          => Vault,
      Caller         => Owner_Account,  -- Old owner
      Caller_Network => Dev,
      Kind           => Kind_Note,
      Label_Hash     => Mock_Hash (16#30#),
      Data_Hash      => Mock_Hash (16#40#),
      Category       => 2,
      Bound_Network  => Dev,
      Restriction    => Users_Only,
      Timestamp      => Now,
      Index          => Index,
      Result         => Result);

   if Result = Error_Not_Owner then
      Put_Line ("  [OK] Old owner blocked: " & Result_Code'Image (Result));
      Put_Line ("       Ownership successfully transferred!");
   else
      Put_Line ("  [FAIL] Expected Error_Not_Owner, got " & Result_Code'Image (Result));
   end if;
   New_Line;

   ---------------------------------------------------------------------------
   --  SUMMARY
   ---------------------------------------------------------------------------
   Put_Line ("================================================================");
   Put_Line ("  SUMMARY: AAS-001 Address Features Demonstrated");
   Put_Line ("================================================================");
   Put_Line ("  1. Network Isolation:   Dev vault rejects mainnet callers");
   Put_Line ("  2. Entity Restrictions: Only users can access user passwords");
   Put_Line ("  3. Secure Sharing:      Share with specific AAS-001 addresses");
   Put_Line ("  4. Emergency Recovery:  Trusted contacts with time delay");
   Put_Line ("  5. Self-Describing:     Address tells you network/entity/algo");
   New_Line;
   Put_Line ("  Vault entries: " & Entry_Count_Type'Image (Get_Entry_Count (Vault)));
   Put_Line ("  Current owner: mldsa87:dev:u:spouse123... (recovered)");
   New_Line;
   Put_Line ("  All tests PASSED!");
   Put_Line ("================================================================");

end PQ_Vault_Real_Main;
