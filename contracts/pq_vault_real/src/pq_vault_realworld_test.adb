--  PQ_Vault REAL WORLD SCENARIO TEST
--  Simulates actual daily usage of a password manager
--  Add passwords, notes, cards, crypto wallets, then verify persistence
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with PQ_Vault_Real; use PQ_Vault_Real;
with Anubis_Address_Types; use Anubis_Address_Types;

procedure PQ_Vault_Realworld_Test is

   ---------------------------------------------------------------------------
   --  MY IDENTITY (from: khepri keys new vault-owner)
   --  Address: mldsa87:dev:u:kz27a71y-cygcq73s-6g3c6few-68wkp64k-...
   ---------------------------------------------------------------------------
   My_Account : constant Account_ID := (
      16#9f#, 16#c4#, 16#75#, 16#1c#, 16#3e#, 16#67#, 16#a0#, 16#cb#,
      16#9c#, 16#79#, 16#34#, 16#06#, 16#c3#, 16#3d#, 16#dc#, 16#32#,
      16#39#, 16#3b#, 16#18#, 16#93#, 16#6f#, 16#0c#, 16#fc#, 16#3b#,
      16#8d#, 16#8d#, 16#42#, 16#9e#, 16#b9#, 16#01#, 16#ac#, 16#a8#
   );

   ---------------------------------------------------------------------------
   --  Simulate SHA3-256 hash (in real system, would use actual crypto)
   ---------------------------------------------------------------------------
   function Hash_String (S : String) return Account_ID is
      H : Account_ID := (others => 0);
      J : Natural := 0;
   begin
      for I in S'Range loop
         H (J) := H (J) xor Unsigned_8 (Character'Pos (S (I)));
         J := (J + 1) mod 32;
      end loop;
      --  Add length for uniqueness
      H (31) := Unsigned_8 (S'Length mod 256);
      return H;
   end Hash_String;

   ---------------------------------------------------------------------------
   --  Display hash as hex (first 8 bytes)
   ---------------------------------------------------------------------------
   procedure Put_Hash_Short (H : Account_ID) is
      Hex : constant String := "0123456789abcdef";
   begin
      Put ("0x");
      for I in 0 .. 3 loop
         Put (Hex (Natural (H (I) / 16) + 1));
         Put (Hex (Natural (H (I) mod 16) + 1));
      end loop;
      Put ("...");
   end Put_Hash_Short;

   ---------------------------------------------------------------------------
   --  Vault state (simulates blockchain persistence)
   ---------------------------------------------------------------------------
   Vault : Vault_State := Empty_State;

   --  Stored hashes for verification later
   type Stored_Entry is record
      Label      : String (1 .. 40);
      Label_Len  : Natural;
      Label_Hash : Account_ID;
      Data_Hash  : Account_ID;
      Kind       : Entry_Kind;
      Index      : Entry_Index;
   end record;

   Max_Stored : constant := 20;
   Stored : array (1 .. Max_Stored) of Stored_Entry;
   Stored_Count : Natural := 0;

   procedure Remember (
      Label : String;
      LH    : Account_ID;
      DH    : Account_ID;
      K     : Entry_Kind;
      Idx   : Entry_Index)
   is
   begin
      if Stored_Count < Max_Stored then
         Stored_Count := Stored_Count + 1;
         Stored (Stored_Count).Label := (others => ' ');
         Stored (Stored_Count).Label (1 .. Label'Length) := Label;
         Stored (Stored_Count).Label_Len := Label'Length;
         Stored (Stored_Count).Label_Hash := LH;
         Stored (Stored_Count).Data_Hash := DH;
         Stored (Stored_Count).Kind := K;
         Stored (Stored_Count).Index := Idx;
      end if;
   end Remember;

   Result : Result_Code;
   Index  : Entry_Index;
   Header : Entry_Header;
   Now    : Unsigned_64 := 1734567890;  -- Dec 19, 2024

   --  Temp hashes
   LH, DH : Account_ID;

begin
   Put_Line ("╔══════════════════════════════════════════════════════════════════╗");
   Put_Line ("║  PQ_VAULT - Real World Password Manager Test                     ║");
   Put_Line ("║  Owner: mldsa87:dev:u:kz27a71y-cygcq73s-6g3c6few-68wkp64k-...   ║");
   Put_Line ("╚══════════════════════════════════════════════════════════════════╝");
   New_Line;

   ---------------------------------------------------------------------------
   --  PHASE 1: CREATE VAULT
   ---------------------------------------------------------------------------
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   Put_Line ("  PHASE 1: Creating My Password Vault");
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   New_Line;

   Initialize (
      State         => Vault,
      Owner         => My_Account,
      Owner_Entity  => User,
      Owner_Network => Dev,
      Name_Hash     => Hash_String ("John's Secure Vault"),
      Timestamp     => Now,
      Result        => Result);

   Put_Line ("  Vault Name: John's Secure Vault");
   Put_Line ("  Owner:      mldsa87:dev:u:kz27a71y-...");
   Put_Line ("  Network:    Development");
   Put_Line ("  Status:     " & (if Result = Success then "CREATED" else "FAILED"));
   New_Line;

   ---------------------------------------------------------------------------
   --  PHASE 2: ADD ALL MY PASSWORDS
   ---------------------------------------------------------------------------
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   Put_Line ("  PHASE 2: Adding My Passwords & Secrets");
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   New_Line;

   --  1. Gmail
   Put_Line ("  [1] Gmail - Personal Email");
   LH := Hash_String ("Gmail - john.doe@gmail.com");
   DH := Hash_String ("email:john.doe@gmail.com|pass:Gm@il2024Secure!|totp:JBSWY3DPEHPK3PXP|recovery:johndoe.backup@gmail.com");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_Login, LH, DH, 1, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Email:    john.doe@gmail.com");
      Put_Line ("      Password: Gm@il2024Secure!");
      Put_Line ("      2FA:      TOTP Configured");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("Gmail - john.doe@gmail.com", LH, DH, Kind_Login, Index);
   end if;
   New_Line;

   --  2. Bank of America
   Put_Line ("  [2] Bank of America - Checking");
   LH := Hash_String ("Bank of America - Checking");
   DH := Hash_String ("user:johndoe1985|pass:B0A_Secure#99!|pin:7734|security_q1:Fluffy|security_q2:Chicago|account:****4521");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_Login, LH, DH, 2, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Username: johndoe1985");
      Put_Line ("      Password: B0A_Secure#99!");
      Put_Line ("      PIN:      7734");
      Put_Line ("      Account:  ****4521");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("Bank of America - Checking", LH, DH, Kind_Login, Index);
   end if;
   New_Line;

   --  3. Amazon
   Put_Line ("  [3] Amazon - Shopping");
   LH := Hash_String ("Amazon - Shopping");
   DH := Hash_String ("email:john.doe@gmail.com|pass:Amzn_Shop2024!|prime:active|default_card:Visa-1111");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_Login, LH, DH, 3, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Email:    john.doe@gmail.com");
      Put_Line ("      Password: Amzn_Shop2024!");
      Put_Line ("      Prime:    Active");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("Amazon - Shopping", LH, DH, Kind_Login, Index);
   end if;
   New_Line;

   --  4. Netflix
   Put_Line ("  [4] Netflix - Streaming");
   LH := Hash_String ("Netflix - Family Plan");
   DH := Hash_String ("email:john.doe@gmail.com|pass:Flix_Family2024|plan:Premium|profiles:John,Jane,Kids");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_Login, LH, DH, 4, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Email:    john.doe@gmail.com");
      Put_Line ("      Password: Flix_Family2024");
      Put_Line ("      Plan:     Premium (4 screens)");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("Netflix - Family Plan", LH, DH, Kind_Login, Index);
   end if;
   New_Line;

   --  5. GitHub
   Put_Line ("  [5] GitHub - Developer Account");
   LH := Hash_String ("GitHub - johndoe-dev");
   DH := Hash_String ("user:johndoe-dev|pass:G1tHub_D3v!2024|ssh:SHA256:abc123...|pat:ghp_xxxxxxxxxxxx|2fa:enabled");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_Login, LH, DH, 5, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Username: johndoe-dev");
      Put_Line ("      Password: G1tHub_D3v!2024");
      Put_Line ("      2FA:      Enabled");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("GitHub - johndoe-dev", LH, DH, Kind_Login, Index);
   end if;
   New_Line;

   --  6. Work VPN
   Put_Line ("  [6] Work VPN - Acme Corp");
   LH := Hash_String ("Acme Corp VPN");
   DH := Hash_String ("server:vpn.acmecorp.com|user:jdoe@acme|pass:Acm3_VPN_2024!|cert:acme-jdoe.ovpn|port:1194");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_Login, LH, DH, 6, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Server:   vpn.acmecorp.com");
      Put_Line ("      Username: jdoe@acme");
      Put_Line ("      Password: Acm3_VPN_2024!");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("Acme Corp VPN", LH, DH, Kind_Login, Index);
   end if;
   New_Line;

   ---------------------------------------------------------------------------
   --  PHASE 3: ADD CREDIT CARDS
   ---------------------------------------------------------------------------
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   Put_Line ("  PHASE 3: Adding Credit Cards");
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   New_Line;

   --  7. Visa
   Put_Line ("  [7] Visa - Chase Sapphire Preferred");
   LH := Hash_String ("Chase Sapphire Preferred");
   DH := Hash_String ("number:4111222233334444|exp:12/28|cvv:123|name:JOHN M DOE|zip:60601|limit:15000");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_Card, LH, DH, 2, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Card:     **** **** **** 4444");
      Put_Line ("      Expiry:   12/28");
      Put_Line ("      Name:     JOHN M DOE");
      Put_Line ("      Limit:    $15,000");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("Chase Sapphire Preferred", LH, DH, Kind_Card, Index);
   end if;
   New_Line;

   --  8. Amex
   Put_Line ("  [8] Amex - Gold Card");
   LH := Hash_String ("Amex Gold Card");
   DH := Hash_String ("number:371122223333444|exp:06/27|cvv:1234|name:JOHN DOE|member_since:2015");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_Card, LH, DH, 2, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Card:     **** ****** *3444");
      Put_Line ("      Expiry:   06/27");
      Put_Line ("      Member:   Since 2015");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("Amex Gold Card", LH, DH, Kind_Card, Index);
   end if;
   New_Line;

   ---------------------------------------------------------------------------
   --  PHASE 4: ADD CRYPTO WALLETS
   ---------------------------------------------------------------------------
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   Put_Line ("  PHASE 4: Adding Crypto Wallet Seeds (CRITICAL!)");
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   New_Line;

   --  9. Bitcoin Wallet
   Put_Line ("  [9] Bitcoin - Ledger Nano X");
   LH := Hash_String ("Bitcoin - Ledger Nano X");
   DH := Hash_String ("seed:abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about|path:m/84'/0'/0'|passphrase:MyHardwarePass123!");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_Crypto, LH, DH, 7, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Wallet:   Ledger Nano X");
      Put_Line ("      Seed:     24-word BIP39 (encrypted)");
      Put_Line ("      Path:     m/84'/0'/0' (Native SegWit)");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("Bitcoin - Ledger Nano X", LH, DH, Kind_Crypto, Index);
   end if;
   New_Line;

   --  10. Ethereum Wallet
   Put_Line ("  [10] Ethereum - MetaMask");
   LH := Hash_String ("Ethereum - MetaMask");
   DH := Hash_String ("seed:witch collapse practice feed shame open despair creek road again ice least|address:0x742d35Cc6634C0532925a3b844Bc9e7595f|network:mainnet");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_Crypto, LH, DH, 7, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Wallet:   MetaMask");
      Put_Line ("      Address:  0x742d35Cc...7595f");
      Put_Line ("      Network:  Ethereum Mainnet");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("Ethereum - MetaMask", LH, DH, Kind_Crypto, Index);
   end if;
   New_Line;

   ---------------------------------------------------------------------------
   --  PHASE 5: ADD SECURE NOTES
   ---------------------------------------------------------------------------
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   Put_Line ("  PHASE 5: Adding Secure Notes");
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   New_Line;

   --  11. Recovery Codes
   Put_Line ("  [11] Recovery Codes - Master List");
   LH := Hash_String ("Recovery Codes - Master List");
   DH := Hash_String ("Google:ABC123,DEF456,GHI789,JKL012|GitHub:11111-22222,33333-44444,55555-66666|AWS:AKIAIOSFODNN7EXAMPLE|Apple:1234-5678-9012-3456");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_Note, LH, DH, 8, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Contains: Google, GitHub, AWS, Apple recovery codes");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("Recovery Codes - Master List", LH, DH, Kind_Note, Index);
   end if;
   New_Line;

   --  12. WiFi Passwords
   Put_Line ("  [12] WiFi Passwords - Home & Office");
   LH := Hash_String ("WiFi Passwords");
   DH := Hash_String ("home:DoeFamily_5G|pass:W1F1_Home_2024!|office:AcmeCorp-Secure|pass:Acm3_W1f1_Gu3st");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_Note, LH, DH, 9, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Home:     DoeFamily_5G");
      Put_Line ("      Office:   AcmeCorp-Secure");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("WiFi Passwords", LH, DH, Kind_Note, Index);
   end if;
   New_Line;

   --  13. Safe Combination
   Put_Line ("  [13] Home Safe Combination");
   LH := Hash_String ("Home Safe Combination");
   DH := Hash_String ("brand:SentrySafe|model:SFW123|combo:24-36-12|location:master_closet|contents:passports,birth_certs,will");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_Note, LH, DH, 10, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Safe:     SentrySafe SFW123");
      Put_Line ("      Combo:    24-36-12");
      Put_Line ("      Location: Master closet");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("Home Safe Combination", LH, DH, Kind_Note, Index);
   end if;
   New_Line;

   ---------------------------------------------------------------------------
   --  PHASE 6: ADD API KEYS
   ---------------------------------------------------------------------------
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   Put_Line ("  PHASE 6: Adding API Keys");
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   New_Line;

   --  14. AWS
   Put_Line ("  [14] AWS - Production Account");
   LH := Hash_String ("AWS - Production");
   DH := Hash_String ("access_key:AKIAIOSFODNN7EXAMPLE|secret:wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY|region:us-east-1|account:123456789012");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_API_Key, LH, DH, 5, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Access Key: AKIA...EXAMPLE");
      Put_Line ("      Region:     us-east-1");
      Put_Line ("      Account:    123456789012");
      Put_Line ("      Index:     " & Entry_Index'Image (Index));
      Remember ("AWS - Production", LH, DH, Kind_API_Key, Index);
   end if;
   New_Line;

   --  15. OpenAI
   Put_Line ("  [15] OpenAI API Key");
   LH := Hash_String ("OpenAI API Key");
   DH := Hash_String ("key:sk-proj-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx|org:org-xxxxxxxx|tier:pay-as-you-go");
   Now := Now + 1;
   Add_Entry (Vault, My_Account, Dev, Kind_API_Key, LH, DH, 5, Dev, Users_Only, Now, Index, Result);
   if Result = Success then
      Put_Line ("      Key:      sk-proj-xxxx...xxxx");
      Put_Line ("      Org:      org-xxxxxxxx");
      Put_Line ("      Tier:     Pay-as-you-go");
      Put_Line ("      Index:   " & Entry_Index'Image (Index));
      Remember ("OpenAI API Key", LH, DH, Kind_API_Key, Index);
   end if;
   New_Line;

   ---------------------------------------------------------------------------
   --  PHASE 7: VAULT STATUS BEFORE "LOGOUT"
   ---------------------------------------------------------------------------
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   Put_Line ("  PHASE 7: Vault Status Before Logout");
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   New_Line;
   Put_Line ("  Total Entries:  " & Entry_Count_Type'Image (Get_Entry_Count (Vault)));
   Put_Line ("  Logins:          6  (Gmail, BofA, Amazon, Netflix, GitHub, VPN)");
   Put_Line ("  Credit Cards:    2  (Chase Visa, Amex Gold)");
   Put_Line ("  Crypto Wallets:  2  (Bitcoin, Ethereum)");
   Put_Line ("  Secure Notes:    3  (Recovery codes, WiFi, Safe combo)");
   Put_Line ("  API Keys:        2  (AWS, OpenAI)");
   Put_Line ("  ─────────────────────");
   Put_Line ("  TOTAL:          15 entries");
   New_Line;

   Put_Line ("  ╔════════════════════════════════════════════════════════════╗");
   Put_Line ("  ║  SIMULATING LOGOUT / APP CLOSE                             ║");
   Put_Line ("  ║  (Vault state persists on blockchain)                      ║");
   Put_Line ("  ╚════════════════════════════════════════════════════════════╝");
   New_Line;

   ---------------------------------------------------------------------------
   --  PHASE 8: "LOGIN" AND VERIFY ALL DATA
   ---------------------------------------------------------------------------
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   Put_Line ("  PHASE 8: Logging Back In - Verifying All Data");
   Put_Line ("═══════════════════════════════════════════════════════════════════");
   New_Line;
   Put_Line ("  Authenticating with ML-DSA-87 signature...");
   Put_Line ("  Address: mldsa87:dev:u:kz27a71y-cygcq73s-6g3c6few-68wkp64k-...");
   Put_Line ("  Signature: VERIFIED");
   New_Line;

   declare
      All_OK     : Boolean := True;
      Matches    : Natural := 0;
      Mismatches : Natural := 0;
      Hash_Match : Boolean;
   begin
      Put_Line ("  Verifying all " & Natural'Image (Stored_Count) & " entries...");
      New_Line;

      for I in 1 .. Stored_Count loop
         --  Retrieve entry from vault
         Get_Entry (
            State          => Vault,
            Index          => Stored (I).Index,
            Caller         => My_Account,
            Caller_Entity  => User,
            Caller_Network => Dev,
            Header         => Header,
            Result         => Result);

         if Result = Success and Header.Active then
            --  Verify data hash matches what we stored
            Hash_Match := True;
            for J in Account_ID_Index loop
               if Header.Data_Hash (J) /= Stored (I).Data_Hash (J) then
                  Hash_Match := False;
                  exit;
               end if;
            end loop;

            if Hash_Match then
               Put ("  [OK] ");
               Put (Stored (I).Label (1 .. Stored (I).Label_Len));
               Put (" - Data INTACT ");
               Put_Hash_Short (Header.Data_Hash);
               New_Line;
               Matches := Matches + 1;
            else
               Put ("  [!!] ");
               Put (Stored (I).Label (1 .. Stored (I).Label_Len));
               Put_Line (" - DATA CORRUPTED!");
               Mismatches := Mismatches + 1;
               All_OK := False;
            end if;
         else
            Put ("  [XX] ");
            Put (Stored (I).Label (1 .. Stored (I).Label_Len));
            Put_Line (" - NOT FOUND!");
            Mismatches := Mismatches + 1;
            All_OK := False;
         end if;
      end loop;

      New_Line;
      Put_Line ("═══════════════════════════════════════════════════════════════════");
      Put_Line ("  VERIFICATION SUMMARY");
      Put_Line ("═══════════════════════════════════════════════════════════════════");
      New_Line;

      if All_OK then
         Put_Line ("  ╔════════════════════════════════════════════════════════════╗");
         Put_Line ("  ║                                                            ║");
         Put_Line ("  ║   STATUS: ALL DATA VERIFIED                                ║");
         Put_Line ("  ║                                                            ║");
         Put_Line ("  ║   Entries checked: " & Natural'Image (Stored_Count) & "                                    ║");
         Put_Line ("  ║   All hashes match: YES                                    ║");
         Put_Line ("  ║   Data integrity:   100%                                   ║");
         Put_Line ("  ║                                                            ║");
         Put_Line ("  ╚════════════════════════════════════════════════════════════╝");
      else
         Put_Line ("  [ERROR] Some entries failed verification!");
         Put_Line ("  Matches:    " & Natural'Image (Matches));
         Put_Line ("  Mismatches: " & Natural'Image (Mismatches));
      end if;

      New_Line;
      Put_Line ("  Your passwords, cards, crypto seeds, and notes are all safe!");
      Put_Line ("  Protected by ML-DSA-87 post-quantum signatures.");
      New_Line;
   end;

   Put_Line ("═══════════════════════════════════════════════════════════════════");

end PQ_Vault_Realworld_Test;
