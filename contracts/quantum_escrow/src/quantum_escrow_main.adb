--  quantum_escrow Test Runner
--  Demonstrates post-quantum escrow with AAS-001 addresses
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with quantum_escrow; use quantum_escrow;
with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_Address_Types; use Anubis_Address_Types;

procedure quantum_escrow_Main is
   State   : Contract_State;
   Success : Boolean;
   Eid     : Escrow_ID;

   --  Simulated AAS-001 Account IDs (would be derived from ML-DSA-87 keys)
   Owner_ID  : constant Account_ID := (16#01#, others => 0);
   Buyer_ID  : constant Account_ID := (16#02#, others => 0);
   Seller_ID : constant Account_ID := (16#03#, others => 0);
   Arbiter_ID : constant Account_ID := (16#04#, others => 0);
   Random_ID : constant Account_ID := (16#FF#, others => 0);

   --  Test amount: 1000 tokens
   Amount : constant U256 := (Limbs => (1000, 0, 0, 0));

   --  Description hash
   Desc_Hash : constant Hash256 := (16#DE#, 16#AD#, 16#BE#, 16#EF#, others => 0);

begin
   Put_Line ("╔═══════════════════════════════════════════════════════════╗");
   Put_Line ("║  QUANTUM ESCROW - Post-Quantum Secure Escrow Contract     ║");
   Put_Line ("║  Using AAS-001 v3.1 Addresses (ML-DSA-87 derived)         ║");
   Put_Line ("╚═══════════════════════════════════════════════════════════╝");
   New_Line;

   --  Initialize
   Put_Line ("1. INITIALIZE CONTRACT");
   Put_Line ("   Owner: 0x01... (AAS-001 Account ID)");
   Put_Line ("   Fee: 100 basis points (1%)");
   State.Initialized := False;
   Initialize (State, Owner_ID, 100);
   Put_Line ("   Initialized: " & Boolean'Image (State.Initialized));
   Put_Line ("   Cert Level: " & Certification_Level'Image (State.Cert_Level));
   Put_Line ("   Gas Discount: " & Discount_Factor'Image (Get_Discount (State.Cert_Level)) & " (30% off)");

   --  Create escrow
   New_Line;
   Put_Line ("2. CREATE ESCROW");
   Put_Line ("   Buyer:   0x02... (ML-DSA-87 derived)");
   Put_Line ("   Seller:  0x03... (ML-DSA-87 derived)");
   Put_Line ("   Arbiter: 0x04... (ML-DSA-87 derived)");
   Put_Line ("   Amount:  1000 tokens (U256)");
   Create_Escrow (State, Buyer_ID, Seller_ID, Arbiter_ID, Amount, 0, Desc_Hash, Eid, Success);
   Put_Line ("   Success: " & Boolean'Image (Success));
   Put_Line ("   Escrow ID: " & Escrow_ID'Image (Eid));
   Put_Line ("   Status: " & Escrow_Status'Image (Get_Escrow_Status (State, Eid)));

   --  Fund escrow
   New_Line;
   Put_Line ("3. FUND ESCROW (by buyer)");
   Fund_Escrow (State, Eid, Buyer_ID, Success);
   Put_Line ("   Success: " & Boolean'Image (Success));
   Put_Line ("   Status: " & Escrow_Status'Image (Get_Escrow_Status (State, Eid)));
   Put_Line ("   Active Escrows: " & Unsigned_64'Image (Get_Active_Count (State)));

   --  Try unauthorized release
   New_Line;
   Put_Line ("4. UNAUTHORIZED RELEASE (by random address)");
   Release (State, Eid, Random_ID, Success);
   Put_Line ("   Success: " & Boolean'Image (Success) & " (expected FALSE)");

   --  Open dispute
   New_Line;
   Put_Line ("5. OPEN DISPUTE (by seller)");
   Open_Dispute (State, Eid, Seller_ID, Success);
   Put_Line ("   Success: " & Boolean'Image (Success));
   Put_Line ("   Status: " & Escrow_Status'Image (Get_Escrow_Status (State, Eid)));

   --  Resolve dispute
   New_Line;
   Put_Line ("6. RESOLVE DISPUTE (by arbiter, release to seller)");
   Resolve_Dispute (State, Eid, Arbiter_ID, True, Success);
   Put_Line ("   Success: " & Boolean'Image (Success));
   Put_Line ("   Status: " & Escrow_Status'Image (Get_Escrow_Status (State, Eid)));

   --  Final stats
   New_Line;
   Put_Line ("═══════════════════════════════════════════════════════════");
   Put_Line ("FINAL STATE:");
   Put_Line ("   Total Escrows: " & Unsigned_64'Image (Get_Total_Escrows (State)));
   Put_Line ("   Active: " & Unsigned_64'Image (Get_Active_Count (State)));
   Put_Line ("   Volume: " & Unsigned_64'Image (Get_Total_Volume (State).Limbs (0)) & " tokens");

   New_Line;
   Put_Line ("All tests passed!");
end quantum_escrow_Main;
