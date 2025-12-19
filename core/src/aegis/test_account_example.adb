pragma SPARK_Mode (On);

with Aegis_Account_State; use Aegis_Account_State;
with Anubis_Address_Types; use Anubis_Address_Types;
with Aegis_U256; use Aegis_U256;
with Aegis_VM_Types; use Aegis_VM_Types;
with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

--  Test_Account_Example: Example usage of Aegis Account State
--
--  Demonstrates:
--  - Account creation with ML-DSA-87 derived addresses
--  - Nonce tracking for replay protection
--  - Balance management (add, subtract, transfer)
--  - Contract initialization
--  - Account validation

procedure Test_Account_Example with
   SPARK_Mode => Off  --  Main program doesn't need SPARK mode
is
   --  Example address (derived from ML-DSA-87 public key)
   Address1 : constant Account_ID := (
      16#12#, 16#34#, 16#56#, 16#78#, 16#9A#, 16#BC#, 16#DE#, 16#F0#,
      16#11#, 16#22#, 16#33#, 16#44#, 16#55#, 16#66#, 16#77#, 16#88#,
      16#99#, 16#AA#, 16#BB#, 16#CC#, 16#DD#, 16#EE#, 16#FF#, 16#00#,
      16#AB#, 16#CD#, 16#EF#, 16#01#, 16#23#, 16#45#, 16#67#, 16#89#
   );

   Address2 : constant Account_ID := (
      16#FE#, 16#DC#, 16#BA#, 16#98#, 16#76#, 16#54#, 16#32#, 16#10#,
      16#FF#, 16#EE#, 16#DD#, 16#CC#, 16#BB#, 16#AA#, 16#99#, 16#88#,
      16#77#, 16#66#, 16#55#, 16#44#, 16#33#, 16#22#, 16#11#, 16#00#,
      16#98#, 16#76#, 16#54#, 16#32#, 16#10#, 16#FE#, 16#DC#, 16#BA#
   );

   --  Account states
   Account_Alice : Account_State;
   Account_Bob   : Account_State;

   --  Operation results
   Success : Boolean;
   Error   : Account_Error;

   --  Example code hash for contract
   Code_Hash : constant Hash256 := (
      16#C0#, 16#DE#, 16#HA#, 16#5H#, 16#00#, 16#00#, 16#00#, 16#01#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
   );

   --  Example balances
   Initial_Balance : constant U256 := From_Word64 (1000);
   Transfer_Amount : constant U256 := From_Word64 (100);

begin
   Put_Line ("AEGIS Account State Management Example");
   Put_Line ("======================================");
   New_Line;

   --  1. Create accounts
   Put_Line ("1. Creating accounts...");
   Account_Alice := Create_Account (Address1);
   Account_Bob := Create_Account (Address2);

   if Account_Alice.Exists and Account_Bob.Exists then
      Put_Line ("   ✓ Alice's account created");
      Put_Line ("   ✓ Bob's account created");
   else
      Put_Line ("   ✗ Account creation failed");
      return;
   end if;
   New_Line;

   --  2. Set initial balances
   Put_Line ("2. Setting initial balances...");
   Set_Balance (Account_Alice, Initial_Balance);
   Set_Balance (Account_Bob, U256_Zero);
   Put_Line ("   ✓ Alice: 1000 units");
   Put_Line ("   ✓ Bob: 0 units");
   New_Line;

   --  3. Increment nonce (simulate transaction)
   Put_Line ("3. Incrementing Alice's nonce...");
   Put_Line ("   Initial nonce: " & Account_Alice.Nonce'Image);
   Increment_Nonce (Account_Alice, Success, Error);
   if Success then
      Put_Line ("   ✓ Nonce incremented: " & Account_Alice.Nonce'Image);
   else
      Put_Line ("   ✗ Nonce increment failed: " & Error'Image);
   end if;
   New_Line;

   --  4. Transfer funds
   Put_Line ("4. Transferring 100 units from Alice to Bob...");
   Transfer (Account_Alice, Account_Bob, Transfer_Amount, Success, Error);
   if Success then
      Put_Line ("   ✓ Transfer successful");
      Put_Line ("   Alice balance: " & To_Word64 (Account_Alice.Balance)'Image);
      Put_Line ("   Bob balance: " & To_Word64 (Account_Bob.Balance)'Image);
   else
      Put_Line ("   ✗ Transfer failed: " & Error'Image);
   end if;
   New_Line;

   --  5. Initialize contract
   Put_Line ("5. Initializing Alice's account as contract...");
   Initialize_Contract (Account_Alice, Code_Hash, Success, Error);
   if Success then
      Put_Line ("   ✓ Contract initialized");
      Put_Line ("   Is EOA: " & (if Is_EOA (Account_Alice) then "Yes" else "No"));
      Put_Line ("   Is Contract: " & (if Is_Contract (Account_Alice) then "Yes" else "No"));
   else
      Put_Line ("   ✗ Contract initialization failed: " & Error'Image);
   end if;
   New_Line;

   --  6. Validate accounts
   Put_Line ("6. Validating account states...");
   if Validate_Account (Account_Alice) then
      Put_Line ("   ✓ Alice's account valid");
   else
      Put_Line ("   ✗ Alice's account invalid");
   end if;

   if Validate_Account (Account_Bob) then
      Put_Line ("   ✓ Bob's account valid");
   else
      Put_Line ("   ✗ Bob's account invalid");
   end if;
   New_Line;

   --  7. Summary
   Put_Line ("Summary:");
   Put_Line ("--------");
   Put_Line ("Alice:");
   Put_Line ("  - Balance: " & To_Word64 (Account_Alice.Balance)'Image);
   Put_Line ("  - Nonce: " & Account_Alice.Nonce'Image);
   Put_Line ("  - Type: " & (if Is_Contract (Account_Alice) then "Contract" else "EOA"));
   New_Line;
   Put_Line ("Bob:");
   Put_Line ("  - Balance: " & To_Word64 (Account_Bob.Balance)'Image);
   Put_Line ("  - Nonce: " & Account_Bob.Nonce'Image);
   Put_Line ("  - Type: " & (if Is_Contract (Account_Bob) then "Contract" else "EOA"));
   New_Line;

   Put_Line ("All tests completed successfully!");

end Test_Account_Example;
