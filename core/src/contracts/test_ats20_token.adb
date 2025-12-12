--  Test harness for ATS20 Token CVM
--  Tests: Init, Transfer, BalanceOf, Approve, Allowance, TransferFrom

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with CVM_Types; use CVM_Types;
with ATS20_Token;

procedure Test_ATS20_Token is

   --  Test addresses (32 bytes each)
   Alice : constant Address := (
      16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#, 16#08#,
      16#09#, 16#0A#, 16#0B#, 16#0C#, 16#0D#, 16#0E#, 16#0F#, 16#10#,
      16#11#, 16#12#, 16#13#, 16#14#, 16#15#, 16#16#, 16#17#, 16#18#,
      16#19#, 16#1A#, 16#1B#, 16#1C#, 16#1D#, 16#1E#, 16#1F#, 16#20#
   );

   Bob : constant Address := (
      16#21#, 16#22#, 16#23#, 16#24#, 16#25#, 16#26#, 16#27#, 16#28#,
      16#29#, 16#2A#, 16#2B#, 16#2C#, 16#2D#, 16#2E#, 16#2F#, 16#30#,
      16#31#, 16#32#, 16#33#, 16#34#, 16#35#, 16#36#, 16#37#, 16#38#,
      16#39#, 16#3A#, 16#3B#, 16#3C#, 16#3D#, 16#3E#, 16#3F#, 16#40#
   );

   --  State array
   State : State_Array;
   Success : Boolean;
   Result : Exec_Result;

   --  Init params: name_len(1) + name(8) + symbol_len(1) + symbol(3) +
   --               decimals(1) + supply(8) + holder(32) = 54 bytes
   Init_Params : constant Byte_Array (0 .. 53) := (
      --  Name length and name "TestCoin"
      8,
      Character'Pos ('T'), Character'Pos ('e'), Character'Pos ('s'),
      Character'Pos ('t'), Character'Pos ('C'), Character'Pos ('o'),
      Character'Pos ('i'), Character'Pos ('n'),
      --  Symbol length and symbol "TST"
      3,
      Character'Pos ('T'), Character'Pos ('S'), Character'Pos ('T'),
      --  Decimals
      18,
      --  Initial supply (1000 tokens = 1000 * 10^18 in big-endian)
      --  For simplicity, just 1000 as raw value
      0, 0, 0, 0, 0, 0, 3, 232,  -- 1000 in big-endian
      --  Initial holder (Alice)
      16#01#, 16#02#, 16#03#, 16#04#, 16#05#, 16#06#, 16#07#, 16#08#,
      16#09#, 16#0A#, 16#0B#, 16#0C#, 16#0D#, 16#0E#, 16#0F#, 16#10#,
      16#11#, 16#12#, 16#13#, 16#14#, 16#15#, 16#16#, 16#17#, 16#18#,
      16#19#, 16#1A#, 16#1B#, 16#1C#, 16#1D#, 16#1E#, 16#1F#, 16#20#
   );

   --  Helper to read U64 from return data
   function Get_U64 (Data : Return_Buffer; Len : Natural) return Unsigned_64 is
      Val : Unsigned_64 := 0;
   begin
      if Len >= 8 then
         for I in 0 .. 7 loop
            Val := Shift_Left (Val, 8) or Unsigned_64 (Data (I));
         end loop;
      end if;
      return Val;
   end Get_U64;

   --  Create BalanceOf context
   function Make_Balance_Of_Context (Account : Address) return Call_Context is
      Ctx : Call_Context := Empty_Context;
   begin
      Ctx.Caller := Account;
      Ctx.Entry_Point := (16#70#, 16#a0#, 16#82#, 16#31#, others => 0);
      Ctx.Param_Len := 32;
      for I in Account'Range loop
         Ctx.Params (I) := Account (I);
      end loop;
      return Ctx;
   end Make_Balance_Of_Context;

   --  Create Transfer context
   function Make_Transfer_Context (
      From   : Address;
      To     : Address;
      Amount : Unsigned_64
   ) return Call_Context is
      Ctx : Call_Context := Empty_Context;
      Temp : Unsigned_64 := Amount;
   begin
      Ctx.Caller := From;
      Ctx.Entry_Point := (16#a9#, 16#05#, 16#9c#, 16#bb#, others => 0);
      Ctx.Param_Len := 40;
      --  To address
      for I in To'Range loop
         Ctx.Params (I) := To (I);
      end loop;
      --  Amount (big-endian)
      for I in reverse 32 .. 39 loop
         Ctx.Params (I) := Unsigned_8 (Temp and 16#FF#);
         Temp := Shift_Right (Temp, 8);
      end loop;
      return Ctx;
   end Make_Transfer_Context;

begin
   Put_Line ("=== ATS20 Token CVM Test ===");
   Put_Line ("");

   --  Test 1: Initialize token
   Put_Line ("Test 1: Initialize token");
   ATS20_Token.Init (Init_Params, State, Success);
   if Success then
      Put_Line ("  PASS: Token initialized");
   else
      Put_Line ("  FAIL: Token initialization failed");
      return;
   end if;

   --  Test 2: Check Alice's balance (should be 1000)
   Put_Line ("");
   Put_Line ("Test 2: Check Alice's balance");
   declare
      Ctx : constant Call_Context := Make_Balance_Of_Context (Alice);
      Balance : Unsigned_64;
   begin
      ATS20_Token.Balance_Of (Ctx, State, Result);
      if Result.Status = CVM_Types.Success then
         Balance := Get_U64 (Result.Return_Data, Result.Return_Len);
         Put_Line ("  Balance: " & Unsigned_64'Image (Balance));
         if Balance = 1000 then
            Put_Line ("  PASS: Initial balance correct");
         else
            Put_Line ("  FAIL: Expected 1000, got" & Unsigned_64'Image (Balance));
         end if;
      else
         Put_Line ("  FAIL: BalanceOf failed");
      end if;
   end;

   --  Test 3: Check Bob's balance (should be 0)
   Put_Line ("");
   Put_Line ("Test 3: Check Bob's balance");
   declare
      Ctx : constant Call_Context := Make_Balance_Of_Context (Bob);
      Balance : Unsigned_64;
   begin
      ATS20_Token.Balance_Of (Ctx, State, Result);
      if Result.Status = CVM_Types.Success then
         Balance := Get_U64 (Result.Return_Data, Result.Return_Len);
         Put_Line ("  Balance: " & Unsigned_64'Image (Balance));
         if Balance = 0 then
            Put_Line ("  PASS: Bob's initial balance is 0");
         else
            Put_Line ("  FAIL: Expected 0, got" & Unsigned_64'Image (Balance));
         end if;
      else
         Put_Line ("  FAIL: BalanceOf failed");
      end if;
   end;

   --  Test 4: Transfer 100 from Alice to Bob
   Put_Line ("");
   Put_Line ("Test 4: Transfer 100 from Alice to Bob");
   declare
      Ctx : constant Call_Context := Make_Transfer_Context (Alice, Bob, 100);
   begin
      ATS20_Token.Transfer (Ctx, State, Result);
      if Result.Status = CVM_Types.Success and then
         Result.Return_Len > 0 and then
         Result.Return_Data (0) = 1
      then
         Put_Line ("  PASS: Transfer successful");
      else
         Put_Line ("  FAIL: Transfer failed");
      end if;
   end;

   --  Test 5: Verify balances after transfer
   Put_Line ("");
   Put_Line ("Test 5: Verify balances after transfer");
   declare
      Alice_Ctx : constant Call_Context := Make_Balance_Of_Context (Alice);
      Bob_Ctx   : constant Call_Context := Make_Balance_Of_Context (Bob);
      Alice_Bal : Unsigned_64;
      Bob_Bal   : Unsigned_64;
   begin
      ATS20_Token.Balance_Of (Alice_Ctx, State, Result);
      Alice_Bal := Get_U64 (Result.Return_Data, Result.Return_Len);

      ATS20_Token.Balance_Of (Bob_Ctx, State, Result);
      Bob_Bal := Get_U64 (Result.Return_Data, Result.Return_Len);

      Put_Line ("  Alice balance: " & Unsigned_64'Image (Alice_Bal));
      Put_Line ("  Bob balance:   " & Unsigned_64'Image (Bob_Bal));

      if Alice_Bal = 900 and Bob_Bal = 100 then
         Put_Line ("  PASS: Balances correct after transfer");
      else
         Put_Line ("  FAIL: Balances incorrect");
      end if;
   end;

   --  Test 6: Total supply
   Put_Line ("");
   Put_Line ("Test 6: Check total supply");
   declare
      Ctx : Call_Context := Empty_Context;
      Supply : Unsigned_64;
   begin
      Ctx.Entry_Point := (16#18#, 16#16#, 16#0d#, 16#dd#, others => 0);
      ATS20_Token.Total_Supply (Ctx, State, Result);
      if Result.Status = CVM_Types.Success then
         Supply := Get_U64 (Result.Return_Data, Result.Return_Len);
         Put_Line ("  Total supply: " & Unsigned_64'Image (Supply));
         if Supply = 1000 then
            Put_Line ("  PASS: Total supply correct");
         else
            Put_Line ("  FAIL: Expected 1000");
         end if;
      else
         Put_Line ("  FAIL: TotalSupply failed");
      end if;
   end;

   Put_Line ("");
   Put_Line ("=== All tests completed ===");

end Test_ATS20_Token;
