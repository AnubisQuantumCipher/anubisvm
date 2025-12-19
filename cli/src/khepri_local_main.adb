with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_U256;      use Aegis_U256;
with Local_Executor;

procedure Khepri_Local_Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   From_Zero : constant Contract_Address := (others => 0);

   function Hex_To_Address (S : String) return Contract_Address is
      Addr : Contract_Address := (others => 0);
      Hex  : String := S;

      function Hex_Digit (C : Character) return Integer is
      begin
         if C in '0' .. '9' then
            return Character'Pos (C) - Character'Pos ('0');
         elsif C in 'a' .. 'f' then
            return 10 + Character'Pos (C) - Character'Pos ('a');
         elsif C in 'A' .. 'F' then
            return 10 + Character'Pos (C) - Character'Pos ('A');
         else
            return -1;
         end if;
      end Hex_Digit;
   begin
      if Hex'Length >= 2 and then Hex (Hex'First) = '0' and then
        (Hex (Hex'First + 1) = 'x' or else Hex (Hex'First + 1) = 'X')
      then
         Hex := Hex (Hex'First + 2 .. Hex'Last);
      end if;

      declare
         Pos      : Natural := Hex'Length;
         Byte_Pos : Natural := 31;
      begin
         while Pos > 0 and Byte_Pos < 32 loop
            declare
               Lo_Char : constant Character := Hex (Hex'First + Pos - 1);
               Lo      : constant Integer   := Hex_Digit (Lo_Char);
               Hi      : Integer := 0;
            begin
               exit when Lo < 0;
               Pos := Pos - 1;
               if Pos > 0 then
                  Hi := Hex_Digit (Hex (Hex'First + Pos - 1));
                  if Hi >= 0 then
                     Pos := Pos - 1;
                  else
                     Hi := 0;
                  end if;
               end if;

               Addr (Byte_Pos) := Byte (Hi * 16 + Lo);
               exit when Byte_Pos = 0;
               Byte_Pos := Byte_Pos - 1;
            end;
         end loop;
      end;

      return Addr;
   end Hex_To_Address;

   --  Sugar command handlers
   procedure Handle_Sugar_Command (Cmd : String) is
      Res : Local_Executor.Exec_Result;
   begin
      --  deposit <amount> → SimpleVault Deposit
      if Cmd = "deposit" and Argument_Count >= 2 then
         declare
            Amt : constant String := Argument (2);
            Args : Local_Executor.Arg_Array (0 .. 0);
         begin
            Args (0) := To_Unbounded_String ("0x" & Amt);
            Local_Executor.Execute_Local
              (From_Zero, "SimpleVault", "Deposit", Args, 1_000_000, U256_Zero, Res);
            if Res.Success then
               Put_Line ("✓ Deposited " & Amt & " wei");
               Put_Line ("  Total: 0x" & To_String (Res.Return_Hex));
            else
               Put_Line ("✗ Deposit failed: " & To_String (Res.Error));
            end if;
         end;

      --  withdraw <amount> → SimpleVault Withdraw
      elsif Cmd = "withdraw" and Argument_Count >= 2 then
         declare
            Amt : constant String := Argument (2);
            Args : Local_Executor.Arg_Array (0 .. 0);
         begin
            Args (0) := To_Unbounded_String ("0x" & Amt);
            Local_Executor.Execute_Local
              (From_Zero, "SimpleVault", "Withdraw", Args, 1_000_000, U256_Zero, Res);
            if Res.Success then
               Put_Line ("✓ Withdrew " & Amt & " wei");
            else
               Put_Line ("✗ Withdraw failed: " & To_String (Res.Error));
            end if;
         end;

      --  balance [address] → SimpleVault BalanceOf or TotalDeposits
      elsif Cmd = "balance" then
         if Argument_Count >= 2 then
            declare
               Addr : constant String := Argument (2);
               Args : Local_Executor.Arg_Array (0 .. 0);
            begin
               Args (0) := To_Unbounded_String (Addr);
               Local_Executor.Execute_Local
                 (From_Zero, "SimpleVault", "BalanceOf", Args, 1_000_000, U256_Zero, Res);
               if Res.Success then
                  Put_Line ("Balance: 0x" & To_String (Res.Return_Hex));
               else
                  Put_Line ("✗ Query failed: " & To_String (Res.Error));
               end if;
            end;
         else
            declare
               Empty_Args : Local_Executor.Arg_Array (0 .. -1);
            begin
               Local_Executor.Execute_Local
                 (From_Zero, "SimpleVault", "TotalDeposits", Empty_Args, 1_000_000, U256_Zero, Res);
               if Res.Success then
                  Put_Line ("Total Vault Balance: 0x" & To_String (Res.Return_Hex));
               else
                  Put_Line ("✗ Query failed: " & To_String (Res.Error));
               end if;
            end;
         end if;

      --  transfer <to> <amount> → SimpleToken Transfer
      elsif (Cmd = "transfer" or Cmd = "send") and Argument_Count >= 3 then
         declare
            To_Addr : constant String := Argument (2);
            Amt     : constant String := Argument (3);
            Args : Local_Executor.Arg_Array (0 .. 1);
         begin
            Args (0) := To_Unbounded_String (To_Addr);
            Args (1) := To_Unbounded_String ("0x" & Amt);
            Local_Executor.Execute_Local
              (From_Zero, "SimpleToken", "Transfer", Args, 1_000_000, U256_Zero, Res);
            if Res.Success then
               Put_Line ("✓ Transferred " & Amt & " tokens to " & To_Addr);
            else
               Put_Line ("✗ Transfer failed: " & To_String (Res.Error));
            end if;
         end;

      --  tokens [address] → SimpleToken BalanceOf
      elsif Cmd = "tokens" then
         if Argument_Count >= 2 then
            declare
               Addr : constant String := Argument (2);
               Args : Local_Executor.Arg_Array (0 .. 0);
            begin
               Args (0) := To_Unbounded_String (Addr);
               Local_Executor.Execute_Local
                 (From_Zero, "SimpleToken", "BalanceOf", Args, 1_000_000, U256_Zero, Res);
               if Res.Success then
                  Put_Line ("Token Balance: 0x" & To_String (Res.Return_Hex));
               else
                  Put_Line ("✗ Query failed: " & To_String (Res.Error));
               end if;
            end;
         else
            declare
               Empty_Args : Local_Executor.Arg_Array (0 .. -1);
            begin
               Local_Executor.Execute_Local
                 (From_Zero, "SimpleToken", "TotalSupply", Empty_Args, 1_000_000, U256_Zero, Res);
               if Res.Success then
                  Put_Line ("Total Token Supply: 0x" & To_String (Res.Return_Hex));
               else
                  Put_Line ("✗ Query failed: " & To_String (Res.Error));
               end if;
            end;
         end if;

      --  count → HelloCounter Get_Count
      elsif Cmd = "count" then
         declare
            Empty_Args : Local_Executor.Arg_Array (0 .. -1);
         begin
            Local_Executor.Execute_Local
              (From_Zero, "HelloCounter", "Get_Count", Empty_Args, 1_000_000, U256_Zero, Res);
            if Res.Success then
               Put_Line ("Counter: 0x" & To_String (Res.Return_Hex));
            else
               Put_Line ("✗ Query failed: " & To_String (Res.Error));
            end if;
         end;

      --  inc [amount] → HelloCounter Increment or Increment_By
      elsif Cmd = "inc" then
         if Argument_Count >= 2 then
            declare
               Amt : constant String := Argument (2);
               Args : Local_Executor.Arg_Array (0 .. 0);
            begin
               Args (0) := To_Unbounded_String ("0x" & Amt);
               Local_Executor.Execute_Local
                 (From_Zero, "HelloCounter", "Increment_By", Args, 1_000_000, U256_Zero, Res);
            end;
         else
            declare
               Empty_Args : Local_Executor.Arg_Array (0 .. -1);
            begin
               Local_Executor.Execute_Local
                 (From_Zero, "HelloCounter", "Increment", Empty_Args, 1_000_000, U256_Zero, Res);
            end;
         end if;
         if Res.Success then
            Put_Line ("✓ Counter: 0x" & To_String (Res.Return_Hex));
         else
            Put_Line ("✗ Increment failed: " & To_String (Res.Error));
         end if;

      else
         Put_Line ("Unknown sugar command: " & Cmd);
         Put_Line ("Available: deposit, withdraw, balance, transfer, send, tokens, count, inc");
      end if;
   end Handle_Sugar_Command;

begin
   if Argument_Count = 0 then
      Put_Line ("Usage:");
      Put_Line ("  khepri-local [--from 0xADDR] [--gas N] [--value 0xAMOUNT] <contract> <entry> [args...]");
      Put_Line ("");
      Put_Line ("Quick commands:");
      Put_Line ("  khepri-local deposit <amount>           Deposit to vault");
      Put_Line ("  khepri-local withdraw <amount>          Withdraw from vault");
      Put_Line ("  khepri-local balance [address]          Check vault balance");
      Put_Line ("  khepri-local transfer <to> <amount>     Transfer tokens");
      Put_Line ("  khepri-local tokens [address]           Check token balance");
      Put_Line ("  khepri-local count                      Get counter value");
      Put_Line ("  khepri-local inc [amount]               Increment counter");
      Put_Line ("");
      Put_Line ("Examples:");
      Put_Line ("  khepri-local deposit 100");
      Put_Line ("  khepri-local HelloCounter Increment");
      Put_Line ("  khepri-local SimpleToken Transfer 0x<to> 0x<amount>");
      return;
   end if;

   --  Check for sugar commands (single lowercase word)
   declare
      First_Arg : constant String := Argument (1);
   begin
      if First_Arg'Length > 0 and then
         First_Arg (First_Arg'First) in 'a' .. 'z' and then
         First_Arg /= "HelloCounter" and then
         First_Arg /= "SimpleToken" and then
         First_Arg /= "SimpleVault" and then
         First_Arg /= "QuantumDID" and then
         First_Arg /= "Staking" and then
         First_Arg /= "Governance" and then
         First_Arg /= "QuantumVault"
      then
         Handle_Sugar_Command (First_Arg);
         return;
      end if;
   end;

   declare
      From_Addr  : Contract_Address := From_Zero;
      Gas        : Gas_Amount := 1_000_000;
      Call_Value : U256 := U256_Zero;

      --  Simple hex parser for U256 values (big-endian)
      function Parse_U256_Hex (S : String; Ok : out Boolean) return U256 is
         Hex  : String := S;

         function Hex_Digit (C : Character) return Integer is
         begin
            if C in '0' .. '9' then
               return Character'Pos (C) - Character'Pos ('0');
            elsif C in 'a' .. 'f' then
               return 10 + Character'Pos (C) - Character'Pos ('a');
            elsif C in 'A' .. 'F' then
               return 10 + Character'Pos (C) - Character'Pos ('A');
            else
               return -1;
            end if;
         end Hex_Digit;

         Bytes : Hash256 := (others => 0);
         Pos   : Natural := Hex'Length;
         Byte_Pos : Natural := 31;
      begin
         Ok := True;

         if Hex'Length >= 2 and then Hex (Hex'First) = '0' and then
           (Hex (Hex'First + 1) = 'x' or else Hex (Hex'First + 1) = 'X')
         then
            Hex := Hex (Hex'First + 2 .. Hex'Last);
            Pos := Hex'Length;
         end if;

         while Pos > 0 and Byte_Pos < 32 loop
            declare
               Lo_Char : constant Character := Hex (Hex'First + Pos - 1);
               Lo      : constant Integer   := Hex_Digit (Lo_Char);
               Hi      : Integer := 0;
            begin
               if Lo < 0 then
                  Ok := False;
                  exit;
               end if;
               Pos := Pos - 1;
               if Pos > 0 then
                  Hi := Hex_Digit (Hex (Hex'First + Pos - 1));
                  if Hi >= 0 then
                     Pos := Pos - 1;
                  else
                     Hi := 0;
                  end if;
               end if;

               Bytes (Byte_Pos) := Byte (Hi * 16 + Lo);
               exit when Byte_Pos = 0;
               Byte_Pos := Byte_Pos - 1;
            end;
         end loop;

         if not Ok then
            return U256_Zero;
         end if;

         return From_Bytes_BE (Bytes);
      end Parse_U256_Hex;

      --  Parse flags: [--from 0xADDR] [--gas N] [--value 0xAMOUNT]
      Arg_Index    : Natural := 1;
      Contract_Arg : Natural := 0;
      Entry_Arg    : Natural := 0;
   begin
      while Arg_Index <= Argument_Count loop
         declare
            A : constant String := Argument (Arg_Index);
         begin
            if A = "--from" and then Arg_Index + 1 <= Argument_Count then
               From_Addr := Hex_To_Address (Argument (Arg_Index + 1));
               Arg_Index := Arg_Index + 2;
            elsif A = "--gas" and then Arg_Index + 1 <= Argument_Count then
               declare
                  Val : Long_Long_Integer;
               begin
                  begin
                     Val := Long_Long_Integer'Value (Argument (Arg_Index + 1));
                     if Val < 0 then
                        Gas := 0;
                     else
                        Gas := Gas_Amount (Val);
                     end if;
                  exception
                     when others =>
                        Gas := 1_000_000;
                  end;
               end;
               Arg_Index := Arg_Index + 2;
            elsif A = "--value" and then Arg_Index + 1 <= Argument_Count then
               declare
                  Ok : Boolean;
               begin
                  Call_Value := Parse_U256_Hex (Argument (Arg_Index + 1), Ok);
                  if not Ok then
                     Put_Line ("warning: invalid --value, using 0");
                     Call_Value := U256_Zero;
                  end if;
               end;
               Arg_Index := Arg_Index + 2;
            elsif A'Length >= 2 and then A (A'First) = '-' and then A (A'First + 1) = '-' then
               Put_Line ("Unknown option: " & A);
               return;
            else
               exit;
            end if;
         end;
      end loop;

      if Arg_Index > Argument_Count - 1 then
         Put_Line ("Usage:");
         Put_Line ("  khepri-local [--from 0xADDR] [--gas N] [--value 0xAMOUNT] <contract> <entry> [arg1_hex [arg2_hex ...]]");
         return;
      end if;

      Contract_Arg := Arg_Index;
      Entry_Arg    := Arg_Index + 1;

      declare
         Contract_Name : constant String := Argument (Contract_Arg);
         Entry_Name    : constant String := Argument (Entry_Arg);

         Res       : Local_Executor.Exec_Result;
         Arg_Start : constant Natural := Entry_Arg + 1;
         Arg_Count : constant Natural :=
           (if Arg_Start <= Argument_Count then Argument_Count - Arg_Start + 1
            else 0);
      begin
         if Arg_Count = 0 then
            declare
               Empty_Args : Local_Executor.Arg_Array (0 .. -1);
            begin
               Local_Executor.Execute_Local
                 (From_Address  => From_Addr,
                  Contract_Name => Contract_Name,
                  Entry_Point   => Entry_Name,
                  Args          => Empty_Args,
                  Gas_Limit     => Gas,
                  Value         => Call_Value,
                  Result        => Res);
            end;
         else
            declare
               Arg_List : Local_Executor.Arg_Array (0 .. Arg_Count - 1);
            begin
               for I in 0 .. Arg_Count - 1 loop
                  Arg_List (I) :=
                    To_Unbounded_String (Argument (Arg_Start + I));
               end loop;

               Local_Executor.Execute_Local
                 (From_Address  => From_Addr,
                  Contract_Name => Contract_Name,
                  Entry_Point   => Entry_Name,
                  Args          => Arg_List,
                  Gas_Limit     => Gas,
                  Value         => Call_Value,
                  Result        => Res);
            end;
         end if;

         if Res.Success then
            Put_Line ("success: true");
            Put_Line ("gas_used: " & Gas_Amount'Image (Res.Gas_Used));
            if Length (Res.Return_Hex) > 0 then
               Put_Line ("return: 0x" & To_String (Res.Return_Hex));
            end if;
         else
            Put_Line ("success: false");
            Put_Line ("gas_used: " & Gas_Amount'Image (Res.Gas_Used));
            if Length (Res.Error) > 0 then
               Put_Line ("error: " & To_String (Res.Error));
            end if;
         end if;
      end;
   end;

end Khepri_Local_Main;
