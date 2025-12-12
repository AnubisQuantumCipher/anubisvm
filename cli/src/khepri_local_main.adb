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

begin
   if Argument_Count = 0 then
      Put_Line ("Usage:");
      Put_Line ("  khepri-local [--from 0xADDR] [--gas N] [--value 0xAMOUNT] <contract> <entry> [arg1_hex [arg2_hex ...]]");
      Put_Line ("Examples:");
      Put_Line ("  khepri-local HelloCounter Increment");
      Put_Line ("  khepri-local SimpleToken Transfer 0x<to> 0x<amount>");
      return;
   end if;

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
