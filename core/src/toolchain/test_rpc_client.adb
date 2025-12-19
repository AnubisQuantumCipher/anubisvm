pragma SPARK_Mode (Off);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Khepri_RPC_Client; use Khepri_RPC_Client;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;

--  Simple test program for RPC client
--  Usage: test_rpc_client <endpoint>
--
--  Example:
--    test_rpc_client http://localhost:8545

procedure Test_RPC_Client is
   Endpoint : RPC_Endpoint;
   Block_Number : Word64;
   Error : RPC_Error;
   Success : Boolean;

   function To_String (BS : Bounded_String) return String is
      Result : String (1 .. BS.Length);
   begin
      for I in 0 .. BS.Length - 1 loop
         Result (I + 1) := BS.Data (I);
      end loop;
      return Result;
   end To_String;

begin
   Put_Line ("AnubisVM RPC Client Test");
   Put_Line ("========================");
   New_Line;

   --  Parse endpoint from command line or use default
   declare
      URL : constant String := (if Ada.Command_Line.Argument_Count > 0
                                then Ada.Command_Line.Argument (1)
                                else "http://localhost:8545");
   begin
      Put_Line ("Connecting to: " & URL);
      Endpoint := Parse_Endpoint (URL);
      Put_Line ("Host: " & To_String (Endpoint.Host));
      Put_Line ("Port:" & Natural'Image (Endpoint.Port));
      Put_Line ("TLS: " & Boolean'Image (Endpoint.Use_TLS));
      New_Line;
   end;

   --  Test 1: Get block number
   Put_Line ("Test 1: Get current block number");
   Get_Block_Number (Endpoint, Block_Number, Error, Success);

   if Success then
      Put_Line ("  SUCCESS: Block number =" & Word64'Image (Block_Number));
   else
      Put_Line ("  FAILED: " & RPC_Error_Code'Image (Error.Code));
      declare
         Msg : String (1 .. Error.Message.Length);
      begin
         for I in 0 .. Error.Message.Length - 1 loop
            Msg (I + 1) := Error.Message.Data (I);
         end loop;
         if Msg'Length > 0 then
            Put_Line ("  Message: " & Msg);
         end if;
      end;
   end if;
   New_Line;

   --  Test 2: Get balance of zero address
   declare
      Balance : Uint256;
      Zero_Addr : constant Address := (others => 0);
   begin
      Put_Line ("Test 2: Get balance of zero address");
      Get_Balance (Endpoint, Zero_Addr, Balance, Error, Success);

      if Success then
         Put_Line ("  SUCCESS: Balance retrieved");
         Put_Line ("  Limb[0]:" & Word64'Image (Balance.Limbs (0)));
      else
         Put_Line ("  FAILED: " & RPC_Error_Code'Image (Error.Code));
      end if;
   end;
   New_Line;

   --  Test 3: Get code of zero address (should be empty)
   declare
      Code : Byte_Array (0 .. 65535);
      Code_Size : Natural;
      Zero_Addr : constant Address := (others => 0);
   begin
      Put_Line ("Test 3: Get code of zero address");
      Get_Code (Endpoint, Zero_Addr, Code, Code_Size, Error, Success);

      if Success then
         Put_Line ("  SUCCESS: Code size =" & Natural'Image (Code_Size));
      else
         Put_Line ("  FAILED: " & RPC_Error_Code'Image (Error.Code));
      end if;
   end;
   New_Line;

   Put_Line ("Tests completed");

exception
   when E : others =>
      Put_Line ("ERROR: " & Ada.Exceptions.Exception_Information (E));
end Test_RPC_Client;
