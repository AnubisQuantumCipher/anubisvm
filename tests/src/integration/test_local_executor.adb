with Ada.Text_IO; use Ada.Text_IO;

with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_U256;      use Aegis_U256;
with Local_Executor;

procedure Test_Local_Executor is

   From_Zero : constant Contract_Address := (others => 0);

   procedure Run_Call
     (Contract : String;
      Entry    : String;
      Args     : Local_Executor.Arg_Array) is
      Res : Local_Executor.Exec_Result;
   begin
      Local_Executor.Execute_Local
        (From_Address  => From_Zero,
         Contract_Name => Contract,
         Entry_Point   => Entry,
         Args          => Args,
         Gas_Limit     => 1_000_000,
         Value         => U256_Zero,
         Result        => Res);

      Put_Line ("Call: " & Contract & "." & Entry);
      Put_Line ("  success: " & (if Res.Success then "true" else "false"));
      Put_Line ("  gas_used:" & Gas_Amount'Image (Res.Gas_Used));
      if Res.Return_Hex'Length > 0 then
         Put_Line ("  return: 0x" & Res.Return_Hex);
      end if;
      if (not Res.Success) and then Res.Error'Length > 0 then
         Put_Line ("  error: " & Res.Error);
      end if;
   end Run_Call;

   Empty_Args : Local_Executor.Arg_Array (0 .. -1);

begin
   Put_Line ("[Local_Executor] HelloCounter flow");

   Run_Call ("HelloCounter", "Initialize", Empty_Args);
   Run_Call ("HelloCounter", "Get_Count", Empty_Args);
   Run_Call ("HelloCounter", "Increment", Empty_Args);
   Run_Call ("HelloCounter", "Get_Count", Empty_Args);

   Put_Line ("[Local_Executor] SimpleToken flow");

   declare
      Args_Init : Local_Executor.Arg_Array (0 .. 0);
   begin
      Args_Init (0) := "0x01";
      Run_Call ("SimpleToken", "Initialize", Args_Init);
      Run_Call ("SimpleToken", "TotalSupply", Empty_Args);
   end;

end Test_Local_Executor;

