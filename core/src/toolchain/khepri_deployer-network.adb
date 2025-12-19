pragma SPARK_Mode (Off);

with Khepri_RPC_Client; use Khepri_RPC_Client;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Real_Time;

package body Khepri_Deployer.Network is

   ---------------------------------------------------------------------------
   --  Convert Bounded_String to String
   ---------------------------------------------------------------------------

   function To_String (BS : Bounded_String) return String is
      Result : String (1 .. BS.Length);
   begin
      for I in 0 .. BS.Length - 1 loop
         Result (I + 1) := BS.Data (I);
      end loop;
      return Result;
   end To_String;

   ---------------------------------------------------------------------------
   --  Network Transaction Submission
   ---------------------------------------------------------------------------

   procedure Submit_Transaction_RPC (
      Session       : in     Deployment_Session;
      Transaction   : in     Deploy_Transaction;
      Tx_Hash       : out    Hash256;
      Success       : out    Boolean
   ) is
      Serialized : Byte_Array (0 .. 65535);
      Tx_Size : Natural;
      Serialize_Success : Boolean;
      RPC_Endpoint : Khepri_RPC_Client.RPC_Endpoint;
      RPC_Err : RPC_Error;
   begin
      Success := False;
      Tx_Hash := (others => 0);

      --  Verify transaction is signed
      if not Transaction.Is_Signed then
         return;
      end if;

      --  Serialize transaction
      Serialize_Transaction (Transaction, Serialized, Tx_Size, Serialize_Success);

      if not Serialize_Success or Tx_Size = 0 then
         return;
      end if;

      --  Parse RPC endpoint from session config
      declare
         URL : constant String := To_String (Session.Config.Network_Cfg.RPC_Endpoint);
      begin
         if URL'Length = 0 then
            --  No RPC endpoint configured, return error
            return;
         end if;

         RPC_Endpoint := Parse_Endpoint (URL);
      end;

      --  Submit via RPC
      Send_Raw_Transaction (
         Endpoint  => RPC_Endpoint,
         Signed_Tx => Serialized (0 .. Tx_Size - 1),
         Tx_Hash   => Tx_Hash,
         Error     => RPC_Err,
         Success   => Success
      );

      --  Log error if failed (in production, would use proper logging)
      if not Success then
         case RPC_Err.Code is
            when RPC_Success =>
               null;
            when RPC_Tx_Underpriced =>
               null;  --  Transaction underpriced
            when RPC_Tx_Already_Known =>
               null;  --  Already known
            when RPC_Network_Error =>
               null;  --  Network error
            when others =>
               null;  --  Other error
         end case;
      end if;
   end Submit_Transaction_RPC;

   ---------------------------------------------------------------------------
   --  Network Receipt Polling
   ---------------------------------------------------------------------------

   procedure Wait_Confirmation_RPC (
      Session       : in     Deployment_Session;
      Tx_Hash       : in     Hash256;
      Confirmations : in     Natural;
      Receipt       : out    Deployment_Receipt;
      Success       : out    Boolean
   ) is
      RPC_Endpoint : Khepri_RPC_Client.RPC_Endpoint;
      RPC_Receipt : Transaction_Receipt;
      RPC_Err : RPC_Error;
      Max_Wait : constant Natural := 300;  --  5 minutes max
   begin
      Success := False;
      Receipt := (
         Contract_Address    => (others => 0),
         Transaction_Hash    => Tx_Hash,
         Block_Number        => 0,
         Gas_Used            => 0,
         Effective_Gas_Price => 0,
         Status              => Deploy_Error,
         Error_Message       => Empty_String,
         Timestamp           => 0
      );

      --  Parse RPC endpoint from session config
      declare
         URL : constant String := To_String (Session.Config.Network_Cfg.RPC_Endpoint);
      begin
         if URL'Length = 0 then
            return;
         end if;

         RPC_Endpoint := Parse_Endpoint (URL);
      end;

      --  Wait for confirmations via RPC polling
      Wait_Confirmations (
         Endpoint         => RPC_Endpoint,
         Tx_Hash          => Tx_Hash,
         Confirmations    => Confirmations,
         Max_Wait_Seconds => Max_Wait,
         Receipt          => RPC_Receipt,
         Error            => RPC_Err,
         Success          => Success
      );

      if not Success then
         Receipt.Status := Deploy_Error;
         return;
      end if;

      --  Convert RPC receipt to deployment receipt
      Receipt.Transaction_Hash := RPC_Receipt.Transaction_Hash;
      Receipt.Block_Number := RPC_Receipt.Block_Number;
      Receipt.Gas_Used := RPC_Receipt.Gas_Used;
      Receipt.Effective_Gas_Price := RPC_Receipt.Effective_Gas_Price;
      Receipt.Contract_Address := RPC_Receipt.Contract_Address;

      --  Map status
      if RPC_Receipt.Status then
         Receipt.Status := Deploy_Success;
      else
         Receipt.Status := Deploy_Failed;
      end if;

      --  Set timestamp (would be from block in production)
      Receipt.Timestamp := Word64 (Ada.Real_Time.To_Duration (Ada.Real_Time.Clock));

      Success := True;
   end Wait_Confirmation_RPC;

   ---------------------------------------------------------------------------
   --  Network Code Verification
   ---------------------------------------------------------------------------

   procedure Get_Deployed_Code_RPC (
      Session       : in     Deployment_Session;
      Contract_Addr : in     Khepri_Types.Address;
      Code          : out    Byte_Array;
      Code_Size     : out    Natural;
      Success       : out    Boolean
   ) is
      RPC_Endpoint : Khepri_RPC_Client.RPC_Endpoint;
      RPC_Err : RPC_Error;
   begin
      Success := False;
      Code := (others => 0);
      Code_Size := 0;

      --  Parse RPC endpoint from session config
      declare
         URL : constant String := To_String (Session.Config.Network_Cfg.RPC_Endpoint);
      begin
         if URL'Length = 0 then
            return;
         end if;

         RPC_Endpoint := Parse_Endpoint (URL);
      end;

      --  Fetch code via RPC
      Get_Code (
         Endpoint      => RPC_Endpoint,
         Contract_Addr => Contract_Addr,
         Code          => Code,
         Code_Size     => Code_Size,
         Error         => RPC_Err,
         Success       => Success
      );
   end Get_Deployed_Code_RPC;

end Khepri_Deployer.Network;
