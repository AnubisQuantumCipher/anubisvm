pragma SPARK_Mode (Off);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_Types; use Khepri_Types;

--  KHEPRI Deployer Network Layer
--
--  This child package handles network I/O for transaction submission
--  and receipt polling. SPARK_Mode is Off due to network operations.
--
--  This package wraps Khepri_RPC_Client to provide deployer-specific
--  network functionality while maintaining clean separation from
--  the SPARK-verified core deployer logic.

package Khepri_Deployer.Network is

   ---------------------------------------------------------------------------
   --  Network Transaction Submission
   ---------------------------------------------------------------------------

   --  Submit signed transaction via RPC and return transaction hash
   --  This wraps the RPC client with deployer-specific logic
   procedure Submit_Transaction_RPC (
      Session       : in     Deployment_Session;
      Transaction   : in     Deploy_Transaction;
      Tx_Hash       : out    Hash256;
      Success       : out    Boolean
   );

   ---------------------------------------------------------------------------
   --  Network Receipt Polling
   ---------------------------------------------------------------------------

   --  Wait for transaction confirmation via RPC polling
   --  Polls until receipt is available and required confirmations met
   procedure Wait_Confirmation_RPC (
      Session       : in     Deployment_Session;
      Tx_Hash       : in     Hash256;
      Confirmations : in     Natural;
      Receipt       : out    Deployment_Receipt;
      Success       : out    Boolean
   );

   ---------------------------------------------------------------------------
   --  Network Code Verification
   ---------------------------------------------------------------------------

   --  Fetch deployed contract code and verify against expected hash
   procedure Get_Deployed_Code_RPC (
      Session       : in     Deployment_Session;
      Contract_Addr : in     Khepri_Types.Address;
      Code          : out    Byte_Array;
      Code_Size     : out    Natural;
      Success       : out    Boolean
   );

end Khepri_Deployer.Network;
