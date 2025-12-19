--  KHEPRI-AEGIS Integration Adapter
--
--  This package bridges the Khepri state management system with the
--  AEGIS execution environment. It provides the Storage_Load/Storage_Store
--  interface required by Aegis_Execution while using Khepri MPT underneath.
--
--  Certification Target: GOLD
pragma SPARK_Mode (On);

with Aegis_VM_Types;     use Aegis_VM_Types;
with Aegis_U256;         use Aegis_U256;
with Aegis_Execution;    use Aegis_Execution;
with Aegis_Storage;      use Aegis_Storage;
with Khepri_State_Manager;

package Khepri_Aegis_Adapter with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Storage Operations (Aegis_Execution interface)
   ---------------------------------------------------------------------------

   --  Load storage value (called by SLOAD syscall)
   procedure Storage_Load (
      Ctx     : in Out Execution_Context;
      Address : in     Contract_Address;
      Key     : in     Storage_Key;
      Value   : out    Storage_Value;
      Success : out    Boolean
   ) with
      Global => (Input => Khepri_State_Manager.State_Manager_Data);

   --  Store storage value (called by SSTORE syscall)
   procedure Storage_Store (
      Ctx     : in Out Execution_Context;
      Address : in     Contract_Address;
      Key     : in     Storage_Key;
      Value   : in     Storage_Value;
      Success : out    Boolean
   ) with
      Global => (In_Out => Khepri_State_Manager.State_Manager_Data);

   ---------------------------------------------------------------------------
   --  Account State Operations
   ---------------------------------------------------------------------------

   --  Get account balance
   function Get_Balance (
      Ctx     : Execution_Context;
      Address : Contract_Address
   ) return U256 with
      Global => (Input => Khepri_State_Manager.State_Manager_Data);

   --  Get account nonce
   function Get_Nonce (
      Ctx     : Execution_Context;
      Address : Contract_Address
   ) return Word64 with
      Global => (Input => Khepri_State_Manager.State_Manager_Data);

   --  Get account code hash
   function Get_Code_Hash (
      Ctx     : Execution_Context;
      Address : Contract_Address
   ) return Hash256 with
      Global => (Input => Khepri_State_Manager.State_Manager_Data);

   --  Check if account is contract
   function Is_Contract (
      Ctx     : Execution_Context;
      Address : Contract_Address
   ) return Boolean with
      Global => (Input => Khepri_State_Manager.State_Manager_Data);

   ---------------------------------------------------------------------------
   --  Transaction Lifecycle
   ---------------------------------------------------------------------------

   --  Begin new transaction
   procedure Begin_Transaction (
      Success : out Boolean
   ) with
      Global => (In_Out => Khepri_State_Manager.State_Manager_Data);

   --  Commit transaction
   procedure Commit_Transaction (
      New_Root : out Hash256;
      Success  : out Boolean
   ) with
      Global => (In_Out => Khepri_State_Manager.State_Manager_Data);

   --  Rollback transaction
   procedure Rollback_Transaction (
      Success : out Boolean
   ) with
      Global => (In_Out => Khepri_State_Manager.State_Manager_Data);

   ---------------------------------------------------------------------------
   --  Snapshot Management (for nested calls)
   ---------------------------------------------------------------------------

   --  Create state snapshot
   procedure Create_State_Snapshot (
      Snapshot_ID : out Snapshot_ID;
      Success     : out Boolean
   ) with
      Global => (In_Out => Khepri_State_Manager.State_Manager_Data);

   --  Rollback to snapshot
   procedure Rollback_State_Snapshot (
      Snapshot_ID : in     Snapshot_ID;
      Success     : out    Boolean
   ) with
      Global => (In_Out => Khepri_State_Manager.State_Manager_Data);

   --  Commit snapshot
   procedure Commit_State_Snapshot (
      Snapshot_ID : in Snapshot_ID
   ) with
      Global => (In_Out => Khepri_State_Manager.State_Manager_Data);

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize state system
   procedure Initialize_State_System (
      Success : out Boolean
   ) with
      Global => (Output => Khepri_State_Manager.State_Manager_Data);

   --  Initialize from existing root
   procedure Initialize_State_From_Root (
      Root    : in     Hash256;
      Success : out    Boolean
   ) with
      Global => (Output => Khepri_State_Manager.State_Manager_Data);

   --  Get current state root
   function Get_State_Root return Hash256 with
      Global => (Input => Khepri_State_Manager.State_Manager_Data),
      Volatile_Function;

end Khepri_Aegis_Adapter;
