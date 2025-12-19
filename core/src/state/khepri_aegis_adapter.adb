--  KHEPRI-AEGIS Integration Adapter Implementation
pragma SPARK_Mode (On);

with Khepri_State_Trie;

package body Khepri_Aegis_Adapter with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   --  Track current transaction snapshot
   Current_Snapshot : Snapshot_ID := 0;
   Snapshot_Valid   : Boolean := False;

   ---------------------------------------------------------------------------
   --  Storage Operations
   ---------------------------------------------------------------------------

   procedure Storage_Load (
      Ctx     : in Out Execution_Context;
      Address : in     Contract_Address;
      Key     : in     Storage_Key;
      Value   : out    Storage_Value;
      Success : out    Boolean
   ) is
      pragma Unreferenced (Ctx);
      Slot  : constant U256 := U256 (Key);
      Val   : U256;
      Error : Khepri_State_Manager.State_Manager_Error;
   begin
      Khepri_State_Manager.SLOAD (Address, Slot, Val, Success, Error);

      if Success then
         Value := Storage_Value (Val);
      else
         Value := Storage_Value (U256_Zero);
      end if;
   end Storage_Load;

   procedure Storage_Store (
      Ctx     : in Out Execution_Context;
      Address : in     Contract_Address;
      Key     : in     Storage_Key;
      Value   : in     Storage_Value;
      Success : out    Boolean
   ) is
      pragma Unreferenced (Ctx);
      Slot  : constant U256 := U256 (Key);
      Val   : constant U256 := U256 (Value);
      Error : Khepri_State_Manager.State_Manager_Error;
   begin
      Khepri_State_Manager.SSTORE (Address, Slot, Val, Success, Error);
   end Storage_Store;

   ---------------------------------------------------------------------------
   --  Account State Operations
   ---------------------------------------------------------------------------

   function Get_Balance (
      Ctx     : Execution_Context;
      Address : Contract_Address
   ) return U256 is
      pragma Unreferenced (Ctx);
   begin
      return Khepri_State_Manager.Get_Balance (Address);
   end Get_Balance;

   function Get_Nonce (
      Ctx     : Execution_Context;
      Address : Contract_Address
   ) return Word64 is
      pragma Unreferenced (Ctx);
   begin
      return Khepri_State_Trie.Get_Nonce (Address);
   end Get_Nonce;

   function Get_Code_Hash (
      Ctx     : Execution_Context;
      Address : Contract_Address
   ) return Hash256 is
      pragma Unreferenced (Ctx);
   begin
      return Khepri_State_Trie.Get_Code_Hash (Address);
   end Get_Code_Hash;

   function Is_Contract (
      Ctx     : Execution_Context;
      Address : Contract_Address
   ) return Boolean is
      pragma Unreferenced (Ctx);
   begin
      return Khepri_State_Trie.Is_Contract (Address);
   end Is_Contract;

   ---------------------------------------------------------------------------
   --  Transaction Lifecycle
   ---------------------------------------------------------------------------

   procedure Begin_Transaction (
      Success : out Boolean
   ) is
   begin
      --  Create snapshot for entire transaction
      Khepri_State_Manager.Create_Snapshot (Current_Snapshot, Success);
      Snapshot_Valid := Success;
   end Begin_Transaction;

   procedure Commit_Transaction (
      New_Root : out Hash256;
      Success  : out Boolean
   ) is
   begin
      --  Commit all changes and get new state root
      Khepri_State_Manager.Commit_State (New_Root, Success);

      --  Discard transaction snapshot
      if Snapshot_Valid then
         Khepri_State_Manager.Commit_Snapshot (Current_Snapshot);
         Snapshot_Valid := False;
      end if;
   end Commit_Transaction;

   procedure Rollback_Transaction (
      Success : out Boolean
   ) is
      Error : Khepri_State_Manager.State_Manager_Error;
   begin
      --  Rollback to transaction start
      if Snapshot_Valid then
         Khepri_State_Manager.Rollback_Snapshot (Current_Snapshot, Success, Error);
         Snapshot_Valid := False;
      else
         Success := False;
      end if;
   end Rollback_Transaction;

   ---------------------------------------------------------------------------
   --  Snapshot Management
   ---------------------------------------------------------------------------

   procedure Create_State_Snapshot (
      Snapshot_ID : out Snapshot_ID;
      Success     : out Boolean
   ) is
      Temp_ID : Natural;
   begin
      Khepri_State_Manager.Create_Snapshot (Temp_ID, Success);
      if Success and then Temp_ID <= Snapshot_ID'Last then
         Snapshot_ID := Snapshot_ID (Temp_ID);
      else
         Snapshot_ID := 0;
         Success := False;
      end if;
   end Create_State_Snapshot;

   procedure Rollback_State_Snapshot (
      Snapshot_ID : in     Snapshot_ID;
      Success     : out    Boolean
   ) is
      Error : Khepri_State_Manager.State_Manager_Error;
   begin
      Khepri_State_Manager.Rollback_Snapshot (Natural (Snapshot_ID), Success, Error);
   end Rollback_State_Snapshot;

   procedure Commit_State_Snapshot (
      Snapshot_ID : in Snapshot_ID
   ) is
   begin
      Khepri_State_Manager.Commit_Snapshot (Natural (Snapshot_ID));
   end Commit_State_Snapshot;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize_State_System (
      Success : out Boolean
   ) is
   begin
      Khepri_State_Manager.Initialize (Success);
   end Initialize_State_System;

   procedure Initialize_State_From_Root (
      Root    : in     Hash256;
      Success : out    Boolean
   ) is
      Error : Khepri_State_Manager.State_Manager_Error;
      Root_256 : Hash_256;
   begin
      --  Convert Hash256 to Hash_256
      for I in 0 .. 31 loop
         Root_256 (I) := Root (I);
      end loop;

      Khepri_State_Manager.Initialize_From_Root (Root_256, Success, Error);
   end Initialize_State_From_Root;

   function Get_State_Root return Hash256 is
      Root_256 : constant Hash_256 := Khepri_State_Manager.Get_State_Root;
      Result   : Hash256;
   begin
      --  Convert Hash_256 to Hash256
      for I in 0 .. 31 loop
         Result (I) := Root_256 (I);
      end loop;
      return Result;
   end Get_State_Root;

end Khepri_Aegis_Adapter;
