pragma SPARK_Mode (On);

with Anubis_Node_Types; use Anubis_Node_Types;
with Aegis_VM_Types; use Aegis_VM_Types;

--  Node Contract Registry: In-memory contract storage for devnet
--
--  Stores deployed contracts and their code for the duration of a
--  node session. Production would persist to disk/MPT.

package Node_Contract_Registry with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Contract Storage Types
   ---------------------------------------------------------------------------

   --  Maximum stored contracts
   Max_Stored_Contracts : constant := 256;
   subtype Stored_Contract_Index is Natural range 0 .. Max_Stored_Contracts - 1;

   --  Stored contract entry
   type Stored_Contract is record
      Is_Valid     : Boolean;
      Contract_ID  : Contract_Address;
      Code_Hash    : Hash256;
      Manifest     : Node_Contract_Manifest;
      Code         : Node_Code_Buffer;
      Code_Size    : Natural;
      Deploy_Block : U256;
   end record;

   --  Empty stored contract
   Empty_Stored_Contract : constant Stored_Contract := (
      Is_Valid     => False,
      Contract_ID  => (others => 0),
      Code_Hash    => (others => 0),
      Manifest     => (
         Name          => (others => ' '),
         Name_Len      => 0,
         Version_Major => 0,
         Version_Minor => 0,
         Version_Patch => 0,
         Cert          => Cert_None
      ),
      Code         => (others => 0),
      Code_Size    => 0,
      Deploy_Block => (Limbs => (0, 0, 0, 0))
   );

   --  Registry storage
   type Contract_Storage is array (Stored_Contract_Index) of Stored_Contract;

   --  Registry state
   type Registry_State is record
      Contracts     : Contract_Storage;
      Contract_Count : Natural;
      Is_Initialized : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Registry Operations
   ---------------------------------------------------------------------------

   --  Initialize registry
   procedure Initialize (Registry : out Registry_State) with
      Global => null,
      Post   => Registry.Is_Initialized and Registry.Contract_Count = 0;

   --  Register a new contract
   procedure Register_Contract (
      Registry  : in out Registry_State;
      Manifest  : in     Node_Contract_Manifest;
      Code      : in     Node_Code_Buffer;
      Code_Size : in     Natural;
      Block_Num : in     U256;
      Result    : out    Stored_Contract;
      Success   : out    Boolean
   ) with
      Global => null,
      Pre    => Registry.Is_Initialized and
                Code_Size > 0 and Code_Size <= Node_Max_Code_Size and
                Registry.Contract_Count < Max_Stored_Contracts,
      Post   => (Success and then
                   (Registry.Contract_Count = Registry.Contract_Count'Old + 1) and then
                   Result.Is_Valid)
                or else (not Success and then
                   (Registry.Contract_Count = Registry.Contract_Count'Old));

   --  Look up contract by ID
   function Get_Contract (
      Registry    : Registry_State;
      Contract_ID : Contract_Address
   ) return Stored_Contract with
      Global => null,
      Pre    => Registry.Is_Initialized;

   --  Check if contract exists
   function Contract_Exists (
      Registry    : Registry_State;
      Contract_ID : Contract_Address
   ) return Boolean with
      Global => null,
      Pre    => Registry.Is_Initialized;

   --  Get contract count
   function Count (Registry : Registry_State) return Natural with
      Global => null,
      Pre    => Registry.Is_Initialized;

end Node_Contract_Registry;
