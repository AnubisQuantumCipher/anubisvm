--  Contract Template for AnubisVM
--  Replace CONTRACT_TEMPLATE with your contract name
--
--  SPARK/Ada Smart Contract Specification
--  Post-Quantum Secure | Formally Verified

pragma SPARK_Mode (On);

with CVM_Types;     use CVM_Types;
with CVM_Interface; use CVM_Interface;

package Contract_Template with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Contract Entry Points
   --
   --  Each entry point must have:
   --    - Context : in     Call_Context  (caller info, calldata, gas)
   --    - State   : in Out State_Array   (contract storage)
   --    - Result  :    out Exec_Result   (success/failure, gas used, return data)
   ---------------------------------------------------------------------------

   --  Initialize the contract (called once on deployment)
   procedure Initialize (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  :    out Exec_Result
   ) with
      Global  => null,
      Depends => (Result => Context, State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Result.Success or Result.Error_Code /= 0;

   --  Example: Transfer value between accounts
   procedure Transfer (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  :    out Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => (Context, State)),
      Pre     => Is_Valid_Context (Context),
      Post    => Result.Success or Result.Error_Code /= 0;

   --  Example: Query balance (read-only)
   procedure Balance_Of (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  :    out Exec_Result
   ) with
      Global  => null,
      Depends => (Result => (Context, State), State => State),
      Pre     => Is_Valid_Context (Context),
      Post    => Result.Success;

   ---------------------------------------------------------------------------
   --  CVM Registration
   ---------------------------------------------------------------------------

   --  Returns the contract descriptor for registry
   function Get_Descriptor return CVM_Descriptor;

end Contract_Template;
