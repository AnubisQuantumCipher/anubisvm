pragma SPARK_Mode (On);

package body Khepri_Types is

   ---------------------------------------------------------------------------
   --  Integer Conversion Helpers
   ---------------------------------------------------------------------------

   function From_Natural (N : Natural) return Uint256 is
   begin
      return From_Word64 (Word64 (N));
   end From_Natural;

   ---------------------------------------------------------------------------
   --  Arithmetic Operations
   ---------------------------------------------------------------------------

   function "+" (Left, Right : Uint256) return Uint256 is
   begin
      return Add_Mod (Left, Right);
   end "+";

   function "-" (Left, Right : Uint256) return Uint256 is
   begin
      return Sub_Mod (Left, Right);
   end "-";

   function "*" (Left, Right : Uint256) return Uint256 is
   begin
      return Mul_Mod (Left, Right);
   end "*";

   function "/" (Left, Right : Uint256) return Uint256 is
   begin
      return Div (Left, Right);
   end "/";

   function "mod" (Left, Right : Uint256) return Uint256 is
   begin
      return Mod_Op (Left, Right);
   end "mod";

   ---------------------------------------------------------------------------
   --  Result Constructors
   ---------------------------------------------------------------------------

   function Ok (Value : Uint256) return Result is
   begin
      return (Success => True, Value => Value);
   end Ok;

   function Err (Code : Error_Code) return Result is
   begin
      return (Success => False, Error => Code);
   end Err;

   function Ok_Bool (Value : Boolean) return Bool_Result is
   begin
      return (Success => True, Value => Value);
   end Ok_Bool;

end Khepri_Types;
