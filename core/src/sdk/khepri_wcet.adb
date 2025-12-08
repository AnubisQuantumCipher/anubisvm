--  KHEPRI WCET Analysis Implementation
pragma SPARK_Mode (On);

package body Khepri_WCET with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Loop Analysis
   ---------------------------------------------------------------------------

   function Loop_Gas (Bound : Loop_Bound) return Execution_Bound is
      Min_Gas : Gas;
      Max_Gas : Gas;
      Avg_Gas : Gas;
   begin
      Min_Gas := Gas (Bound.Min_Iterations) * Bound.Body_Cost;
      Max_Gas := Gas (Bound.Max_Iterations) * Bound.Body_Cost;

      --  Average is midpoint of min/max
      Avg_Gas := (Min_Gas + Max_Gas) / 2;

      return (
         Min_Gas    => Min_Gas,
         Max_Gas    => Max_Gas,
         Avg_Gas    => Avg_Gas,
         Confidence => (if Bound.Proven then 100 else 50)
      );
   end Loop_Gas;

   ---------------------------------------------------------------------------
   --  Gas Estimation Functions
   ---------------------------------------------------------------------------

   function Hash_Gas (
      Algorithm  : Natural;
      Input_Size : Natural;
      Model      : Cost_Model
   ) return Gas is
      Words     : Natural;
      Base      : Gas;
      Per_Word  : Gas;
   begin
      --  Round up to 32-byte words
      Words := (Input_Size + 31) / 32;

      case Algorithm is
         when 0 =>      --  SHA3
            Base := Model.Crypto.SHA3_Base;
            Per_Word := Model.Crypto.SHA3_Per_Word;
         when 1 =>      --  BLAKE2
            Base := Model.Crypto.BLAKE2_Base;
            Per_Word := Model.Crypto.BLAKE2_Per_Word;
         when others =>
            --  Default to SHA3 costs
            Base := Model.Crypto.SHA3_Base;
            Per_Word := Model.Crypto.SHA3_Per_Word;
      end case;

      return Base + Gas (Words) * Per_Word;
   end Hash_Gas;

   function Verify_Gas (
      Algorithm : Natural;
      Model     : Cost_Model
   ) return Gas is
   begin
      case Algorithm is
         when 0 =>
            return Model.Crypto.ECDSA_Verify;
         when 1 =>
            return Model.Crypto.ED25519_Verify;
         when 2 =>
            return Model.Crypto.MLDSA_Verify;
         when others =>
            return Model.Crypto.ECDSA_Verify;
      end case;
   end Verify_Gas;

   function Copy_Gas (
      Bytes : Natural;
      Model : Cost_Model
   ) return Gas is
      Words : Natural;
   begin
      --  Round up to 32-byte words
      Words := (Bytes + 31) / 32;
      return Gas (Words) * Model.Instructions.Copy_Per_Word;
   end Copy_Gas;

   function Storage_Read_Gas (
      Slot_Count : Natural;
      Cold       : Boolean;
      Model      : Cost_Model
   ) return Gas is
      Per_Slot : Gas;
   begin
      if Cold then
         Per_Slot := Model.Storage.SLOAD_Cold;
      else
         Per_Slot := Model.Storage.SLOAD_Warm;
      end if;

      return Gas (Slot_Count) * Per_Slot;
   end Storage_Read_Gas;

   function Storage_Write_Gas (
      Slot_Count : Natural;
      Is_Set     : Boolean;
      Model      : Cost_Model
   ) return Gas is
      Per_Slot : Gas;
   begin
      if Is_Set then
         Per_Slot := Model.Storage.SSTORE_Set;
      else
         Per_Slot := Model.Storage.SSTORE_Reset;
      end if;

      return Gas (Slot_Count) * Per_Slot;
   end Storage_Write_Gas;

   ---------------------------------------------------------------------------
   --  Bound Combination Functions
   ---------------------------------------------------------------------------

   function Sequence (A, B : Execution_Bound) return Execution_Bound is
      Result : Execution_Bound;
   begin
      Result.Min_Gas := A.Min_Gas + B.Min_Gas;
      Result.Max_Gas := A.Max_Gas + B.Max_Gas;
      Result.Avg_Gas := A.Avg_Gas + B.Avg_Gas;

      --  Combined confidence is minimum of both
      Result.Confidence := Natural'Min (A.Confidence, B.Confidence);

      return Result;
   end Sequence;

   function Branch (A, B : Execution_Bound) return Execution_Bound is
      Result : Execution_Bound;
   begin
      --  Min is the minimum of both branches
      Result.Min_Gas := Gas'Min (A.Min_Gas, B.Min_Gas);

      --  Max is the maximum of both branches (worst case)
      Result.Max_Gas := Gas'Max (A.Max_Gas, B.Max_Gas);

      --  Average assumes equal probability
      Result.Avg_Gas := (A.Avg_Gas + B.Avg_Gas) / 2;

      --  Confidence is minimum
      Result.Confidence := Natural'Min (A.Confidence, B.Confidence);

      return Result;
   end Branch;

   function Scale (Bound : Execution_Bound; Factor : Natural) return Execution_Bound is
      Result : Execution_Bound;
   begin
      if Factor = 0 then
         return (
            Min_Gas    => 0,
            Max_Gas    => 0,
            Avg_Gas    => 0,
            Confidence => 100
         );
      end if;

      Result.Min_Gas := Bound.Min_Gas * Gas (Factor);
      Result.Max_Gas := Bound.Max_Gas * Gas (Factor);
      Result.Avg_Gas := Bound.Avg_Gas * Gas (Factor);
      Result.Confidence := Bound.Confidence;

      return Result;
   end Scale;

   ---------------------------------------------------------------------------
   --  Safety Margin Functions
   ---------------------------------------------------------------------------

   function With_Margin (
      Bound   : Execution_Bound;
      Percent : Natural
   ) return Execution_Bound is
      Result   : Execution_Bound;
      Margin   : Gas;
   begin
      Result := Bound;

      --  Calculate margin as percentage of max
      Margin := (Bound.Max_Gas * Gas (Percent)) / 100;

      --  Apply margin to max only (min stays same)
      Result.Max_Gas := Bound.Max_Gas + Margin;

      --  Adjust average
      Result.Avg_Gas := (Bound.Avg_Gas + Result.Max_Gas) / 2;

      return Result;
   end With_Margin;

end Khepri_WCET;
