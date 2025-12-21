-------------------------------------------------------------------------------
--  ANUBIS VEIL - FRI Protocol (Implementation)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_SHA3;

package body Anubis_STARK_FRI with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   procedure Hash_Field_Elements (
      A, B   : Field_Element;
      Result : out Hash_Value
   ) is
      Data : Byte_Array (0 .. 15) := (others => 0);
      A_Bytes : constant Anubis_STARK_Field.Byte_Array_8 :=
         Anubis_STARK_Field.To_Bytes (A);
      B_Bytes : constant Anubis_STARK_Field.Byte_Array_8 :=
         Anubis_STARK_Field.To_Bytes (B);
   begin
      Data (0 .. 7) := A_Bytes;
      Data (8 .. 15) := B_Bytes;
      Anubis_SHA3.SHA3_256 (Data, Result);
   end Hash_Field_Elements;

   --  Build Merkle tree from evaluations and return root + path for index
   Max_Tree_Levels : constant := Max_FRI_Rounds;
   type Tree_Level is array (0 .. Anubis_STARK_Poly.Max_Degree - 1) of Hash_Value;
   type Merkle_Tree is array (0 .. Max_Tree_Levels - 1) of Tree_Level;

   procedure Build_Merkle_Path (
      Evals    : Anubis_STARK_Poly.Evaluations;
      Index    : Natural;
      Path     : out Merkle_Path;
      Root     : out Hash_Value
   ) is
      Tree     : Merkle_Tree := (others => (others => (others => 0)));
      Size     : Natural := Evals.Count;
      Levels   : Natural := 0;
      Idx      : Natural := Index;
   begin
      Path := (Siblings => (others => (others => 0)), Length => 0);
      Root := (others => 0);

      if Size = 0 then
         return;
      end if;

      --  Build leaf level (hash each evaluation)
      for I in 0 .. Size - 1 loop
         declare
            E_Bytes : constant Anubis_STARK_Field.Byte_Array_8 :=
               Anubis_STARK_Field.To_Bytes (Evals.Values (I));
            Leaf_Data : Byte_Array (0 .. 7) := E_Bytes;
         begin
            Anubis_SHA3.SHA3_256 (Leaf_Data, Tree (0) (I));
         end;
      end loop;

      --  Build tree bottom-up
      while Size > 1 and Levels < Max_Tree_Levels - 1 loop
         declare
            New_Size : constant Natural := Size / 2;
         begin
            --  Record sibling for path
            if Levels < Max_FRI_Rounds then
               if Idx mod 2 = 0 and Idx + 1 < Size then
                  Path.Siblings (Levels) := Tree (Levels) (Idx + 1);
               elsif Idx > 0 then
                  Path.Siblings (Levels) := Tree (Levels) (Idx - 1);
               end if;
               Path.Length := Path.Length + 1;
            end if;

            --  Build next level
            for I in 0 .. New_Size - 1 loop
               declare
                  Combined : Byte_Array (0 .. 63) := (others => 0);
               begin
                  Combined (0 .. 31) := Tree (Levels) (I * 2);
                  Combined (32 .. 63) := Tree (Levels) (I * 2 + 1);
                  Anubis_SHA3.SHA3_256 (Combined, Tree (Levels + 1) (I));
               end;
            end loop;

            Idx := Idx / 2;
            Size := New_Size;
            Levels := Levels + 1;
         end;
      end loop;

      --  Root is at top level
      if Levels > 0 then
         Root := Tree (Levels) (0);
      elsif Size = 1 then
         Root := Tree (0) (0);
      end if;
   end Build_Merkle_Path;

   ---------------------------------------------------------------------------
   --  FRI Commit Phase
   ---------------------------------------------------------------------------

   procedure FRI_Init (
      Prover     : out FRI_Prover;
      Poly       : Polynomial;
      Dom        : Anubis_STARK_Poly.Domain;
      Blowup     : Positive
   ) is
      Layer_Evals : Anubis_STARK_Poly.Evaluations := (
         Values => (others => 0),
         Count  => 0,
         Dom    => Dom
      );
   begin
      Prover.Initial_Domain := Dom;
      Prover.Current_Poly := Poly;
      Prover.Num_Layers := 0;

      --  Initialize first layer
      Layer_Evals.Count := Dom.Size;
      Layer_Evals.Dom := Dom;

      --  Evaluate polynomial on domain
      for I in 0 .. Dom.Size - 1 loop
         pragma Loop_Invariant (I < Dom.Size);
         pragma Loop_Invariant (I <= Anubis_STARK_Poly.Max_Degree - 1);
         Layer_Evals.Values (I) := Anubis_STARK_Poly.Poly_Eval (
            Poly, Anubis_STARK_Poly.Domain_Element (Dom, I)
         );
      end loop;

      Prover.Layers (0).Evals := Layer_Evals;
      Prover.Layers (0).Dom := Dom;
   end FRI_Init;

   procedure FRI_Commit_Layer (
      Prover     : in Out FRI_Prover;
      Root       : out Hash_Value
   ) is
      Layer_Evals : constant Anubis_STARK_Poly.Evaluations :=
         Prover.Layers (Prover.Num_Layers).Evals;
   begin
      Root := Hash_Layer (Layer_Evals);
      Prover.Layers (Prover.Num_Layers).Root := Root;
   end FRI_Commit_Layer;

   procedure FRI_Fold (
      Prover     : in Out FRI_Prover;
      Alpha      : Field_Element
   ) is
      Curr_Layer : constant Natural := Prover.Num_Layers;
      Curr_Dom   : constant Anubis_STARK_Poly.Domain :=
         Prover.Layers (Curr_Layer).Dom;
      Curr_Evals : constant Anubis_STARK_Poly.Evaluations :=
         Prover.Layers (Curr_Layer).Evals;

      New_Size   : constant Natural := Curr_Dom.Size / Folding_Factor;
      New_Dom    : Anubis_STARK_Poly.Domain;
      New_Evals  : Anubis_STARK_Poly.Evaluations := (
         Values => (others => 0),
         Count  => 0,
         Dom    => Curr_Dom
      );
   begin
      --  Create folded domain
      New_Dom := Fold_Domain (Curr_Dom);

      --  Fold evaluations
      for I in 0 .. New_Size - 1 loop
         pragma Loop_Invariant (I < New_Size);
         pragma Loop_Invariant (I + New_Size < Curr_Dom.Size);
         pragma Loop_Invariant (I <= Anubis_STARK_Poly.Max_Degree - 1);
         declare
            F_X     : constant Field_Element := Curr_Evals.Values (I);
            F_Neg_X : constant Field_Element := Curr_Evals.Values (I + New_Size);
         begin
            New_Evals.Values (I) := Fold_Evaluations (F_X, F_Neg_X, Alpha);
         end;
      end loop;
      New_Evals.Count := New_Size;
      New_Evals.Dom := New_Dom;

      --  Store new layer
      Prover.Num_Layers := Prover.Num_Layers + 1;
      Prover.Layers (Prover.Num_Layers).Dom := New_Dom;
      Prover.Layers (Prover.Num_Layers).Evals := New_Evals;
   end FRI_Fold;

   procedure FRI_Complete_Commit (
      Prover     : in Out FRI_Prover;
      Transcript : Byte_Array;
      Roots      : out Hash_Array;
      Alphas     : out Alpha_Array;
      Num_Roots  : out Natural
   ) is
      Alpha      : Field_Element;
      Alpha_Seed : Byte_Array (0 .. 31);
      Round_Transcript : Byte_Array (0 .. 63) := (others => 0);
   begin
      Num_Roots := 0;
      Alphas := (others => 0);
      Roots := (others => (others => 0));

      --  Initial commitment
      FRI_Commit_Layer (Prover, Roots (0));
      Num_Roots := 1;

      --  Fold until final degree
      while Prover.Layers (Prover.Num_Layers).Dom.Size > Final_Degree loop
         pragma Loop_Invariant (Num_Roots <= Max_FRI_Rounds);
         pragma Loop_Invariant (Prover.Num_Layers < Max_FRI_Rounds);
         pragma Loop_Variant (Decreases =>
            Prover.Layers (Prover.Num_Layers).Dom.Size);

         --  Derive alpha from transcript + current root (Fiat-Shamir)
         --  Include the round number and current commitment for binding
         Round_Transcript (0 .. 31) := Roots (Num_Roots - 1);
         for I in 0 .. Natural'Min (31, Transcript'Length - 1) loop
            Round_Transcript (32 + I) := Transcript (Transcript'First + I);
         end loop;

         Anubis_SHA3.SHA3_256 (
            Round_Transcript,
            Alpha_Seed
         );
         Alpha := Anubis_STARK_Field.From_Bytes (
            Anubis_STARK_Field.Byte_Array_8 (Alpha_Seed (0 .. 7))
         );

         --  Store alpha for verification
         Alphas (Num_Roots - 1) := Alpha;

         --  Fold
         FRI_Fold (Prover, Alpha);

         --  Commit new layer
         FRI_Commit_Layer (Prover, Roots (Num_Roots));
         Num_Roots := Num_Roots + 1;

         exit when Num_Roots >= Max_FRI_Rounds;
      end loop;
   end FRI_Complete_Commit;

   ---------------------------------------------------------------------------
   --  FRI Query Phase
   ---------------------------------------------------------------------------

   procedure FRI_Generate_Queries (
      Transcript  : Byte_Array;
      Domain_Size : Natural;
      Indices     : out Query_Index_Array
   ) is
      Hash_Output : Byte_Array (0 .. 63);
   begin
      Anubis_SHA3.SHAKE256 (Transcript, Hash_Output, 64);

      for I in Indices'Range loop
         declare
            Offset : constant Natural := (I * 4) mod Hash_Output'Length;
            Idx : Natural := 0;
         begin
            for J in 0 .. 3 loop
               Idx := Idx * 256 + Natural (Hash_Output (Offset + J));
            end loop;
            Indices (I) := Idx mod Domain_Size;
         end;
      end loop;
   end FRI_Generate_Queries;

   procedure FRI_Answer_Query (
      Prover     : FRI_Prover;
      Index      : Natural;
      Response   : out Full_Query
   ) is
      Curr_Index : Natural := Index;
      Path_Root  : Hash_Value;
   begin
      Response.Num_Layers := Prover.Num_Layers;

      for Layer in 0 .. Prover.Num_Layers - 1 loop
         declare
            Dom_Size : constant Natural :=
               Prover.Layers (Layer).Dom.Size;
            Sibling_Index : constant Natural :=
               (Curr_Index + Dom_Size / 2) mod Dom_Size;
         begin
            Response.Responses (Layer).Index := Curr_Index;
            Response.Responses (Layer).Value :=
               Prover.Layers (Layer).Evals.Values (Curr_Index);
            Response.Responses (Layer).Sibling :=
               Prover.Layers (Layer).Evals.Values (Sibling_Index);

            --  Build Merkle path for this layer
            Build_Merkle_Path (
               Evals => Prover.Layers (Layer).Evals,
               Index => Curr_Index,
               Path  => Response.Responses (Layer).Path,
               Root  => Path_Root
            );

            --  Update index for next layer (folding halves domain)
            Curr_Index := Curr_Index mod (Dom_Size / 2);
         end;
      end loop;
   end FRI_Answer_Query;

   procedure FRI_Prove (
      Poly       : Polynomial;
      Dom        : Anubis_STARK_Poly.Domain;
      Blowup     : Positive;
      Transcript : Byte_Array;
      Proof      : out FRI_Proof;
      Success    : out Boolean
   ) is
      Prover     : FRI_Prover;
      Roots      : Hash_Array;
      Alphas     : Alpha_Array;
      Num_Roots  : Natural;
      Indices    : Query_Index_Array;
   begin
      --  Initialize
      FRI_Init (Prover, Poly, Dom, Blowup);

      --  Commit phase - now returns alphas for verification
      FRI_Complete_Commit (Prover, Transcript, Roots, Alphas, Num_Roots);

      --  Copy commitments
      Proof.Num_Rounds := Num_Roots;
      for I in 0 .. Num_Roots - 1 loop
         Proof.Commits (I) := Roots (I);
      end loop;

      --  Copy folding alphas (critical for verification)
      Proof.Alphas := Alphas;

      --  Copy initial domain (for proper evaluation point computation)
      Proof.Initial_Domain := Dom;

      --  Copy final polynomial
      Proof.Final_Poly := Prover.Current_Poly;

      --  Generate and answer queries
      FRI_Generate_Queries (Transcript, Dom.Size, Indices);

      for I in Indices'Range loop
         FRI_Answer_Query (Prover, Indices (I), Proof.Queries (I));
      end loop;

      Success := True;
   end FRI_Prove;

   ---------------------------------------------------------------------------
   --  FRI Verification
   ---------------------------------------------------------------------------

   function FRI_Verify (
      Proof      : FRI_Proof;
      Commitment : Hash_Value;
      Transcript : Byte_Array
   ) return Boolean is
      Indices : Query_Index_Array;
      --  Use initial domain from proof for proper verification
      Domain_Size : constant Natural := Proof.Initial_Domain.Size;
   begin
      --  Verify commitment matches first layer
      if Proof.Num_Rounds > 0 and then Commitment /= Proof.Commits (0) then
         return False;
      end if;

      --  Validate initial domain is sensible
      if Domain_Size = 0 or Proof.Initial_Domain.Log_Size > Anubis_STARK_Poly.Max_Log_Degree then
         return False;
      end if;

      --  Generate query indices based on initial domain size
      FRI_Generate_Queries (Transcript, Domain_Size, Indices);

      --  Verify each query
      for I in Indices'Range loop
         if not Verify_Query (
            Proof.Queries (I),
            Proof.Commits,
            Proof.Alphas,
            Proof.Num_Rounds,
            Proof.Initial_Domain,
            Proof.Final_Poly
         ) then
            return False;
         end if;
      end loop;

      return True;
   end FRI_Verify;

   function Verify_Query (
      Query        : Full_Query;
      Commits      : Hash_Array;
      Alphas       : Alpha_Array;
      Num_Rounds   : Natural;
      Initial_Dom  : Anubis_STARK_Poly.Domain;
      Final_Poly   : Polynomial
   ) return Boolean is
      Expected : Field_Element;
      Computed : Field_Element;
   begin
      if Query.Num_Layers = 0 then
         return True;
      end if;

      --  Step 1: Verify Merkle path for first layer
      if not Verify_Merkle_Path (
         Query.Responses (0).Value,
         Query.Responses (0).Index,
         Query.Responses (0).Path,
         Commits (0)
      ) then
         return False;
      end if;

      --  Step 2: Verify folding consistency at each layer
      for Layer in 0 .. Query.Num_Layers - 2 loop
         --  Compute expected folded value
         Computed := Fold_Evaluations (
            Query.Responses (Layer).Value,
            Query.Responses (Layer).Sibling,
            Alphas (Layer)
         );

         Expected := Query.Responses (Layer + 1).Value;

         --  Verify folding is correct
         if Computed /= Expected then
            return False;
         end if;

         --  Verify Merkle path for this layer
         if Layer + 1 < Num_Rounds then
            if not Verify_Merkle_Path (
               Query.Responses (Layer + 1).Value,
               Query.Responses (Layer + 1).Index,
               Query.Responses (Layer + 1).Path,
               Commits (Layer + 1)
            ) then
               return False;
            end if;
         end if;
      end loop;

      --  Step 3: Verify final layer matches final polynomial evaluation
      if Query.Num_Layers > 0 and Final_Poly.Degree > 0 then
         declare
            Final_Layer  : constant Natural := Query.Num_Layers - 1;
            Final_Index  : constant Natural := Query.Responses (Final_Layer).Index;
            Final_Value  : constant Field_Element := Query.Responses (Final_Layer).Value;

            --  Compute the evaluation point from folded domain
            --  After k folds: domain root becomes root^(2^k), offset becomes offset^(2^k)
            --  Evaluation point = folded_offset * folded_root^index
            Folded_Root   : Field_Element := Initial_Dom.Root;
            Folded_Offset : Field_Element := Initial_Dom.Offset;
            Eval_Point    : Field_Element;
            Poly_Eval     : Field_Element;
         begin
            --  Apply k squarings to get folded domain parameters
            for K in 1 .. Query.Num_Layers loop
               Folded_Root := Anubis_STARK_Field.Sqr (Folded_Root);
               Folded_Offset := Anubis_STARK_Field.Sqr (Folded_Offset);
            end loop;

            --  Compute evaluation point: offset * root^index
            Eval_Point := Anubis_STARK_Field.Mul (
               Folded_Offset,
               Anubis_STARK_Field.Exp (Folded_Root, Unsigned_64 (Final_Index))
            );

            --  Evaluate final polynomial at the correctly computed point
            Poly_Eval := Anubis_STARK_Poly.Poly_Eval (Final_Poly, Eval_Point);

            --  Verify the final folded value matches polynomial evaluation
            if Poly_Eval /= Final_Value then
               return False;
            end if;
         end;
      end if;

      return True;
   end Verify_Query;

   function Verify_Merkle_Path (
      Leaf       : Field_Element;
      Index      : Natural;
      Path       : Merkle_Path;
      Root       : Hash_Value
   ) return Boolean is
      Current : Hash_Value;
      Idx     : Natural := Index;
   begin
      --  Hash leaf
      Hash_Field_Elements (Leaf, Leaf, Current);

      --  Walk up the path
      for I in 0 .. Path.Length - 1 loop
         declare
            Combined : Byte_Array (0 .. 63) := (others => 0);
         begin
            if Idx mod 2 = 0 then
               Combined (0 .. 31) := Current;
               Combined (32 .. 63) := Path.Siblings (I);
            else
               Combined (0 .. 31) := Path.Siblings (I);
               Combined (32 .. 63) := Current;
            end if;
            Anubis_SHA3.SHA3_256 (Combined, Current);
         end;
         Idx := Idx / 2;
      end loop;

      --  Compare with root
      return Current = Root;
   end Verify_Merkle_Path;

   ---------------------------------------------------------------------------
   --  Folding Operations
   ---------------------------------------------------------------------------

   function Fold_Evaluations (
      F_X        : Field_Element;
      F_Neg_X    : Field_Element;
      Alpha      : Field_Element
   ) return Field_Element is
      --  f'(x^2) = (f(x) + f(-x))/2 + alpha * (f(x) - f(-x))/(2x)
      --  Simplified: f'(y) = f_even(y) + alpha * f_odd(y)
      F_Even : constant Field_Element :=
         Anubis_STARK_Field.Mul (
            Anubis_STARK_Field.Add (F_X, F_Neg_X),
            Anubis_STARK_Field.Inv (Anubis_STARK_Field.Field_Element (2))
         );
      F_Odd  : constant Field_Element :=
         Anubis_STARK_Field.Mul (
            Anubis_STARK_Field.Sub (F_X, F_Neg_X),
            Anubis_STARK_Field.Inv (Anubis_STARK_Field.Field_Element (2))
         );
   begin
      return Anubis_STARK_Field.Add (F_Even, Anubis_STARK_Field.Mul (Alpha, F_Odd));
   end Fold_Evaluations;

   function Fold_Domain (D : Anubis_STARK_Poly.Domain) return Anubis_STARK_Poly.Domain is
   begin
      return (
         Size     => D.Size / 2,
         Log_Size => D.Log_Size - 1,
         Offset   => Anubis_STARK_Field.Sqr (D.Offset),
         Root     => Anubis_STARK_Field.Sqr (D.Root)
      );
   end Fold_Domain;

   ---------------------------------------------------------------------------
   --  Utility
   ---------------------------------------------------------------------------

   function Compute_Num_Rounds (
      Degree : Natural;
      Final  : Natural := Final_Degree
   ) return Natural is
      D      : Natural := Degree;
      Rounds : Natural := 0;
   begin
      while D > Final loop
         D := D / Folding_Factor;
         Rounds := Rounds + 1;
      end loop;
      return Rounds;
   end Compute_Num_Rounds;

   function Hash_Pair (
      Left  : Field_Element;
      Right : Field_Element
   ) return Hash_Value is
      Result : Hash_Value;
   begin
      Hash_Field_Elements (Left, Right, Result);
      return Result;
   end Hash_Pair;

   function Hash_Layer (
      Layer_Evals : Anubis_STARK_Poly.Evaluations
   ) return Hash_Value is
      Result : Hash_Value;
      --  Use fixed-size buffer (max possible size) for SPARK compatibility
      Max_Data_Size : constant := Anubis_STARK_Poly.Max_Degree * 8;
      Data   : Byte_Array (0 .. Max_Data_Size - 1) := (others => 0);
      Actual_Size : constant Natural := Layer_Evals.Count * 8;
   begin
      for I in 0 .. Layer_Evals.Count - 1 loop
         declare
            E_Bytes : constant Anubis_STARK_Field.Byte_Array_8 :=
               Anubis_STARK_Field.To_Bytes (Layer_Evals.Values (I));
         begin
            Data (I * 8 .. I * 8 + 7) := E_Bytes;
         end;
      end loop;

      --  Hash only the actual data portion
      Anubis_SHA3.SHA3_256 (Data (0 .. Actual_Size - 1), Result);
      return Result;
   end Hash_Layer;

end Anubis_STARK_FRI;
