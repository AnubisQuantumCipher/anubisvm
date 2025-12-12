pragma SPARK_Mode (On);

package body Anubis_Bytes with
   SPARK_Mode => On
is

   --  CT_Equal: Accumulate differences, then check if any occurred
   --  Timing is independent of where/how many differences exist
   function CT_Equal (A, B : Byte_Array) return Boolean is
      Diff : Byte := 0;
   begin
      for I in A'Range loop
         pragma Loop_Invariant (I >= A'First);
         pragma Loop_Invariant (I <= A'Last);
         pragma Loop_Invariant ((Diff = 0) =
            (for all J in A'First .. I - 1 => A (J) = B (B'First + (J - A'First))));

         Diff := Diff or (A (I) xor B (B'First + (I - A'First)));
      end loop;

      --  Diff is 0 iff all bytes matched
      return Diff = 0;
   end CT_Equal;

   --  CT_Less_Than: Lexicographic comparison without early exit
   --  Processes all bytes, masks out results after first difference
   function CT_Less_Than (A, B : Byte_Array) return Boolean is
      Result : Word := 0;      -- 1 if A < B found, 2 if A > B found
      Done : Word := 0;        -- 1 if difference found
   begin
      for I in A'Range loop
         pragma Loop_Invariant (I >= A'First);
         pragma Loop_Invariant (I <= A'Last);
         pragma Loop_Invariant (Result in 0 | 1 | 2);
         pragma Loop_Invariant (Done in 0 | 1);

         declare
            A_Byte : constant Word := Word (A (I));
            B_Byte : constant Word := Word (B (B'First + (I - A'First)));

            Is_Less : constant Word := CT_Lt (A_Byte, B_Byte);
            Is_Greater : constant Word := CT_Gt (A_Byte, B_Byte);
            Is_Diff : constant Word := Is_Less or Is_Greater;

            Not_Done_Mask : constant Word := 0 - (1 - Done);

            --  Compute new result value for this iteration
            New_Result_Val : constant Word := Is_Less or (Is_Greater * 2);
         begin
            --  Assert: A byte is either less, greater, or equal - not both less AND greater
            pragma Assert ((Is_Less and Is_Greater) = 0);
            pragma Assert (New_Result_Val in 0 | 1 | 2);

            --  Update result only if not yet found a difference (using constant-time select)
            --  If Done = 0 (Not_Done_Mask = all 1s): Result := New_Result_Val
            --  If Done = 1 (Not_Done_Mask = 0): Result := Result (unchanged)
            Result := (New_Result_Val and Not_Done_Mask) or (Result and not Not_Done_Mask);

            --  Mark as done if we found a difference
            Done := Done or Is_Diff;
         end;
      end loop;

      --  Result = 1 means A < B, Result = 2 means A > B, Result = 0 means A = B
      return Result = 1;
   end CT_Less_Than;

   --  CT_Copy: Conditional copy using masking
   procedure CT_Copy (Cond : Word; Dest : in out Byte_Array; Src : Byte_Array) is
      --  Mask as Word first, then extract low byte
      Mask_Word : constant Word := 0 - Cond;  -- 0xFFFF...FFFF if Cond=1, 0x0 if Cond=0
      Mask : constant Byte := Byte (Mask_Word and 16#FF#);
   begin
      for I in Dest'Range loop
         pragma Loop_Invariant (I >= Dest'First);
         pragma Loop_Invariant (I <= Dest'Last);
         pragma Loop_Invariant (if Cond = 1 then
            (for all J in Dest'First .. I - 1 =>
               Dest (J) = Src (Src'First + (J - Dest'First)))
         else
            (for all J in Dest'First .. I - 1 =>
               Dest (J) = Dest'Loop_Entry (J)));

         Dest (I) := (Src (Src'First + (I - Dest'First)) and Mask) or
                     (Dest (I) and not Mask);
      end loop;
   end CT_Copy;

   --  Secure_Zero: Write zeros with volatile semantics
   --  The Volatile aspect (when added) prevents dead-store elimination
   procedure Secure_Zero (Data : out Byte_Array) is
   begin
      --  Initialize all elements to avoid uninitialized read in loop invariant
      Data := (others => 0);

      --  Additional explicit loop for proof purposes (compiler will optimize)
      for I in Data'Range loop
         pragma Loop_Invariant (I >= Data'First);
         pragma Loop_Invariant (I <= Data'Last);
         pragma Loop_Invariant (for all J in Data'Range => Data (J) = 0);

         Data (I) := 0;
      end loop;
   end Secure_Zero;

   --  CT_Is_Zero: Check if all bytes are zero without early exit
   function CT_Is_Zero (Data : Byte_Array) return Boolean is
      Acc : Byte := 0;
   begin
      for I in Data'Range loop
         pragma Loop_Invariant (I >= Data'First);
         pragma Loop_Invariant (I <= Data'Last);

         Acc := Acc or Data (I);
      end loop;

      return Acc = 0;
   end CT_Is_Zero;

   --  XOR_Arrays: Element-wise XOR
   procedure XOR_Arrays (Result : in out Byte_Array; A, B : Byte_Array) is
   begin
      for I in Result'Range loop
         pragma Loop_Invariant (I >= Result'First);
         pragma Loop_Invariant (I <= Result'Last);
         pragma Loop_Invariant (for all J in Result'First .. I - 1 =>
            Result (J) = (A (A'First + (J - Result'First)) xor
                          B (B'First + (J - Result'First))));

         Result (I) := A (A'First + (I - Result'First)) xor
                       B (B'First + (I - Result'First));
      end loop;
   end XOR_Arrays;

   --  AND_Arrays: Element-wise AND
   procedure AND_Arrays (Result : in out Byte_Array; A, B : Byte_Array) is
   begin
      for I in Result'Range loop
         pragma Loop_Invariant (I >= Result'First);
         pragma Loop_Invariant (I <= Result'Last);
         pragma Loop_Invariant (for all J in Result'First .. I - 1 =>
            Result (J) = (A (A'First + (J - Result'First)) and
                          B (B'First + (J - Result'First))));

         Result (I) := A (A'First + (I - Result'First)) and
                       B (B'First + (I - Result'First));
      end loop;
   end AND_Arrays;

   --  Encode_LE64: encode a 64-bit value as 8 little-endian bytes
   procedure Encode_LE64 (Value : Unsigned_64; Output : out Byte_Array) is
      V : Unsigned_64 := Value;
   begin
      --  Output'Length is guaranteed to be 8 by the precondition in the spec.
      for I in 0 .. 7 loop
         pragma Loop_Invariant (I in 0 .. 7);
         Output (Output'First + I) := Byte (V and 16#FF#);
         V := Shift_Right (V, 8);
      end loop;
   end Encode_LE64;

end Anubis_Bytes;
