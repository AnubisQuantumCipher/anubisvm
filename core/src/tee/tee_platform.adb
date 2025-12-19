pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body TEE_Platform with
   SPARK_Mode => On
is

   --  Verify timestamp is within acceptable range
   function Verify_Timestamp (
      Current_Block_Time : Unix_Timestamp;
      TS                 : Timestamped_Value
   ) return Boolean is
      One_Hour : constant Word64 := 3600;  --  1 hour in seconds
      Max_Future : Word64;
   begin
      --  Must be valid
      if not TS.Valid then
         return False;
      end if;

      --  Test timestamps can be zero
      if TS.Source = Test_Time then
         return True;
      end if;

      --  Real timestamps must be non-zero
      if TS.Value = 0 then
         return False;
      end if;

      --  Check not too far in the future (avoid overflow in addition)
      if Current_Block_Time <= Word64'Last - One_Hour then
         Max_Future := Current_Block_Time + One_Hour;
         if TS.Value > Max_Future then
            return False;
         end if;
      end if;

      --  Prefer trusted sources
      case TS.Source is
         when Block_Time | Hardware_Clock =>
            return True;
         when System_Time =>
            --  System time is acceptable but less trusted
            return True;
         when Test_Time =>
            --  Already handled above
            return True;
      end case;
   end Verify_Timestamp;

   --  Verify entropy meets minimum quality requirements
   function Verify_Entropy (
      E        : Entropy_Sample;
      Min_Size : Positive;
      Min_Qual : Entropy_Quality
   ) return Boolean is
      Has_Non_Zero : Boolean := False;
   begin
      --  Must be valid
      if not E.Valid then
         return False;
      end if;

      --  Must have sufficient size
      if E.Size < Min_Size then
         return False;
      end if;

      --  Check quality level (ordered from highest to lowest quality)
      case Min_Qual is
         when Hardware_RNG =>
            --  Require hardware RNG
            if E.Quality /= Hardware_RNG then
               return False;
            end if;
         when System_RNG =>
            --  Accept hardware or system RNG
            if E.Quality /= Hardware_RNG and E.Quality /= System_RNG then
               return False;
            end if;
         when PRNG =>
            --  Accept any quality except test fixed
            if E.Quality = Test_Fixed then
               return False;
            end if;
         when Test_Fixed =>
            --  Accept any quality (testing mode)
            null;
      end case;

      --  Sanity check: entropy should not be all zeros (unless test mode)
      if E.Quality /= Test_Fixed then
         for I in 0 .. E.Size - 1 loop
            pragma Loop_Invariant (I >= 0 and I < E.Size);
            pragma Loop_Invariant (not Has_Non_Zero or Has_Non_Zero);
            if E.Data (I) /= 0 then
               Has_Non_Zero := True;
               exit;
            end if;
         end loop;

         if not Has_Non_Zero then
            return False;
         end if;
      end if;

      return True;
   end Verify_Entropy;

   --  Verify hardware measurement is valid
   function Verify_Measurement (
      M : Hardware_Measurement
   ) return Boolean is
      All_Zeros : Boolean := True;
   begin
      --  Must be valid
      if not M.Valid then
         return False;
      end if;

      --  Check if hash is all zeros
      for I in M.Hash'Range loop
         pragma Loop_Invariant (All_Zeros or not All_Zeros);
         if M.Hash (I) /= 0 then
            All_Zeros := False;
            exit;
         end if;
      end loop;

      --  Hash should not be all zeros (unless test fixture)
      if All_Zeros and M.Source /= Test_Fixture then
         return False;
      end if;

      --  Prefer trusted sources
      case M.Source is
         when Hardware_PCR | Enclave_Report =>
            --  Most trusted
            return True;
         when Software_Hash =>
            --  Acceptable but less trusted
            return True;
         when Test_Fixture =>
            --  Test mode
            return True;
      end case;
   end Verify_Measurement;

   --  Verify platform capabilities meet minimum requirements
   function Verify_Capabilities (
      Caps            : Platform_Capabilities;
      Require_HW_RNG  : Boolean;
      Require_Enclave : Boolean
   ) return Boolean is
   begin
      --  Check hardware RNG requirement
      if Require_HW_RNG and not Caps.Has_Hardware_RNG then
         return False;
      end if;

      --  Check enclave requirement
      if Require_Enclave and not Caps.Has_Enclave_Support then
         return False;
      end if;

      return True;
   end Verify_Capabilities;

   --  Compare two timestamps (constant-time where it matters)
   function Equal_Timestamps (
      A : Timestamped_Value;
      B : Timestamped_Value
   ) return Boolean is
      Diff : Word64 := 0;
   begin
      --  XOR the values (constant-time)
      Diff := A.Value xor B.Value;

      --  Check other fields (these are not secret, so timing is okay)
      if A.Source /= B.Source then
         return False;
      end if;

      if A.Valid /= B.Valid then
         return False;
      end if;

      --  Return true if diff is zero (values match)
      return Diff = 0;
   end Equal_Timestamps;

   --  Compare two measurements (constant-time)
   function Equal_Measurements (
      A : Hardware_Measurement;
      B : Hardware_Measurement
   ) return Boolean is
      Diff : Byte := 0;
   begin
      --  XOR all hash bytes (constant-time)
      for I in A.Hash'Range loop
         pragma Loop_Invariant (I in A.Hash'Range);
         Diff := Diff or (A.Hash (I) xor B.Hash (I));
      end loop;

      --  Check other fields (not secret)
      if A.Source /= B.Source then
         return False;
      end if;

      if A.Valid /= B.Valid then
         return False;
      end if;

      --  Return true if no differences found
      return Diff = 0;
   end Equal_Measurements;

   --  Convert Word64 timestamp to byte array (little-endian)
   procedure Timestamp_To_Bytes (
      TS    : Unix_Timestamp;
      Bytes : out Byte_Array
   ) is
      Temp : Word64 := TS;
   begin
      for I in 0 .. 7 loop
         pragma Loop_Invariant (I in 0 .. 7);
         pragma Loop_Invariant (Bytes'First + I <= Bytes'Last);
         Bytes (Bytes'First + I) := Byte (Temp and 16#FF#);
         Temp := Shift_Right (Temp, 8);
      end loop;
   end Timestamp_To_Bytes;

   --  Convert byte array to Word64 timestamp (little-endian)
   function Bytes_To_Timestamp (
      Bytes : Byte_Array
   ) return Unix_Timestamp is
      Result : Word64 := 0;
      Shift_Amount : Natural := 0;
   begin
      for I in 0 .. 7 loop
         pragma Loop_Invariant (I in 0 .. 7);
         pragma Loop_Invariant (Bytes'First + I <= Bytes'Last);
         pragma Loop_Invariant (Shift_Amount = I * 8);
         pragma Loop_Invariant (Shift_Amount <= 56);

         Result := Result or Shift_Left (Word64 (Bytes (Bytes'First + I)), Shift_Amount);
         Shift_Amount := Shift_Amount + 8;
      end loop;

      return Result;
   end Bytes_To_Timestamp;

   --  Check if entropy buffer is all zeros
   function Is_Zero_Entropy (
      E : Entropy_Buffer
   ) return Boolean is
   begin
      for I in E'Range loop
         pragma Loop_Invariant (I in E'Range);
         if E (I) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Zero_Entropy;

   --  Check if measurement is all zeros
   function Is_Zero_Measurement (
      M : Platform_Measurement
   ) return Boolean is
   begin
      for I in M'Range loop
         pragma Loop_Invariant (I in M'Range);
         if M (I) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Zero_Measurement;

end TEE_Platform;
