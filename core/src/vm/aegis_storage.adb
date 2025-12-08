pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types;
with Anubis_SHA3;

package body Aegis_Storage with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Key_To_Nibbles (Key : Hash256) return Nibble_Path is
      Result : Nibble_Path := (others => 0);
   begin
      for I in 0 .. 31 loop
         --  High nibble
         Result (Nibble_Index (I * 2)) := Nibble (Key (I) / 16);
         --  Low nibble
         Result (Nibble_Index (I * 2 + 1)) := Nibble (Key (I) mod 16);
      end loop;
      return Result;
   end Key_To_Nibbles;

   function Is_Empty_Account (Acc : Account_State) return Boolean is
   begin
      return Acc.Balance = U256_Zero and Acc.Nonce = 0 and not Acc.Is_Contract;
   end Is_Empty_Account;

   function Compute_Slot_Key (
      Address : Contract_Address;
      Slot    : Storage_Key
   ) return Hash256 is
      --  Compute Keccak256(Address || Slot) for storage slot key derivation
      --  This matches Ethereum"s storage key computation
      Slot_U256 : constant U256 := U256 (Slot);

      --  Preimage: 32-byte address || 32-byte slot = 64 bytes
      Preimage : Anubis_Types.Byte_Array (0 .. 63) := (others => 0);
      Digest   : Anubis_SHA3.SHA3_256_Digest;
      Result   : Hash256;
   begin
      --  Copy address to first 32 bytes
      for I in 0 .. 31 loop
         Preimage (I) := Anubis_Types.Byte (Address (I));
      end loop;

      --  Copy slot to last 32 bytes (big-endian)
      for I in 0 .. 3 loop
         declare
            Limb : constant Unsigned_64 := Slot_U256.Limbs (3 - I);
         begin
            for J in 0 .. 7 loop
               Preimage (32 + I * 8 + J) := Anubis_Types.Byte (
                  Shift_Right (Limb, (7 - J) * 8) and 16#FF#);
            end loop;
         end;
      end loop;

      --  Compute Keccak-256 (Ethereum-compatible, uses 0x01 domain separator)
      Anubis_SHA3.Keccak_256 (Preimage, Digest);

      --  Copy digest to result
      for I in 0 .. 31 loop
         Result (I) := Byte (Digest (I));
      end loop;

      return Result;
   end Compute_Slot_Key;

end Aegis_Storage;
