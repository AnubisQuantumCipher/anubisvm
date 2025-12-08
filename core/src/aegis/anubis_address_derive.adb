pragma SPARK_Mode (On);

with Anubis_SHA3; use Anubis_SHA3;
with Anubis_Types; use Anubis_Types;

package body Anubis_Address_Derive with
   SPARK_Mode => On
is

   --  Build domain separator string
   --  Format: "aegis-v1-mldsa87-" || type
   procedure Build_Domain_Separator (
      Entity    : in  Entity_Type;
      Separator : out Domain_Separator_String;
      Length    : out Natural
   ) is
      Type_Str : constant String := Entity_String (Entity);
      Pos      : Natural := 1;
   begin
      --  Initialize with spaces (for unused portion)
      Separator := (others => ' ');

      --  Copy "aegis-v1-mldsa87-" (17 characters)
      --  Domain_Prefix'Range is 1..17, Separator range is 1..18
      for I in Domain_Prefix'Range loop
         --  Pos equals I at start of each iteration
         pragma Loop_Invariant (Pos = I);
         Separator (Pos) := Domain_Prefix (I);
         Pos := Pos + 1;
      end loop;

      --  At this point Pos = Domain_Prefix'Length + 1 = 18
      --  Copy entity type string (single char: u/c/v/s)
      --  Entity_String returns exactly 1 character, so only 1 iteration
      if Type_Str'Length >= 1 and Pos <= Max_Domain_Sep_Length then
         Separator (Pos) := Type_Str (Type_Str'First);
         Pos := Pos + 1;
      end if;

      Length := Pos - 1;
   end Build_Domain_Separator;

   --  Derive account ID from ML-DSA-87 public key using SHA3-256
   procedure Derive_Account_ID (
      Entity     : in  Entity_Type;
      Public_Key : in  Public_Key_Bytes;
      Account    : out Account_ID
   ) is
      Domain_Sep    : Domain_Separator_String;
      Domain_Length : Natural;

      --  Combined message: domain_separator || public_key
      --  Max size: 18 + 2592 = 2610 bytes
      Max_Message_Size : constant := Max_Domain_Sep_Length + MLDSA87_Public_Key_Size;

      Message        : Byte_Array (0 .. Max_Message_Size - 1) := (others => 0);
      Message_Length : Natural;
      Digest         : SHA3_256_Digest;
   begin
      --  Build domain separator
      Build_Domain_Separator (Entity, Domain_Sep, Domain_Length);

      --  Copy domain separator to message
      for I in 0 .. Domain_Length - 1 loop
         pragma Loop_Invariant (I < Max_Domain_Sep_Length);
         Message (I) := Character'Pos (Domain_Sep (I + 1));
      end loop;

      --  Append public key to message
      for I in Public_Key'Range loop
         pragma Loop_Invariant (Domain_Length + I < Max_Message_Size);
         Message (Domain_Length + I) := Public_Key (I);
      end loop;

      Message_Length := Domain_Length + Public_Key'Length;

      --  Hash: SHA3-256(domain_separator || public_key)
      SHA3_256 (Message (0 .. Message_Length - 1), Digest);

      --  Copy digest to account ID
      for I in Account_ID_Index loop
         pragma Loop_Invariant (I in Account_ID_Index);
         Account (I) := Digest (I);
      end loop;
   end Derive_Account_ID;

end Anubis_Address_Derive;
