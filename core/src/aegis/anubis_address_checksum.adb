pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_SHA3; use Anubis_SHA3;
with Anubis_Types; use Anubis_Types;

package body Anubis_Address_Checksum with
   SPARK_Mode => On
is

   --  Build preimage string for checksum per AAS-001 v3.1
   --  Format: "mldsa87:network:type:payload" (no dashes in payload)
   procedure Build_Preimage (
      Network   : in  Network_Type;
      Entity    : in  Entity_Type;
      Payload   : in  Payload_Base32;
      Preimage  : out String;
      Length    : out Natural
   ) is
      Algo_Tag : constant String := Algorithm_Tag;  --  "mldsa87"
      Net_Str  : constant String := Network_String (Network);
      Ent_Char : constant Character := Entity_Char (Entity);
      Pos      : Natural := Preimage'First;
   begin
      --  Initialize with spaces
      Preimage := (others => ' ');

      --  Copy algorithm tag "mldsa87"
      for I in Algo_Tag'Range loop
         pragma Loop_Invariant (Pos >= Preimage'First);
         pragma Loop_Invariant (Pos < Preimage'Last);
         Preimage (Pos) := Algo_Tag (I);
         Pos := Pos + 1;
      end loop;

      --  Add colon
      Preimage (Pos) := ':';
      Pos := Pos + 1;

      --  Copy network
      for I in Net_Str'Range loop
         pragma Loop_Invariant (Pos >= Preimage'First);
         pragma Loop_Invariant (Pos < Preimage'Last);
         Preimage (Pos) := Net_Str (I);
         Pos := Pos + 1;
      end loop;

      --  Add colon
      Preimage (Pos) := ':';
      Pos := Pos + 1;

      --  Add entity type character
      Preimage (Pos) := Ent_Char;
      Pos := Pos + 1;

      --  Add colon
      Preimage (Pos) := ':';
      Pos := Pos + 1;

      --  Copy payload (52 characters, NO dashes)
      for I in Payload_Index loop
         pragma Loop_Invariant (Pos >= Preimage'First);
         pragma Loop_Invariant (Pos < Preimage'Last);
         Preimage (Pos) := Payload (I);
         Pos := Pos + 1;
      end loop;

      Length := Pos - Preimage'First;
   end Build_Preimage;

   --  Compute checksum using SHA3-256 per AAS-001 v3.1
   procedure Compute_Checksum (
      Network   : in  Network_Type;
      Entity    : in  Entity_Type;
      Payload   : in  Payload_Base32;
      Checksum  : out Checksum_Bytes
   ) is
      --  Preimage buffer (max 70 chars for staging network)
      Preimage_Str    : String (1 .. Max_Preimage_Length);
      Preimage_Length : Natural;

      --  Convert preimage to byte array for hashing
      Preimage_Bytes  : Byte_Array (0 .. Max_Preimage_Length - 1);
      Digest          : SHA3_256_Digest;
   begin
      --  Build preimage: "mldsa87:network:type:payload"
      Build_Preimage (Network, Entity, Payload, Preimage_Str, Preimage_Length);

      --  Convert to byte array
      for I in 0 .. Preimage_Length - 1 loop
         pragma Loop_Invariant (I < Max_Preimage_Length);
         Preimage_Bytes (I) := Character'Pos (Preimage_Str (I + 1));
      end loop;

      --  Hash preimage: SHA3-256(preimage)
      SHA3_256 (Preimage_Bytes (0 .. Preimage_Length - 1), Digest);

      --  Extract first 24 bits (3 bytes) per AAS-001 v3.1
      Checksum (0) := Digest (0);
      Checksum (1) := Digest (1);
      Checksum (2) := Digest (2);
   end Compute_Checksum;

   --  Verify checksum (constant-time comparison)
   function Verify_Checksum (
      Network           : Network_Type;
      Entity            : Entity_Type;
      Payload           : Payload_Base32;
      Expected_Checksum : Checksum_Bytes
   ) return Boolean is
      Computed : Checksum_Bytes;
      Diff     : Unsigned_8 := 0;
   begin
      Compute_Checksum (Network, Entity, Payload, Computed);

      --  Constant-time comparison to prevent timing attacks
      for I in Checksum_Index loop
         Diff := Diff or (Computed (I) xor Expected_Checksum (I));
      end loop;

      return Diff = 0;
   end Verify_Checksum;

end Anubis_Address_Checksum;
