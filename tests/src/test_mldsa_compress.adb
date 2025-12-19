-------------------------------------------------------------------------------
--  Test ML-DSA-87 Signature Compression
--
--  Validates:
--  1. Round-trip compression/decompression (lossless)
--  2. Compression ratio (~46% target)
--  3. CRC integrity checking
--  4. Z-vector variable-length encoding
--  5. Hint RLE encoding
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;
with Anubis_MLDSA_Compress; use Anubis_MLDSA_Compress;

procedure Test_MLDSA_Compress is

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Test (Name : String; Condition : Boolean) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line ("[PASS] " & Name);
      else
         Put_Line ("[FAIL] " & Name);
      end if;
   end Test;

   ---------------------------------------------------------------------------
   --  Test 1: CRC-16 Computation
   ---------------------------------------------------------------------------

   procedure Test_CRC16 is
      Data : constant Byte_Array := (16#01#, 16#02#, 16#03#, 16#04#);
      CRC1, CRC2 : Unsigned_16;
   begin
      Put_Line ("=== Test CRC-16 ===");

      CRC1 := Compute_CRC16 (Data);
      Test ("CRC16 computes non-zero", CRC1 /= 0);

      CRC2 := Compute_CRC16 (Data);
      Test ("CRC16 is deterministic", CRC1 = CRC2);

      declare
         Modified : constant Byte_Array := (16#01#, 16#02#, 16#03#, 16#05#);
         CRC3 : constant Unsigned_16 := Compute_CRC16 (Modified);
      begin
         Test ("CRC16 detects changes", CRC1 /= CRC3);
      end;

      Test ("CRC16 verification works", Verify_CRC16 (Data, CRC1));
      Test ("CRC16 rejects wrong checksum", not Verify_CRC16 (Data, CRC1 xor 16#FFFF#));

      New_Line;
   end Test_CRC16;

   ---------------------------------------------------------------------------
   --  Test 2: Header Operations
   ---------------------------------------------------------------------------

   procedure Test_Header is
      Buffer : Byte_Array (0 .. Header_Size - 1);
      Version : Natural;
      Flags : Byte;
      Data_Size : Natural;
      Checksum : Unsigned_16;
      Valid : Boolean;
   begin
      Put_Line ("=== Test Header Operations ===");

      -- Write header
      Write_Header (Buffer, 16#AB#, 1234, 16#5678#);

      Test ("Header has magic byte 1", Buffer (0) = Magic_Byte_1);
      Test ("Header has magic byte 2", Buffer (1) = Magic_Byte_2);
      Test ("Header has version", Buffer (2) = Byte (Format_Version));

      -- Read header
      Read_Header (Buffer, Version, Flags, Data_Size, Checksum, Valid);

      Test ("Header is valid", Valid);
      Test ("Version matches", Version = Format_Version);
      Test ("Flags match", Flags = 16#AB#);
      Test ("Data size matches", Data_Size = 1234);
      Test ("Checksum matches", Checksum = 16#5678#);

      -- Test invalid header
      declare
         Bad_Buffer : Byte_Array (0 .. Header_Size - 1) := (others => 0);
      begin
         Read_Header (Bad_Buffer, Version, Flags, Data_Size, Checksum, Valid);
         Test ("Invalid header detected", not Valid);
      end;

      New_Line;
   end Test_Header;

   ---------------------------------------------------------------------------
   --  Test 3: Z Vector Analysis and Encoding
   ---------------------------------------------------------------------------

   procedure Test_Z_Vector is
      Z : Z_Vector;
      All_Small : Boolean;
      Small_Count : Natural;
      Max_Mag : Natural;

      Encode_Buffer : Byte_Array (0 .. 10000);
      Encode_Len : Natural;
      Encode_Small_Count : Natural;
      Encode_Success : Boolean;

      Z_Decoded : Z_Vector;
      Decode_Bytes : Natural;
      Decode_Success : Boolean;
   begin
      Put_Line ("=== Test Z Vector Encoding ===");

      -- Create test Z vector with mostly small values
      for I in Z'Range loop
         for J in Z (I)'Range loop
            if (I + J) mod 10 = 0 then
               Z (I)(J) := 10000;  -- Large value
            else
               Z (I)(J) := Integer ((I + J) mod 1000);  -- Small value
            end if;
         end loop;
      end loop;

      -- Analyze
      Analyze_Z_Vector (Z, All_Small, Small_Count, Max_Mag);

      Test ("Z analysis finds large values", not All_Small);
      Test ("Z max magnitude correct", Max_Mag = 10000);
      Put_Line ("   Small coefficients: " & Natural'Image (Small_Count) & " / " & Natural'Image (L * N));

      -- Encode
      Encode_Z_Vector (Z, Encode_Buffer, Encode_Len, Encode_Small_Count, Encode_Success);

      Test ("Z encoding succeeds", Encode_Success);
      Test ("Z encoding produces output", Encode_Len > 0);
      Test ("Z encoding is efficient", Encode_Len < L * N * 3);
      Put_Line ("   Encoded size: " & Natural'Image (Encode_Len) & " bytes");

      -- Decode and verify round-trip
      Decode_Z_Vector (Encode_Buffer (0 .. Encode_Len - 1), Z_Decoded, Decode_Bytes, Decode_Success);

      Test ("Z decoding succeeds", Decode_Success);
      Test ("Z decode reads correct bytes", Decode_Bytes = Encode_Len);

      -- Verify all coefficients match
      declare
         All_Match : Boolean := True;
      begin
         for I in Z'Range loop
            for J in Z (I)'Range loop
               if Z (I)(J) /= Z_Decoded (I)(J) then
                  All_Match := False;
                  exit;
               end if;
            end loop;
            exit when not All_Match;
         end loop;

         Test ("Z round-trip preserves data", All_Match);
      end;

      New_Line;
   end Test_Z_Vector;

   ---------------------------------------------------------------------------
   --  Test 4: Hint Vector RLE Encoding
   ---------------------------------------------------------------------------

   procedure Test_Hint_Vector is
      Hints : Hint_Vector;
      Set_Count : Natural := 0;

      Encode_Buffer : Byte_Array (0 .. 1000);
      Encode_Len : Natural;
      Encode_Bits : Natural;
      Encode_Success : Boolean;

      Hints_Decoded : Hint_Vector;
      Decode_Bytes : Natural;
      Decode_Success : Boolean;
   begin
      Put_Line ("=== Test Hint Vector RLE ===");

      -- Initialize all hints to False
      for I in Hints'Range loop
         for J in Hints (I)'Range loop
            Hints (I)(J) := False;
         end loop;
      end loop;

      -- Set sparse hints (typical: ~75 out of 2048)
      for I in 0 .. 9 loop
         Hints (I mod K)(I * 17 mod N) := True;
         Set_Count := Set_Count + 1;
      end loop;

      declare
         Counted : constant Natural := Count_Hint_Bits (Hints);
      begin
         Test ("Hint count correct", Counted = Set_Count);
      end;

      -- Encode
      Encode_Hints_RLE (Hints, Encode_Buffer, Encode_Len, Encode_Bits, Encode_Success);

      Test ("Hint encoding succeeds", Encode_Success);
      Test ("Hint count matches", Encode_Bits = Set_Count);
      Test ("Hint encoding is compact", Encode_Len < 100);
      Put_Line ("   Encoded size: " & Natural'Image (Encode_Len) & " bytes for " &
                Natural'Image (Encode_Bits) & " set bits");

      -- Decode
      Decode_Hints_RLE (Encode_Buffer (0 .. Encode_Len - 1), Hints_Decoded, Decode_Bytes, Decode_Success);

      Test ("Hint decoding succeeds", Decode_Success);

      -- Verify all hints match
      declare
         All_Match : Boolean := True;
      begin
         for I in Hints'Range loop
            for J in Hints (I)'Range loop
               if Hints (I)(J) /= Hints_Decoded (I)(J) then
                  All_Match := False;
                  exit;
               end if;
            end loop;
            exit when not All_Match;
         end loop;

         Test ("Hint round-trip preserves data", All_Match);
      end;

      New_Line;
   end Test_Hint_Vector;

   ---------------------------------------------------------------------------
   --  Test 5: Signature Parsing/Serialization
   ---------------------------------------------------------------------------

   procedure Test_Signature_Parse is
      -- Create a fake but structurally valid signature
      Signature : Byte_Array (0 .. MLDSA87_Sig_Size - 1) := (others => 0);

      C_Tilde : Byte_Array (0 .. MLDSA87_C_Tilde_Size - 1);
      Z : Z_Vector;
      Hints : Hint_Vector;
      Parse_Success : Boolean;

      Signature_Recon : Byte_Array (0 .. MLDSA87_Sig_Size - 1);
      Serialize_Success : Boolean;
   begin
      Put_Line ("=== Test Signature Parse/Serialize ===");

      -- Fill c_tilde part with pattern
      for I in 0 .. MLDSA87_C_Tilde_Size - 1 loop
         Signature (I) := Byte (I mod 256);
      end loop;

      -- Initialize z coefficients (would normally be ML-DSA encoded)
      -- For this test, we'll create a valid structure

      -- Parse
      Parse_Signature (Signature, C_Tilde, Z, Hints, Parse_Success);

      Test ("Signature parsing succeeds", Parse_Success);

      -- Serialize back
      Serialize_Signature (C_Tilde, Z, Hints, Signature_Recon, Serialize_Success);

      Test ("Signature serialization succeeds", Serialize_Success);

      -- Check c_tilde matches
      declare
         C_Match : Boolean := True;
      begin
         for I in 0 .. MLDSA87_C_Tilde_Size - 1 loop
            if Signature (I) /= Signature_Recon (I) then
               C_Match := False;
               exit;
            end if;
         end loop;

         Test ("C_tilde round-trip matches", C_Match);
      end;

      New_Line;
   end Test_Signature_Parse;

   ---------------------------------------------------------------------------
   --  Test 6: Full Signature Compression
   ---------------------------------------------------------------------------

   procedure Test_Full_Compression is
      -- Create a synthetic signature
      Original_Sig : Byte_Array (0 .. MLDSA87_Sig_Size - 1);

      Compressed : Byte_Array (0 .. Max_Compressed_Size - 1);
      Comp_Len : Natural;
      Stats : Compression_Stats;
      Comp_Result : Compress_Result;

      Decompressed : Byte_Array (0 .. MLDSA87_Sig_Size - 1);
      Decomp_Result : Decompress_Result;
   begin
      Put_Line ("=== Test Full Signature Compression ===");

      -- Create a valid-looking signature
      -- Fill c_tilde (64 bytes)
      for I in 0 .. 63 loop
         Original_Sig (I) := Byte (16#AA#);
      end loop;

      -- Fill rest with pattern (would be z coefficients and hints)
      for I in 64 .. MLDSA87_Sig_Size - 1 loop
         Original_Sig (I) := Byte ((I * 7) mod 256);
      end loop;

      -- Compress
      Compress_Signature (Original_Sig, Compressed, Comp_Len, Stats, Comp_Result);

      if Comp_Result = Success then
         Test ("Compression succeeds", True);
         Test ("Compressed size reasonable", Comp_Len > 0 and Comp_Len < MLDSA87_Sig_Size);
         Test ("Compression ratio < 100%", Stats.Compression_Ratio < 100);

         Put_Line ("   Original size: " & Natural'Image (Stats.Original_Size) & " bytes");
         Put_Line ("   Compressed size: " & Natural'Image (Stats.Compressed_Size) & " bytes");
         Put_Line ("   Compression ratio: " & Natural'Image (Stats.Compression_Ratio) & "%");
         Put_Line ("   Space saved: " & Natural'Image (Stats.Original_Size - Stats.Compressed_Size) & " bytes");
         Put_Line ("   Small Z count: " & Natural'Image (Stats.Small_Z_Count) & " / " & Natural'Image (L * N));
         Put_Line ("   Hint bits set: " & Natural'Image (Stats.Hint_Set_Count));

         -- Decompress
         Decompress_Signature (Compressed (0 .. Comp_Len - 1), Decompressed, Decomp_Result);

         if Decomp_Result = Success then
            Test ("Decompression succeeds", True);

            -- Verify full round-trip
            declare
               All_Match : Boolean := True;
            begin
               for I in Original_Sig'Range loop
                  if Original_Sig (I) /= Decompressed (I) then
                     All_Match := False;
                     Put_Line ("   Mismatch at byte " & Natural'Image (I) &
                              ": " & Byte'Image (Original_Sig (I)) &
                              " /= " & Byte'Image (Decompressed (I)));
                     exit;
                  end if;
               end loop;

               Test ("Full round-trip preserves signature", All_Match);
            end;
         else
            Test ("Decompression succeeds", False);
            Put_Line ("   Decompress result: " & Decompress_Result'Image);
         end if;

      else
         Test ("Compression succeeds", False);
         Put_Line ("   Compress result: " & Compress_Result'Image);
      end if;

      New_Line;
   end Test_Full_Compression;

   ---------------------------------------------------------------------------
   --  Test 7: Is_Compressed Detection
   ---------------------------------------------------------------------------

   procedure Test_Is_Compressed is
      Compressed_Data : Byte_Array (0 .. 99) := (others => 0);
      Uncompressed_Data : Byte_Array (0 .. 99) := (others => 16#FF#);
   begin
      Put_Line ("=== Test Is_Compressed ===");

      Compressed_Data (0) := Magic_Byte_1;
      Compressed_Data (1) := Magic_Byte_2;

      Test ("Detects compressed data", Is_Compressed (Compressed_Data));
      Test ("Detects uncompressed data", not Is_Compressed (Uncompressed_Data));

      New_Line;
   end Test_Is_Compressed;

   ---------------------------------------------------------------------------
   --  Test 8: Error Conditions
   ---------------------------------------------------------------------------

   procedure Test_Error_Conditions is
      Too_Small : Byte_Array (0 .. 100);
      Result : Decompress_Result;
      Sig : Byte_Array (0 .. MLDSA87_Sig_Size - 1);
   begin
      Put_Line ("=== Test Error Conditions ===");

      -- Test decompression with invalid header
      Too_Small := (others => 0);
      Decompress_Signature (Too_Small, Sig, Result);
      Test ("Rejects invalid header", Result /= Success);

      -- Test decompression with wrong magic
      Too_Small (0) := 16#FF#;
      Too_Small (1) := 16#FF#;
      Decompress_Signature (Too_Small, Sig, Result);
      Test ("Rejects wrong magic bytes", Result /= Success);

      New_Line;
   end Test_Error_Conditions;

   ---------------------------------------------------------------------------
   --  Main Test Runner
   ---------------------------------------------------------------------------

begin
   Put_Line ("===============================================");
   Put_Line ("  ML-DSA-87 Signature Compression Test Suite");
   Put_Line ("===============================================");
   New_Line;

   Test_CRC16;
   Test_Header;
   Test_Z_Vector;
   Test_Hint_Vector;
   Test_Signature_Parse;
   Test_Full_Compression;
   Test_Is_Compressed;
   Test_Error_Conditions;

   Put_Line ("===============================================");
   Put_Line ("  Results: " & Natural'Image (Pass_Count) & " / " &
            Natural'Image (Test_Count) & " tests passed");
   Put_Line ("===============================================");

   if Pass_Count = Test_Count then
      Put_Line ("SUCCESS: All tests passed!");
   else
      Put_Line ("FAILURE: Some tests failed!");
   end if;

end Test_MLDSA_Compress;
