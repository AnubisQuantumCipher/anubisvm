-------------------------------------------------------------------------------
--  ANUBIS WHISPER - Confidential Transactions
--  Hiding transaction amounts with Ajtai commitments and range proofs
--
--  Uses lattice-based commitments for post-quantum security
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;
with Anubis_Bytes; use Anubis_Bytes;

package Anubis_Whisper with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Commitment parameters (Ajtai-style)
   Commitment_Rows    : constant := 256;
   Commitment_Cols    : constant := 512;
   Commitment_Modulus : constant := 8380417;  -- Same as ML-DSA q

   --  Range proof parameters (powers of 2)
   Max_Range_Bits     : constant := 64;
   Range_Proof_Size   : constant := 2048;  -- bytes

   --  Pedersen-style commitment size
   Pedersen_Size      : constant := 64;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Ajtai commitment (hiding amount)
   subtype Amount_Commitment is Byte_Array (0 .. 63);

   --  Blinding factor for commitment
   subtype Blinding_Factor is Byte_Array (0 .. 31);

   --  Range proof proving v in [0, 2^n)
   subtype Range_Proof is Byte_Array (0 .. Range_Proof_Size - 1);

   --  Named array types for SPARK (required for formal parameter arrays)
   type Commitment_Array is array (Natural range <>) of Amount_Commitment;
   type Proof_Array is array (Natural range <>) of Range_Proof;
   type Blinding_Array is array (Natural range <>) of Blinding_Factor;

   --  Confidential amount (commitment + range proof)
   type Confidential_Amount is record
      Commitment  : Amount_Commitment;
      Proof       : Range_Proof;
      Range_Bits  : Natural;  -- Proves amount < 2^Range_Bits
   end record;

   --  Confidential transfer
   type Confidential_Transfer is record
      --  Sender"s output commitment
      Input_Commit  : Amount_Commitment;

      --  Recipient"s output commitment
      Output_Commit : Amount_Commitment;

      --  Change commitment (back to sender)
      Change_Commit : Amount_Commitment;

      --  Range proofs for outputs
      Output_Proof  : Range_Proof;
      Change_Proof  : Range_Proof;

      --  Balance proof: Input = Output + Change + Fee
      Balance_Proof : Byte_Array (0 .. 255);

      --  Fee (public)
      Fee           : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Commitment Operations
   ---------------------------------------------------------------------------

   --  Create Ajtai commitment: C = A*r + b*v (mod q)
   --  where A is public matrix, r is random, b is base point, v is value
   procedure Create_Commitment (
      Value      : Unsigned_64;
      Blinding   : Blinding_Factor;
      Commitment : out Amount_Commitment
   ) with
      Global => null;

   --  Verify commitment opens to value
   function Verify_Commitment (
      Commitment : Amount_Commitment;
      Value      : Unsigned_64;
      Blinding   : Blinding_Factor
   ) return Boolean with
      Global => null;

   --  Homomorphic addition: Commit(a) + Commit(b) = Commit(a+b)
   procedure Add_Commitments (
      A, B   : Amount_Commitment;
      Result : out Amount_Commitment
   ) with
      Global => null;

   --  Homomorphic subtraction
   procedure Sub_Commitments (
      A, B   : Amount_Commitment;
      Result : out Amount_Commitment
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Range Proofs
   ---------------------------------------------------------------------------

   --  Create range proof that committed value is in [0, 2^bits)
   procedure Create_Range_Proof (
      Value       : Unsigned_64;
      Bits        : Natural;
      Blinding    : Blinding_Factor;
      Commitment  : Amount_Commitment;
      Proof       : out Range_Proof;
      Success     : out Boolean
   ) with
      Global => null,
      Pre => Bits <= Max_Range_Bits;

   --  Verify range proof
   function Verify_Range_Proof (
      Commitment : Amount_Commitment;
      Proof      : Range_Proof;
      Bits       : Natural
   ) return Boolean with
      Global => null,
      Pre => Bits <= Max_Range_Bits;

   --  Batch verify multiple range proofs (more efficient)
   function Batch_Verify_Range_Proofs (
      Commitments : Commitment_Array;
      Proofs      : Proof_Array;
      Bits        : Natural
   ) return Boolean with
      Global => null,
      Pre => Commitments'Length = Proofs'Length and Bits <= Max_Range_Bits;

   ---------------------------------------------------------------------------
   --  Confidential Transfer
   ---------------------------------------------------------------------------

   --  Create confidential transfer
   procedure Create_Confidential_Transfer (
      Input_Value    : Unsigned_64;
      Input_Blinding : Blinding_Factor;
      Output_Value   : Unsigned_64;
      Change_Value   : Unsigned_64;
      Fee            : Unsigned_64;
      Transfer       : out Confidential_Transfer;
      Output_Blinding: out Blinding_Factor;
      Change_Blinding: out Blinding_Factor;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Output_Value + Change_Value + Fee = Input_Value;

   --  Verify confidential transfer
   function Verify_Confidential_Transfer (
      Transfer       : Confidential_Transfer;
      Input_Commit   : Amount_Commitment
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Balance Proofs
   ---------------------------------------------------------------------------

   --  Prove that sum of inputs = sum of outputs + fee
   --  Without revealing any individual amounts
   procedure Create_Balance_Proof (
      Input_Commits  : Commitment_Array;
      Output_Commits : Commitment_Array;
      Input_Blindings: Blinding_Array;
      Output_Blindings: Blinding_Array;
      Fee            : Unsigned_64;
      Proof          : out Byte_Array;
      Success        : out Boolean
   ) with
      Global => null,
      Pre => Input_Commits'Length = Input_Blindings'Length
             and Output_Commits'Length = Output_Blindings'Length;

   --  Verify balance proof
   function Verify_Balance_Proof (
      Input_Commits  : Commitment_Array;
      Output_Commits : Commitment_Array;
      Fee_Commit     : Amount_Commitment;
      Proof          : Byte_Array
   ) return Boolean with
      Global => null;

   ---------------------------------------------------------------------------
   --  Key Management for Confidential Balances
   ---------------------------------------------------------------------------

   --  Derive blinding factor from viewing key and tx data
   procedure Derive_Blinding (
      Viewing_Key : Byte_Array;
      Tx_Hash     : Byte_Array;
      Output_Index: Natural;
      Blinding    : out Blinding_Factor
   ) with
      Global => null,
      Pre => Viewing_Key'Length = 32 and Tx_Hash'Length = 32;

   --  Scan for owned outputs using viewing key
   function Scan_Output (
      Viewing_Key : Byte_Array;
      Commitment  : Amount_Commitment;
      Encrypted_Amount : Byte_Array
   ) return Unsigned_64 with
      Global => null,
      Pre => Viewing_Key'Length = 32;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Blinding (B : in out Blinding_Factor) with
      Global => null,
      Post => (for all I in B'Range => B (I) = 0);

end Anubis_Whisper;
