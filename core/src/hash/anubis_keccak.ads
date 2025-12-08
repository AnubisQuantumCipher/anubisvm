pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

--  Anubis_Keccak: Keccak-f[1600] permutation (SHA-3 foundation)
--
--  This package implements the Keccak-f[1600] permutation, which is the core
--  of SHA3-256, SHA3-384, SHA3-512, SHAKE128, and SHAKE256.
--
--  The implementation follows FIPS 202 exactly, with particular attention to
--  the Chi step, which is often incorrectly implemented.
--
--  Security: The Keccak permutation is constant-time by design (no data-dependent
--  branches or memory accesses). This implementation preserves that property.

package Anubis_Keccak with
   SPARK_Mode => On
is

   --  Keccak state: 1600 bits = 25 lanes × 64 bits
   --  Organized as 5×5 array for the permutation steps
   type Lane is mod 2**64;
   type Lane_Index is range 0 .. 4;
   type State_Array is array (Lane_Index, Lane_Index) of Lane;

   --  Keccak-f[1600] round constants (24 rounds)
   --  Derived from LFSR defined in FIPS 202
   type Round_Index is range 0 .. 23;
   type Round_Constants_Array is array (Round_Index) of Lane;

   Round_Constants : constant Round_Constants_Array := (
      16#0000000000000001#, 16#0000000000008082#, 16#800000000000808A#, 16#8000000080008000#,
      16#000000000000808B#, 16#0000000080000001#, 16#8000000080008081#, 16#8000000000008009#,
      16#000000000000008A#, 16#0000000000000088#, 16#0000000080008009#, 16#000000008000000A#,
      16#000000008000808B#, 16#800000000000008B#, 16#8000000000008089#, 16#8000000000008003#,
      16#8000000000008002#, 16#8000000000000080#, 16#000000000000800A#, 16#800000008000000A#,
      16#8000000080008081#, 16#8000000000008080#, 16#0000000080000001#, 16#8000000080008008#
   );

   --  Rotation offsets for Rho step (FIPS 202 Section 3.2.2)
   --  These are constant for Keccak-f[1600]
   type Rho_Offsets_Array is array (Lane_Index, Lane_Index) of Natural;

   Rho_Offsets : constant Rho_Offsets_Array := (
      (0,  36,  3, 41, 18),
      (1,  44, 10, 45,  2),
      (62,  6, 43, 15, 61),
      (28, 55, 25, 21, 56),
      (27, 20, 39,  8, 14)
   );

   --  Theta step: Linear diffusion
   --  Computes parity of each column and XORs it into adjacent columns
   procedure Theta (State : in out State_Array) with
      Global => null,
      Pre => True,
      Post => True;

   --  Rho step: Rotations
   --  Rotates each lane by a triangular number offset
   procedure Rho (State : in out State_Array) with
      Global => null,
      Pre => True,
      Post => True;

   --  Pi step: Permutation
   --  Rearranges lanes within the state
   procedure Pi (State : in out State_Array) with
      Global => null,
      Pre => True,
      Post => True;

   --  Chi step: Nonlinear transformation
   --  CRITICAL: This is where many implementations get it wrong!
   --
   --  The correct formula from FIPS 202 Section 3.2.4 is:
   --    A"[x,y] = A[x,y] XOR ((NOT A[x+1,y]) AND A[x+2,y])
   --
   --  Common mistake: A[x,y] AND NOT B OR NOT A[x,y] AND B (this is XOR, not Chi!)
   --
   --  Visual for row (y fixed):
   --    A"[0] = A[0] ⊕ (¬A[1] ∧ A[2])
   --    A"[1] = A[1] ⊕ (¬A[2] ∧ A[3])
   --    A"[2] = A[2] ⊕ (¬A[3] ∧ A[4])
   --    A"[3] = A[3] ⊕ (¬A[4] ∧ A[0])
   --    A"[4] = A[4] ⊕ (¬A[0] ∧ A[1])
   --
   procedure Chi (State : in out State_Array) with
      Global => null,
      Pre => True,
      Post => True;

   --  Iota step: Round constant addition
   --  Breaks symmetry by adding round-specific constant to A[0,0]
   procedure Iota (State : in out State_Array; Round : Round_Index) with
      Global => null,
      Pre => True,
      Post => True;

   --  Keccak-f[1600] permutation: 24 rounds of θ, ρ, π, χ, ι
   --  This is the core cryptographic primitive
   procedure Keccak_F (State : in out State_Array) with
      Global => null,
      Pre => True,
      Post => True;

end Anubis_Keccak;
