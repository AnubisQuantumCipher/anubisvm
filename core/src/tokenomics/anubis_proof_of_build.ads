-------------------------------------------------------------------------------
--  ANUBIS Proof-of-Build NFT System
--  Immutable On-Chain Verification Records
--
--  INNOVATION: Every significant deliverable generates an on-chain
--  Proof-of-Build NFT that permanently records:
--  - Git commit hash
--  - GNATprove verification hash
--  - Lines of code added
--  - Timestamp
--  - IPFS link to full verification report
--
--  This creates an immutable, verifiable record of work that no one can
--  dispute. Years later, anyone can verify the builder actually built
--  what they claimed.
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Proof_Of_Build with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  NFT Collection
   Collection_Name            : constant String := "ANUBIS Proof-of-Build";
   Collection_Symbol          : constant String := "APOB";

   --  Build categories
   type Build_Category is (
      Core_Cryptography,     -- ML-KEM, ML-DSA, SHA3
      VM_Implementation,     -- AegisVM core
      Privacy_Layer,         -- SHIELD, WHISPER, EYE
      Proof_System,          -- STARK, SCARAB
      SDK_Tooling,           -- Developer tools
      Documentation,         -- Specs, guides
      Testing,               -- Test suites, KAT
      Integration,           -- External integrations
      Milestone              -- Major milestone completion
   );

   --  Minimum requirements for minting
   Min_Lines_For_NFT          : constant := 100;    -- 100 lines minimum
   Min_VCs_For_NFT            : constant := 10;     -- 10 VCs proven minimum

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  NFT metadata
   type Proof_Of_Build_NFT is record
      Token_ID         : Unsigned_64;
      Minter           : Byte_Array (0 .. 31);  -- Builder address

      --  Build information
      Category         : Build_Category;
      Description_Hash : Byte_Array (0 .. 31);

      --  Verification data
      Commit_Hash      : Byte_Array (0 .. 31);
      GNATprove_Hash   : Byte_Array (0 .. 31);
      Lines_Added      : Unsigned_32;
      Lines_Removed    : Unsigned_32;
      VCs_Proven       : Unsigned_32;
      Test_Coverage    : Natural;  -- Percentage 0-100

      --  External references
      IPFS_Report      : Byte_Array (0 .. 45);  -- CID to full report
      Github_PR_Hash   : Byte_Array (0 .. 31);  -- PR link hash

      --  Timing
      Minted_At        : Unsigned_64;  -- Block number
      Timestamp        : Unsigned_64;  -- Unix timestamp

      --  Verification status
      Verified         : Boolean;
      Verifier_PK      : Byte_Array (0 .. 31);
      Verification_Sig : Byte_Array (0 .. 127);
   end record;

   type NFT_Array is array (Natural range <>) of Proof_Of_Build_NFT;

   --  Category counts array type
   type Category_Count_Array is array (Build_Category) of Unsigned_32;

   --  Collection state
   type Collection_State is record
      Total_Minted     : Unsigned_64;
      Total_Lines      : Unsigned_64;
      Total_VCs        : Unsigned_64;
      Category_Counts  : Category_Count_Array;
      Builder_Address  : Byte_Array (0 .. 31);
      Genesis_Block    : Unsigned_64;
      Current_Block    : Unsigned_64;
   end record;

   --  Weekly update NFT (special type)
   type Weekly_Update_NFT is record
      Token_ID         : Unsigned_64;
      Week_Number      : Unsigned_32;
      Year             : Unsigned_16;

      --  Weekly stats
      Commits_Count    : Unsigned_32;
      Lines_Added      : Unsigned_32;
      VCs_Proven       : Unsigned_32;
      Milestone_Progress : Natural;  -- Percentage

      --  Content
      Summary_Hash     : Byte_Array (0 .. 31);
      IPFS_Link        : Byte_Array (0 .. 45);
      Video_Link_Hash  : Byte_Array (0 .. 31);  -- Livestream if applicable

      --  Timing
      Minted_At        : Unsigned_64;
      Week_Start       : Unsigned_64;
      Week_End         : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Results
   ---------------------------------------------------------------------------

   type Mint_Result is (
      Minted,
      Insufficient_Lines,
      Insufficient_VCs,
      Duplicate_Commit,
      Invalid_Proof,
      Not_Builder
   );

   type Verify_Result is (
      Verified,
      Invalid_Signature,
      Already_Verified,
      Not_Found
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Collection (
      State          : out Collection_State;
      Builder        : Byte_Array;
      Genesis_Block  : Unsigned_64
   ) with
      Global => null,
      Pre => Builder'Length = 32,
      Post => State.Total_Minted = 0;

   ---------------------------------------------------------------------------
   --  NFT Minting
   ---------------------------------------------------------------------------

   --  Mint Proof-of-Build NFT
   procedure Mint_Build_NFT (
      State          : in Out Collection_State;
      Category       : Build_Category;
      Commit_Hash    : Byte_Array;
      Proof_Hash     : Byte_Array;
      Lines_Added    : Unsigned_32;
      Lines_Removed  : Unsigned_32;
      VCs_Proven     : Unsigned_32;
      IPFS_Report    : Byte_Array;
      NFT            : out Proof_Of_Build_NFT;
      Result         : out Mint_Result
   ) with
      Global => null,
      Pre => Commit_Hash'Length = 32
             and Proof_Hash'Length = 32
             and IPFS_Report'Length = 46;

   --  Mint weekly update NFT
   procedure Mint_Weekly_Update (
      State          : in Out Collection_State;
      Week_Number    : Unsigned_32;
      Year           : Unsigned_16;
      Commits_Count  : Unsigned_32;
      Lines_Added    : Unsigned_32;
      VCs_Proven     : Unsigned_32;
      Progress       : Natural;
      Summary_Hash   : Byte_Array;
      IPFS_Link      : Byte_Array;
      NFT            : out Weekly_Update_NFT;
      Result         : out Mint_Result
   ) with
      Global => null,
      Pre => Summary_Hash'Length = 32
             and IPFS_Link'Length = 46
             and Progress <= 100;

   --  Mint milestone NFT (special commemorative)
   procedure Mint_Milestone_NFT (
      State          : in Out Collection_State;
      Milestone_ID   : Natural;
      Commit_Hash    : Byte_Array;
      Proof_Hash     : Byte_Array;
      Total_Lines    : Unsigned_32;
      Total_VCs      : Unsigned_32;
      IPFS_Report    : Byte_Array;
      NFT            : out Proof_Of_Build_NFT;
      Result         : out Mint_Result
   ) with
      Global => null,
      Pre => Commit_Hash'Length = 32
             and Proof_Hash'Length = 32
             and IPFS_Report'Length = 46;

   ---------------------------------------------------------------------------
   --  Verification
   ---------------------------------------------------------------------------

   --  Verify NFT by third party
   procedure Verify_NFT (
      NFT            : in Out Proof_Of_Build_NFT;
      Verifier_PK    : Byte_Array;
      Signature      : Byte_Array;
      Result         : out Verify_Result
   ) with
      Global => null,
      Pre => Verifier_PK'Length = 32
             and Signature'Length >= 64;

   --  Verify commit hash matches stored
   function Verify_Commit (
      NFT            : Proof_Of_Build_NFT;
      Commit_Hash    : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Commit_Hash'Length = 32;

   --  Verify proof hash matches stored
   function Verify_Proof (
      NFT            : Proof_Of_Build_NFT;
      Proof_Hash     : Byte_Array
   ) return Boolean with
      Global => null,
      Pre => Proof_Hash'Length = 32;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   --  Get NFT by token ID
   procedure Get_NFT_By_ID (
      Collection     : NFT_Array;
      Token_ID       : Unsigned_64;
      NFT            : out Proof_Of_Build_NFT;
      Found          : out Boolean
   ) with
      Global => null;

   --  Get total minted
   function Get_Total_Minted (State : Collection_State) return Unsigned_64 with
      Global => null;

   --  Get total lines across all NFTs
   function Get_Total_Lines (State : Collection_State) return Unsigned_64 with
      Global => null;

   --  Get total VCs across all NFTs
   function Get_Total_VCs (State : Collection_State) return Unsigned_64 with
      Global => null;

   --  Get category count
   function Get_Category_Count (
      State          : Collection_State;
      Category       : Build_Category
   ) return Unsigned_32 with
      Global => null;

   ---------------------------------------------------------------------------
   --  NFT Metadata Generation
   ---------------------------------------------------------------------------

   --  Generate token URI metadata
   procedure Generate_Token_URI (
      NFT            : Proof_Of_Build_NFT;
      URI            : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => URI'Length >= 256;

   --  Generate collection metadata
   procedure Generate_Collection_Metadata (
      State          : Collection_State;
      Metadata       : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Metadata'Length >= 512;

   ---------------------------------------------------------------------------
   --  Statistics
   ---------------------------------------------------------------------------

   type Build_Statistics is record
      Total_NFTs       : Unsigned_64;
      Total_Lines      : Unsigned_64;
      Total_VCs        : Unsigned_64;
      Avg_Lines_Per_NFT: Unsigned_32;
      Avg_VCs_Per_NFT  : Unsigned_32;
      Weekly_Updates   : Unsigned_32;
      Milestones_Hit   : Natural;
      First_Mint_Block : Unsigned_64;
      Last_Mint_Block  : Unsigned_64;
   end record;

   procedure Calculate_Statistics (
      State          : Collection_State;
      NFTs           : NFT_Array;
      Stats          : out Build_Statistics
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_NFT (
      NFT            : Proof_Of_Build_NFT;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 512;

   procedure Deserialize_NFT (
      Input          : Byte_Array;
      NFT            : out Proof_Of_Build_NFT;
      Success        : out Boolean
   ) with
      Global => null;

   procedure Serialize_Collection_State (
      State          : Collection_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) with
      Global => null,
      Pre => Output'Length >= 256;

   procedure Deserialize_Collection_State (
      Input          : Byte_Array;
      State          : out Collection_State;
      Success        : out Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Collection_State (State : out Collection_State) with
      Global => null;

   procedure Zeroize_NFT (NFT : out Proof_Of_Build_NFT) with
      Global => null;

   procedure Zeroize_Weekly_Update (NFT : out Weekly_Update_NFT) with
      Global => null;

end Anubis_Proof_Of_Build;
