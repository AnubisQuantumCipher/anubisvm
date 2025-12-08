-------------------------------------------------------------------------------
--  ANUBIS Bug Bounty System
--  5% Allocation (50M ANUBIS) for Security Rewards
--
--  On-chain bug bounty and security audit reward system.
--
--  Severity Tiers:
--  - Critical: 500,000 - 5,000,000 ANUBIS (Consensus break, fund theft)
--  - High:     100,000 - 500,000   ANUBIS (State corruption, DoS)
--  - Medium:    25,000 - 100,000   ANUBIS (Privacy leak, gas manipulation)
--  - Low:        5,000 - 25,000    ANUBIS (Minor bugs, UX issues)
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Anubis_Types; use Anubis_Types;

package Anubis_Bug_Bounty with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   Bounty_Allocation          : constant := 50_000_000;  -- 5% of 1B

   --  Severity levels
   type Severity_Level is (Critical, High, Medium, Low);

   --  Reward ranges by severity
   type Reward_Range is record
      Min_Reward : Unsigned_64;
      Max_Reward : Unsigned_64;
   end record;

   Severity_Rewards : constant array (Severity_Level) of Reward_Range := (
      Critical => (500_000, 5_000_000),
      High     => (100_000, 500_000),
      Medium   => (25_000, 100_000),
      Low      => (5_000, 25_000)
   );

   --  Bug categories
   type Bug_Category is (
      Consensus_Break,      -- Consensus/chain split
      Fund_Theft,           -- Unauthorized fund access
      State_Corruption,     -- Invalid state transitions
      Denial_Of_Service,    -- Network/node DoS
      Privacy_Leak,         -- Privacy primitive bypass
      Gas_Manipulation,     -- Gas calculation exploits
      Signature_Bypass,     -- Signature verification bypass
      Cryptographic_Flaw,   -- Crypto implementation bugs
      Access_Control,       -- Permission bypass
      Minor_Bug,            -- Non-security bugs
      UX_Issue              -- User experience issues
   );

   Category_Severity : constant array (Bug_Category) of Severity_Level := (
      Consensus_Break    => Critical,
      Fund_Theft         => Critical,
      State_Corruption   => High,
      Denial_Of_Service  => High,
      Privacy_Leak       => Medium,
      Gas_Manipulation   => Medium,
      Signature_Bypass   => Critical,
      Cryptographic_Flaw => Critical,
      Access_Control     => High,
      Minor_Bug          => Low,
      UX_Issue           => Low
   );

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   type Report_Status is (
      Submitted,
      Triaging,
      Confirmed,
      Fixed,
      Disputed,
      Rejected,
      Paid,
      Duplicate
   );

   type Bug_Report is record
      ID               : Unsigned_64;
      Reporter         : Byte_Array (0 .. 31);
      Category         : Bug_Category;
      Severity         : Severity_Level;
      Status           : Report_Status;

      --  Report details
      Title_Hash       : Byte_Array (0 .. 31);
      Description_Hash : Byte_Array (0 .. 31);
      POC_Hash         : Byte_Array (0 .. 31);  -- Proof of concept
      IPFS_Report      : Byte_Array (0 .. 45);

      --  Assessment
      Assessed_Severity: Severity_Level;
      Final_Reward     : Unsigned_64;
      Fix_Commit       : Byte_Array (0 .. 31);

      --  Timing
      Submitted_At     : Unsigned_64;
      Confirmed_At     : Unsigned_64;
      Fixed_At         : Unsigned_64;
      Paid_At          : Unsigned_64;
   end record;

   type Report_Array is array (Natural range <>) of Bug_Report;

   type Bounty_State is record
      Total_Allocation   : Unsigned_64;
      Remaining_Pool     : Unsigned_64;
      Total_Paid         : Unsigned_64;
      Total_Reports      : Unsigned_64;
      Reports_Confirmed  : Unsigned_64;
      Reports_Rejected   : Unsigned_64;
      Genesis_Block      : Unsigned_64;
      Current_Block      : Unsigned_64;
   end record;

   ---------------------------------------------------------------------------
   --  Results
   ---------------------------------------------------------------------------

   type Submit_Result is (
      Submitted,
      Invalid_Category,
      Duplicate_Report,
      Pool_Exhausted,
      Reporter_Banned
   );

   type Assess_Result is (
      Assessed,
      Invalid_Report,
      Already_Assessed
   );

   type Payout_Result is (
      Paid,
      Not_Confirmed,
      Already_Paid,
      Insufficient_Pool
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Bug_Bounty (
      State          : out Bounty_State;
      Genesis_Block  : Unsigned_64
   ) with
      Global => null,
      Post => State.Remaining_Pool = Bounty_Allocation
              and State.Total_Paid = 0;

   ---------------------------------------------------------------------------
   --  Report Submission
   ---------------------------------------------------------------------------

   procedure Submit_Report (
      State          : in Out Bounty_State;
      Reporter       : Byte_Array;
      Category       : Bug_Category;
      Title_Hash     : Byte_Array;
      Description    : Byte_Array;
      POC_Hash       : Byte_Array;
      Report         : out Bug_Report;
      Result         : out Submit_Result
   ) with
      Global => null,
      Pre => Reporter'Length = 32
             and Title_Hash'Length = 32
             and Description'Length = 32
             and POC_Hash'Length = 32;

   ---------------------------------------------------------------------------
   --  Assessment
   ---------------------------------------------------------------------------

   procedure Assess_Report (
      Report         : in Out Bug_Report;
      Assessor       : Byte_Array;
      Confirmed      : Boolean;
      Final_Severity : Severity_Level;
      Reward         : Unsigned_64;
      Result         : out Assess_Result
   ) with
      Global => null,
      Pre => Assessor'Length = 32;

   function Calculate_Reward (
      Category       : Bug_Category;
      Impact_Score   : Natural;
      Quality_Score  : Natural
   ) return Unsigned_64 with
      Global => null,
      Pre => Impact_Score <= 100 and Quality_Score <= 100;

   ---------------------------------------------------------------------------
   --  Payout
   ---------------------------------------------------------------------------

   procedure Process_Payout (
      State          : in Out Bounty_State;
      Report         : in Out Bug_Report;
      Result         : out Payout_Result
   ) with
      Global => null,
      Pre => Report.Status = Confirmed or Report.Status = Fixed;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   function Get_Remaining_Pool (State : Bounty_State) return Unsigned_64 with
      Global => null;

   function Get_Reward_Range (Severity : Severity_Level) return Reward_Range with
      Global => null;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Bounty_State (State : out Bounty_State) with
      Global => null;

   procedure Zeroize_Report (R : out Bug_Report) with
      Global => null;

end Anubis_Bug_Bounty;
