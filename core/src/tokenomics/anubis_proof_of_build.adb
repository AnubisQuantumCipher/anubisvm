-------------------------------------------------------------------------------
--  ANUBIS Proof-of-Build NFT System Implementation Body
--  Immutable On-Chain Verification Records
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Anubis_Proof_Of_Build with
   SPARK_Mode => On
is
   --  NFT counter
   NFT_Counter : Unsigned_64 := 0;
   Weekly_NFT_Counter : Unsigned_64 := 0;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Init_Collection (
      State          : out Collection_State;
      Builder        : Byte_Array;
      Genesis_Block  : Unsigned_64
   ) is
   begin
      State.Total_Minted := 0;
      State.Total_Lines := 0;
      State.Total_VCs := 0;

      for C in Build_Category loop
         State.Category_Counts (C) := 0;
      end loop;

      --  Copy builder address
      State.Builder_Address := (others => 0);
      for I in Builder'Range loop
         if I - Builder'First <= State.Builder_Address'Last then
            State.Builder_Address (I - Builder'First) := Builder (I);
         end if;
      end loop;

      State.Genesis_Block := Genesis_Block;
      State.Current_Block := Genesis_Block;
   end Init_Collection;

   ---------------------------------------------------------------------------
   --  NFT Minting
   ---------------------------------------------------------------------------

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
   ) is
   begin
      --  Check minimum requirements
      if Lines_Added < Min_Lines_For_NFT then
         Result := Insufficient_Lines;
         NFT := (
            Token_ID => 0,
            Minter => (others => 0),
            Category => Core_Cryptography,
            Description_Hash => (others => 0),
            Commit_Hash => (others => 0),
            GNATprove_Hash => (others => 0),
            Lines_Added => 0,
            Lines_Removed => 0,
            VCs_Proven => 0,
            Test_Coverage => 0,
            IPFS_Report => (others => 0),
            Github_PR_Hash => (others => 0),
            Minted_At => 0,
            Timestamp => 0,
            Verified => False,
            Verifier_PK => (others => 0),
            Verification_Sig => (others => 0)
         );
         return;
      end if;

      if VCs_Proven < Min_VCs_For_NFT then
         Result := Insufficient_VCs;
         NFT := (
            Token_ID => 0,
            Minter => (others => 0),
            Category => Core_Cryptography,
            Description_Hash => (others => 0),
            Commit_Hash => (others => 0),
            GNATprove_Hash => (others => 0),
            Lines_Added => 0,
            Lines_Removed => 0,
            VCs_Proven => 0,
            Test_Coverage => 0,
            IPFS_Report => (others => 0),
            Github_PR_Hash => (others => 0),
            Minted_At => 0,
            Timestamp => 0,
            Verified => False,
            Verifier_PK => (others => 0),
            Verification_Sig => (others => 0)
         );
         return;
      end if;

      --  Mint NFT
      NFT_Counter := NFT_Counter + 1;

      NFT.Token_ID := NFT_Counter;
      NFT.Minter := State.Builder_Address;
      NFT.Category := Category;
      NFT.Description_Hash := (others => 0);

      --  Copy commit hash
      NFT.Commit_Hash := (others => 0);
      for I in Commit_Hash'Range loop
         if I - Commit_Hash'First <= NFT.Commit_Hash'Last then
            NFT.Commit_Hash (I - Commit_Hash'First) := Commit_Hash (I);
         end if;
      end loop;

      --  Copy proof hash
      NFT.GNATprove_Hash := (others => 0);
      for I in Proof_Hash'Range loop
         if I - Proof_Hash'First <= NFT.GNATprove_Hash'Last then
            NFT.GNATprove_Hash (I - Proof_Hash'First) := Proof_Hash (I);
         end if;
      end loop;

      NFT.Lines_Added := Lines_Added;
      NFT.Lines_Removed := Lines_Removed;
      NFT.VCs_Proven := VCs_Proven;
      NFT.Test_Coverage := 0;

      --  Copy IPFS report
      NFT.IPFS_Report := (others => 0);
      for I in IPFS_Report'Range loop
         if I - IPFS_Report'First <= NFT.IPFS_Report'Last then
            NFT.IPFS_Report (I - IPFS_Report'First) := IPFS_Report (I);
         end if;
      end loop;

      NFT.Github_PR_Hash := (others => 0);
      NFT.Minted_At := State.Current_Block;
      --  Calculate timestamp from block height (assuming 6-second blocks)
      --  Estimate: blocks * 6 seconds = Unix timestamp offset from genesis
      NFT.Timestamp := State.Current_Block * 6;
      NFT.Verified := False;
      NFT.Verifier_PK := (others => 0);
      NFT.Verification_Sig := (others => 0);

      --  Update state
      State.Total_Minted := State.Total_Minted + 1;
      State.Total_Lines := State.Total_Lines + Unsigned_64 (Lines_Added);
      State.Total_VCs := State.Total_VCs + Unsigned_64 (VCs_Proven);
      State.Category_Counts (Category) := State.Category_Counts (Category) + 1;

      Result := Minted;
   end Mint_Build_NFT;

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
   ) is
   begin
      Weekly_NFT_Counter := Weekly_NFT_Counter + 1;

      NFT.Token_ID := Weekly_NFT_Counter;
      NFT.Week_Number := Week_Number;
      NFT.Year := Year;
      NFT.Commits_Count := Commits_Count;
      NFT.Lines_Added := Lines_Added;
      NFT.VCs_Proven := VCs_Proven;
      NFT.Milestone_Progress := Progress;

      --  Copy summary hash
      NFT.Summary_Hash := (others => 0);
      for I in Summary_Hash'Range loop
         if I - Summary_Hash'First <= NFT.Summary_Hash'Last then
            NFT.Summary_Hash (I - Summary_Hash'First) := Summary_Hash (I);
         end if;
      end loop;

      --  Copy IPFS link
      NFT.IPFS_Link := (others => 0);
      for I in IPFS_Link'Range loop
         if I - IPFS_Link'First <= NFT.IPFS_Link'Last then
            NFT.IPFS_Link (I - IPFS_Link'First) := IPFS_Link (I);
         end if;
      end loop;

      NFT.Video_Link_Hash := (others => 0);
      NFT.Minted_At := State.Current_Block;
      NFT.Week_Start := 0;
      NFT.Week_End := 0;

      --  Update state
      State.Total_Lines := State.Total_Lines + Unsigned_64 (Lines_Added);
      State.Total_VCs := State.Total_VCs + Unsigned_64 (VCs_Proven);

      Result := Minted;
   end Mint_Weekly_Update;

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
   ) is
      pragma Unreferenced (Milestone_ID);
   begin
      --  Milestones bypass minimum requirements
      NFT_Counter := NFT_Counter + 1;

      NFT.Token_ID := NFT_Counter;
      NFT.Minter := State.Builder_Address;
      NFT.Category := Milestone;
      NFT.Description_Hash := (others => 0);

      --  Copy commit hash
      NFT.Commit_Hash := (others => 0);
      for I in Commit_Hash'Range loop
         if I - Commit_Hash'First <= NFT.Commit_Hash'Last then
            NFT.Commit_Hash (I - Commit_Hash'First) := Commit_Hash (I);
         end if;
      end loop;

      --  Copy proof hash
      NFT.GNATprove_Hash := (others => 0);
      for I in Proof_Hash'Range loop
         if I - Proof_Hash'First <= NFT.GNATprove_Hash'Last then
            NFT.GNATprove_Hash (I - Proof_Hash'First) := Proof_Hash (I);
         end if;
      end loop;

      NFT.Lines_Added := Total_Lines;
      NFT.Lines_Removed := 0;
      NFT.VCs_Proven := Total_VCs;
      NFT.Test_Coverage := 100;

      --  Copy IPFS report
      NFT.IPFS_Report := (others => 0);
      for I in IPFS_Report'Range loop
         if I - IPFS_Report'First <= NFT.IPFS_Report'Last then
            NFT.IPFS_Report (I - IPFS_Report'First) := IPFS_Report (I);
         end if;
      end loop;

      NFT.Github_PR_Hash := (others => 0);
      NFT.Minted_At := State.Current_Block;
      NFT.Timestamp := 0;
      NFT.Verified := False;
      NFT.Verifier_PK := (others => 0);
      NFT.Verification_Sig := (others => 0);

      --  Update state
      State.Total_Minted := State.Total_Minted + 1;
      State.Category_Counts (Milestone) := State.Category_Counts (Milestone) + 1;

      Result := Minted;
   end Mint_Milestone_NFT;

   ---------------------------------------------------------------------------
   --  Verification
   ---------------------------------------------------------------------------

   procedure Verify_NFT (
      NFT            : in Out Proof_Of_Build_NFT;
      Verifier_PK    : Byte_Array;
      Signature      : Byte_Array;
      Result         : out Verify_Result
   ) is
   begin
      --  Check not already verified
      if NFT.Verified then
         Result := Already_Verified;
         return;
      end if;

      --  Copy verifier PK
      for I in Verifier_PK'Range loop
         if I - Verifier_PK'First <= NFT.Verifier_PK'Last then
            NFT.Verifier_PK (I - Verifier_PK'First) := Verifier_PK (I);
         end if;
      end loop;

      --  Copy signature
      for I in Signature'Range loop
         if I - Signature'First <= NFT.Verification_Sig'Last then
            NFT.Verification_Sig (I - Signature'First) := Signature (I);
         end if;
      end loop;

      NFT.Verified := True;

      Result := Verified;
   end Verify_NFT;

   function Verify_Commit (
      NFT            : Proof_Of_Build_NFT;
      Commit_Hash    : Byte_Array
   ) return Boolean is
   begin
      for I in Commit_Hash'Range loop
         if I - Commit_Hash'First <= NFT.Commit_Hash'Last then
            if Commit_Hash (I) /= NFT.Commit_Hash (I - Commit_Hash'First) then
               return False;
            end if;
         end if;
      end loop;

      return True;
   end Verify_Commit;

   function Verify_Proof (
      NFT            : Proof_Of_Build_NFT;
      Proof_Hash     : Byte_Array
   ) return Boolean is
   begin
      for I in Proof_Hash'Range loop
         if I - Proof_Hash'First <= NFT.GNATprove_Hash'Last then
            if Proof_Hash (I) /= NFT.GNATprove_Hash (I - Proof_Hash'First) then
               return False;
            end if;
         end if;
      end loop;

      return True;
   end Verify_Proof;

   ---------------------------------------------------------------------------
   --  Queries
   ---------------------------------------------------------------------------

   procedure Get_NFT_By_ID (
      Collection     : NFT_Array;
      Token_ID       : Unsigned_64;
      NFT            : out Proof_Of_Build_NFT;
      Found          : out Boolean
   ) is
   begin
      Found := False;
      NFT := (
         Token_ID => 0,
         Minter => (others => 0),
         Category => Core_Cryptography,
         Description_Hash => (others => 0),
         Commit_Hash => (others => 0),
         GNATprove_Hash => (others => 0),
         Lines_Added => 0,
         Lines_Removed => 0,
         VCs_Proven => 0,
         Test_Coverage => 0,
         IPFS_Report => (others => 0),
         Github_PR_Hash => (others => 0),
         Minted_At => 0,
         Timestamp => 0,
         Verified => False,
         Verifier_PK => (others => 0),
         Verification_Sig => (others => 0)
      );

      for I in Collection'Range loop
         if Collection (I).Token_ID = Token_ID then
            NFT := Collection (I);
            Found := True;
            return;
         end if;
      end loop;
   end Get_NFT_By_ID;

   function Get_Total_Minted (State : Collection_State) return Unsigned_64 is
   begin
      return State.Total_Minted;
   end Get_Total_Minted;

   function Get_Total_Lines (State : Collection_State) return Unsigned_64 is
   begin
      return State.Total_Lines;
   end Get_Total_Lines;

   function Get_Total_VCs (State : Collection_State) return Unsigned_64 is
   begin
      return State.Total_VCs;
   end Get_Total_VCs;

   function Get_Category_Count (
      State          : Collection_State;
      Category       : Build_Category
   ) return Unsigned_32 is
   begin
      return State.Category_Counts (Category);
   end Get_Category_Count;

   ---------------------------------------------------------------------------
   --  NFT Metadata Generation
   ---------------------------------------------------------------------------

   procedure Generate_Token_URI (
      NFT            : Proof_Of_Build_NFT;
      URI            : out Byte_Array;
      Length         : out Natural
   ) is
      Idx : Natural := URI'First;
      Prefix : constant String := "ipfs://";
   begin
      URI := (others => 0);

      --  Write prefix
      for I in Prefix'Range loop
         if Idx <= URI'Last then
            URI (Idx) := Character'Pos (Prefix (I));
            Idx := Idx + 1;
         end if;
      end loop;

      --  Copy IPFS hash
      for I in NFT.IPFS_Report'Range loop
         if NFT.IPFS_Report (I) /= 0 and then Idx <= URI'Last then
            URI (Idx) := NFT.IPFS_Report (I);
            Idx := Idx + 1;
         end if;
      end loop;

      Length := Idx - URI'First;
   end Generate_Token_URI;

   procedure Generate_Collection_Metadata (
      State          : Collection_State;
      Metadata       : out Byte_Array;
      Length         : out Natural
   ) is
      Idx : Natural := Metadata'First;

      procedure Write_U64 (Val : Unsigned_64) is
         V : Unsigned_64 := Val;
      begin
         for J in 0 .. 7 loop
            if Idx <= Metadata'Last then
               Metadata (Idx) := Unsigned_8 (V mod 256);
               V := V / 256;
               Idx := Idx + 1;
            end if;
         end loop;
      end Write_U64;

   begin
      Metadata := (others => 0);

      Write_U64 (State.Total_Minted);
      Write_U64 (State.Total_Lines);
      Write_U64 (State.Total_VCs);

      --  Builder address
      for I in State.Builder_Address'Range loop
         if Idx <= Metadata'Last then
            Metadata (Idx) := State.Builder_Address (I);
            Idx := Idx + 1;
         end if;
      end loop;

      Write_U64 (State.Genesis_Block);

      Length := Idx - Metadata'First;
   end Generate_Collection_Metadata;

   ---------------------------------------------------------------------------
   --  Statistics
   ---------------------------------------------------------------------------

   procedure Calculate_Statistics (
      State          : Collection_State;
      NFTs           : NFT_Array;
      Stats          : out Build_Statistics
   ) is
      pragma Unreferenced (NFTs);
   begin
      Stats.Total_NFTs := State.Total_Minted;
      Stats.Total_Lines := State.Total_Lines;
      Stats.Total_VCs := State.Total_VCs;

      --  Calculate averages
      if State.Total_Minted > 0 then
         Stats.Avg_Lines_Per_NFT :=
            Unsigned_32 (State.Total_Lines / State.Total_Minted);
         Stats.Avg_VCs_Per_NFT :=
            Unsigned_32 (State.Total_VCs / State.Total_Minted);
      else
         Stats.Avg_Lines_Per_NFT := 0;
         Stats.Avg_VCs_Per_NFT := 0;
      end if;

      --  Count weekly update NFTs from the collection
      Stats.Weekly_Updates := Unsigned_32 (Weekly_NFT_Counter);
      Stats.Milestones_Hit := Natural (State.Category_Counts (Milestone));
      Stats.First_Mint_Block := State.Genesis_Block;
      Stats.Last_Mint_Block := State.Current_Block;
   end Calculate_Statistics;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_NFT (
      NFT            : Proof_Of_Build_NFT;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
      Idx : Natural := Output'First;

      procedure Write_U64 (Val : Unsigned_64) is
         V : Unsigned_64 := Val;
      begin
         for J in 0 .. 7 loop
            if Idx <= Output'Last then
               Output (Idx) := Unsigned_8 (V mod 256);
               V := V / 256;
               Idx := Idx + 1;
            end if;
         end loop;
      end Write_U64;

      procedure Write_U32 (Val : Unsigned_32) is
         V : Unsigned_32 := Val;
      begin
         for J in 0 .. 3 loop
            if Idx <= Output'Last then
               Output (Idx) := Unsigned_8 (V mod 256);
               V := V / 256;
               Idx := Idx + 1;
            end if;
         end loop;
      end Write_U32;

   begin
      Output := (others => 0);

      Write_U64 (NFT.Token_ID);

      --  Minter
      for I in NFT.Minter'Range loop
         if Idx <= Output'Last then
            Output (Idx) := NFT.Minter (I);
            Idx := Idx + 1;
         end if;
      end loop;

      --  Category
      if Idx <= Output'Last then
         Output (Idx) := Build_Category'Pos (NFT.Category);
         Idx := Idx + 1;
      end if;

      --  Commit hash
      for I in NFT.Commit_Hash'Range loop
         if Idx <= Output'Last then
            Output (Idx) := NFT.Commit_Hash (I);
            Idx := Idx + 1;
         end if;
      end loop;

      --  GNATprove hash
      for I in NFT.GNATprove_Hash'Range loop
         if Idx <= Output'Last then
            Output (Idx) := NFT.GNATprove_Hash (I);
            Idx := Idx + 1;
         end if;
      end loop;

      Write_U32 (NFT.Lines_Added);
      Write_U32 (NFT.Lines_Removed);
      Write_U32 (NFT.VCs_Proven);

      Write_U64 (NFT.Minted_At);

      --  Verified flag
      if Idx <= Output'Last then
         Output (Idx) := (if NFT.Verified then 1 else 0);
         Idx := Idx + 1;
      end if;

      Length := Idx - Output'First;
   end Serialize_NFT;

   procedure Deserialize_NFT (
      Input          : Byte_Array;
      NFT            : out Proof_Of_Build_NFT;
      Success        : out Boolean
   ) is
      Idx : Natural := Input'First;

      procedure Read_U64 (Val : out Unsigned_64) is
      begin
         Val := 0;
         for J in 0 .. 7 loop
            if Idx <= Input'Last then
               Val := Val + Unsigned_64 (Input (Idx)) * (256 ** J);
               Idx := Idx + 1;
            end if;
         end loop;
      end Read_U64;

      procedure Read_U32 (Val : out Unsigned_32) is
      begin
         Val := 0;
         for J in 0 .. 3 loop
            if Idx <= Input'Last then
               Val := Val + Unsigned_32 (Input (Idx)) * (256 ** J);
               Idx := Idx + 1;
            end if;
         end loop;
      end Read_U32;

   begin
      --  Initialize
      NFT := (
         Token_ID => 0,
         Minter => (others => 0),
         Category => Core_Cryptography,
         Description_Hash => (others => 0),
         Commit_Hash => (others => 0),
         GNATprove_Hash => (others => 0),
         Lines_Added => 0,
         Lines_Removed => 0,
         VCs_Proven => 0,
         Test_Coverage => 0,
         IPFS_Report => (others => 0),
         Github_PR_Hash => (others => 0),
         Minted_At => 0,
         Timestamp => 0,
         Verified => False,
         Verifier_PK => (others => 0),
         Verification_Sig => (others => 0)
      );
      Success := False;

      if Input'Length < 110 then
         return;
      end if;

      Read_U64 (NFT.Token_ID);

      --  Minter
      for I in NFT.Minter'Range loop
         if Idx <= Input'Last then
            NFT.Minter (I) := Input (Idx);
            Idx := Idx + 1;
         end if;
      end loop;

      --  Category
      if Idx <= Input'Last then
         if Natural (Input (Idx)) <= Build_Category'Pos (Build_Category'Last)
         then
            NFT.Category := Build_Category'Val (Natural (Input (Idx)));
         end if;
         Idx := Idx + 1;
      end if;

      --  Commit hash
      for I in NFT.Commit_Hash'Range loop
         if Idx <= Input'Last then
            NFT.Commit_Hash (I) := Input (Idx);
            Idx := Idx + 1;
         end if;
      end loop;

      --  GNATprove hash
      for I in NFT.GNATprove_Hash'Range loop
         if Idx <= Input'Last then
            NFT.GNATprove_Hash (I) := Input (Idx);
            Idx := Idx + 1;
         end if;
      end loop;

      Read_U32 (NFT.Lines_Added);
      Read_U32 (NFT.Lines_Removed);
      Read_U32 (NFT.VCs_Proven);

      Read_U64 (NFT.Minted_At);

      --  Verified
      if Idx <= Input'Last then
         NFT.Verified := Input (Idx) /= 0;
         Idx := Idx + 1;
      end if;

      Success := True;
   end Deserialize_NFT;

   procedure Serialize_Collection_State (
      State          : Collection_State;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
      Idx : Natural := Output'First;

      procedure Write_U64 (Val : Unsigned_64) is
         V : Unsigned_64 := Val;
      begin
         for J in 0 .. 7 loop
            if Idx <= Output'Last then
               Output (Idx) := Unsigned_8 (V mod 256);
               V := V / 256;
               Idx := Idx + 1;
            end if;
         end loop;
      end Write_U64;

   begin
      Output := (others => 0);

      Write_U64 (State.Total_Minted);
      Write_U64 (State.Total_Lines);
      Write_U64 (State.Total_VCs);

      --  Builder address
      for I in State.Builder_Address'Range loop
         if Idx <= Output'Last then
            Output (Idx) := State.Builder_Address (I);
            Idx := Idx + 1;
         end if;
      end loop;

      Write_U64 (State.Genesis_Block);
      Write_U64 (State.Current_Block);

      Length := Idx - Output'First;
   end Serialize_Collection_State;

   procedure Deserialize_Collection_State (
      Input          : Byte_Array;
      State          : out Collection_State;
      Success        : out Boolean
   ) is
      Idx : Natural := Input'First;

      procedure Read_U64 (Val : out Unsigned_64) is
      begin
         Val := 0;
         for J in 0 .. 7 loop
            if Idx <= Input'Last then
               Val := Val + Unsigned_64 (Input (Idx)) * (256 ** J);
               Idx := Idx + 1;
            end if;
         end loop;
      end Read_U64;

   begin
      Init_Collection (State, (0 .. 31 => 0), 0);
      Success := False;

      if Input'Length < 72 then
         return;
      end if;

      Read_U64 (State.Total_Minted);
      Read_U64 (State.Total_Lines);
      Read_U64 (State.Total_VCs);

      --  Builder address
      for I in State.Builder_Address'Range loop
         if Idx <= Input'Last then
            State.Builder_Address (I) := Input (Idx);
            Idx := Idx + 1;
         end if;
      end loop;

      Read_U64 (State.Genesis_Block);
      Read_U64 (State.Current_Block);

      Success := True;
   end Deserialize_Collection_State;

   ---------------------------------------------------------------------------
   --  Zeroization
   ---------------------------------------------------------------------------

   procedure Zeroize_Collection_State (State : out Collection_State) is
   begin
      State.Total_Minted := 0;
      State.Total_Lines := 0;
      State.Total_VCs := 0;

      for C in Build_Category loop
         State.Category_Counts (C) := 0;
      end loop;

      State.Builder_Address := (others => 0);
      State.Genesis_Block := 0;
      State.Current_Block := 0;
   end Zeroize_Collection_State;

   procedure Zeroize_NFT (NFT : out Proof_Of_Build_NFT) is
   begin
      NFT.Token_ID := 0;
      NFT.Minter := (others => 0);
      NFT.Category := Core_Cryptography;
      NFT.Description_Hash := (others => 0);
      NFT.Commit_Hash := (others => 0);
      NFT.GNATprove_Hash := (others => 0);
      NFT.Lines_Added := 0;
      NFT.Lines_Removed := 0;
      NFT.VCs_Proven := 0;
      NFT.Test_Coverage := 0;
      NFT.IPFS_Report := (others => 0);
      NFT.Github_PR_Hash := (others => 0);
      NFT.Minted_At := 0;
      NFT.Timestamp := 0;
      NFT.Verified := False;
      NFT.Verifier_PK := (others => 0);
      NFT.Verification_Sig := (others => 0);
   end Zeroize_NFT;

   procedure Zeroize_Weekly_Update (NFT : out Weekly_Update_NFT) is
   begin
      NFT.Token_ID := 0;
      NFT.Week_Number := 0;
      NFT.Year := 0;
      NFT.Commits_Count := 0;
      NFT.Lines_Added := 0;
      NFT.VCs_Proven := 0;
      NFT.Milestone_Progress := 0;
      NFT.Summary_Hash := (others => 0);
      NFT.IPFS_Link := (others => 0);
      NFT.Video_Link_Hash := (others => 0);
      NFT.Minted_At := 0;
      NFT.Week_Start := 0;
      NFT.Week_End := 0;
   end Zeroize_Weekly_Update;

end Anubis_Proof_Of_Build;
