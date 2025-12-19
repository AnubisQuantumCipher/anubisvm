pragma SPARK_Mode (On);

package body Khepri_Coverage is

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Min (A, B : Natural) return Natural is
      (if A < B then A else B);

   function Percent (Part, Total : Natural) return Natural is
   begin
      if Total = 0 then
         return 0;
      end if;
      return (Part * 100) / Total;
   end Percent;

   ---------------------------------------------------------------------------
   --  Session Management
   ---------------------------------------------------------------------------

   function Create_Session return Coverage_Session is
   begin
      return (
         Has_Target     => False,
         Code_Size      => 0,
         Is_Collecting  => False,
         Line_Map       => (
            Points      => (others => (
               PC           => 0,
               Point_Type   => Coverage_Line,
               Hit_Count    => 0,
               State        => State_Not_Covered,
               Is_Branch    => False,
               Branch_Taken => False
            )),
            Point_Count    => 0,
            Total_Hits     => 0,
            Total_Lines    => 0,
            Total_Branches => 0
         ),
         Branch_Info    => (
            Branches     => (others => (
               PC           => 0,
               Target_True  => 0,
               Target_False => 0,
               Taken_True   => False,
               Taken_False  => False,
               Hit_Count_T  => 0,
               Hit_Count_F  => 0
            )),
            Branch_Count => 0,
            Covered_Both => 0,
            Covered_One  => 0,
            Not_Covered  => 0
         ),
         Path_Info      => (
            Paths       => (others => (
               Hash      => (others => 0),
               Hit_Count => 0,
               Length    => 0,
               Is_Hot    => False
            )),
            Path_Count  => 0,
            Total_Paths => 0
         ),
         Opcode_Hits    => (others => 0),
         Current_Trace  => (
            Branches => (others => 0),
            Length   => 0
         ),
         Stats          => (others => 0)
      );
   end Create_Session;

   procedure Set_Target (
      Session       : in Out Coverage_Session;
      Contract_Code : in     Byte_Array;
      Code_Size     : in     Natural;
      Success       : out    Boolean
   ) is
      pragma Unreferenced (Contract_Code);
   begin
      if Code_Size > Natural (Max_Coverage_Points) then
         Success := False;
         return;
      end if;

      Session.Code_Size := Code_Size;
      Session.Has_Target := True;
      Session.Line_Map.Total_Lines := Code_Size;

      --  Initialize coverage points for each byte offset
      for I in 0 .. Min (Code_Size, Natural (Max_Coverage_Points)) - 1 loop
         Session.Line_Map.Points (Coverage_Index (I)) := (
            PC           => I,
            Point_Type   => Coverage_Line,
            Hit_Count    => 0,
            State        => State_Not_Covered,
            Is_Branch    => False,
            Branch_Taken => False
         );
      end loop;
      Session.Line_Map.Point_Count := Min (Code_Size, Natural (Max_Coverage_Points));

      Success := True;
   end Set_Target;

   procedure Start_Collection (
      Session : in Out Coverage_Session
   ) is
   begin
      Session.Is_Collecting := True;
      Session.Current_Trace := (
         Branches => (others => 0),
         Length   => 0
      );
   end Start_Collection;

   procedure Stop_Collection (
      Session : in Out Coverage_Session
   ) is
   begin
      Session.Is_Collecting := False;

      --  Calculate final statistics
      declare
         Covered : Natural := 0;
         Hot     : Natural := 0;
         Cold    : Natural := 0;
      begin
         for I in 0 .. Session.Line_Map.Point_Count - 1 loop
            declare
               Idx : constant Coverage_Index := Coverage_Index (I);
            begin
               if Session.Line_Map.Points (Idx).Hit_Count > 0 then
                  Covered := Covered + 1;
                  if Session.Line_Map.Points (Idx).Hit_Count >= 100 then
                     Hot := Hot + 1;
                  elsif Session.Line_Map.Points (Idx).Hit_Count <= 5 then
                     Cold := Cold + 1;
                  end if;
               end if;
            end;
         end loop;

         Session.Stats.Total_Points := Session.Line_Map.Point_Count;
         Session.Stats.Covered_Points := Covered;
         Session.Stats.Hot_Points := Hot;
         Session.Stats.Cold_Points := Cold;
         Session.Stats.Line_Coverage := Percent (Covered, Session.Line_Map.Point_Count);
      end;

      --  Calculate branch coverage
      declare
         Covered_Both : Natural := 0;
      begin
         for I in 0 .. Session.Branch_Info.Branch_Count - 1 loop
            declare
               Idx : constant Branch_Index := Branch_Index (I);
            begin
               if Session.Branch_Info.Branches (Idx).Taken_True and
                  Session.Branch_Info.Branches (Idx).Taken_False
               then
                  Covered_Both := Covered_Both + 1;
               end if;
            end;
         end loop;

         Session.Branch_Info.Covered_Both := Covered_Both;
         Session.Stats.Branch_Coverage :=
            Percent (Covered_Both, Session.Branch_Info.Branch_Count);
      end;

      --  Calculate path coverage
      Session.Stats.Path_Coverage :=
         Percent (Session.Path_Info.Path_Count, Natural'Max (1, Session.Path_Info.Total_Paths));

      --  Calculate opcode coverage
      declare
         Opcode_Count : Natural := 0;
      begin
         for I in Opcode_Index'Range loop
            if Session.Opcode_Hits (I) > 0 then
               Opcode_Count := Opcode_Count + 1;
            end if;
         end loop;
         Session.Stats.Opcode_Coverage := Percent (Opcode_Count, 256);
      end;
   end Stop_Collection;

   procedure Reset_Coverage (
      Session : in Out Coverage_Session
   ) is
   begin
      --  Reset line coverage
      for I in 0 .. Session.Line_Map.Point_Count - 1 loop
         Session.Line_Map.Points (Coverage_Index (I)).Hit_Count := 0;
         Session.Line_Map.Points (Coverage_Index (I)).State := State_Not_Covered;
      end loop;
      Session.Line_Map.Total_Hits := 0;

      --  Reset branch coverage
      for I in 0 .. Session.Branch_Info.Branch_Count - 1 loop
         Session.Branch_Info.Branches (Branch_Index (I)).Taken_True := False;
         Session.Branch_Info.Branches (Branch_Index (I)).Taken_False := False;
         Session.Branch_Info.Branches (Branch_Index (I)).Hit_Count_T := 0;
         Session.Branch_Info.Branches (Branch_Index (I)).Hit_Count_F := 0;
      end loop;
      Session.Branch_Info.Covered_Both := 0;
      Session.Branch_Info.Covered_One := 0;
      Session.Branch_Info.Not_Covered := Session.Branch_Info.Branch_Count;

      --  Reset path coverage
      Session.Path_Info.Path_Count := 0;

      --  Reset opcode coverage
      Session.Opcode_Hits := (others => 0);

      --  Reset stats
      Session.Stats := (others => 0);

      --  Reset trace
      Session.Current_Trace := (
         Branches => (others => 0),
         Length   => 0
      );
   end Reset_Coverage;

   ---------------------------------------------------------------------------
   --  Coverage Recording
   ---------------------------------------------------------------------------

   procedure Record_Instruction (
      Session : in Out Coverage_Session;
      PC      : in     Natural
   ) is
   begin
      if not Session.Is_Collecting then
         return;
      end if;

      if PC >= Session.Line_Map.Point_Count then
         return;
      end if;

      declare
         Idx : constant Coverage_Index := Coverage_Index (PC);
         Old_Count : constant Natural := Session.Line_Map.Points (Idx).Hit_Count;
      begin
         if Old_Count < Natural'Last then
            Session.Line_Map.Points (Idx).Hit_Count := Old_Count + 1;
         end if;

         --  Update state
         if Old_Count = 0 then
            Session.Line_Map.Points (Idx).State := State_Covered;
         elsif Session.Line_Map.Points (Idx).Hit_Count >= 100 then
            Session.Line_Map.Points (Idx).State := State_Hot;
         end if;

         Session.Line_Map.Total_Hits := Session.Line_Map.Total_Hits + 1;
      end;
   end Record_Instruction;

   procedure Record_Branch (
      Session    : in Out Coverage_Session;
      PC         : in     Natural;
      Taken      : in     Boolean;
      Target     : in     Natural
   ) is
      Found : Boolean := False;
      Idx   : Branch_Index := 0;
   begin
      if not Session.Is_Collecting then
         return;
      end if;

      --  Find existing branch or add new one
      for I in 0 .. Session.Branch_Info.Branch_Count - 1 loop
         if Session.Branch_Info.Branches (Branch_Index (I)).PC = PC then
            Found := True;
            Idx := Branch_Index (I);
            exit;
         end if;
      end loop;

      if not Found then
         if Session.Branch_Info.Branch_Count >= Natural (Max_Branches) then
            return;
         end if;
         Idx := Branch_Index (Session.Branch_Info.Branch_Count);
         Session.Branch_Info.Branches (Idx) := (
            PC           => PC,
            Target_True  => Target,
            Target_False => 0,
            Taken_True   => False,
            Taken_False  => False,
            Hit_Count_T  => 0,
            Hit_Count_F  => 0
         );
         Session.Branch_Info.Branch_Count := Session.Branch_Info.Branch_Count + 1;
      end if;

      --  Update branch info
      if Taken then
         Session.Branch_Info.Branches (Idx).Taken_True := True;
         Session.Branch_Info.Branches (Idx).Target_True := Target;
         if Session.Branch_Info.Branches (Idx).Hit_Count_T < Natural'Last then
            Session.Branch_Info.Branches (Idx).Hit_Count_T :=
               Session.Branch_Info.Branches (Idx).Hit_Count_T + 1;
         end if;
      else
         Session.Branch_Info.Branches (Idx).Taken_False := True;
         Session.Branch_Info.Branches (Idx).Target_False := Target;
         if Session.Branch_Info.Branches (Idx).Hit_Count_F < Natural'Last then
            Session.Branch_Info.Branches (Idx).Hit_Count_F :=
               Session.Branch_Info.Branches (Idx).Hit_Count_F + 1;
         end if;
      end if;

      --  Add to current trace
      if Session.Current_Trace.Length < Natural (Max_Trace_Length) then
         Session.Current_Trace.Branches (Trace_Index (Session.Current_Trace.Length)) := PC;
         Session.Current_Trace.Length := Session.Current_Trace.Length + 1;
      end if;
   end Record_Branch;

   procedure Record_Path (
      Session : in Out Coverage_Session;
      Hash    : in     Path_Hash
   ) is
      Found : Boolean := False;
   begin
      if not Session.Is_Collecting then
         return;
      end if;

      --  Check if path already recorded
      for I in 0 .. Session.Path_Info.Path_Count - 1 loop
         if Session.Path_Info.Paths (Path_Index (I)).Hash = Hash then
            Found := True;
            if Session.Path_Info.Paths (Path_Index (I)).Hit_Count < Natural'Last then
               Session.Path_Info.Paths (Path_Index (I)).Hit_Count :=
                  Session.Path_Info.Paths (Path_Index (I)).Hit_Count + 1;
            end if;
            if Session.Path_Info.Paths (Path_Index (I)).Hit_Count >= 100 then
               Session.Path_Info.Paths (Path_Index (I)).Is_Hot := True;
            end if;
            exit;
         end if;
      end loop;

      --  Add new path
      if not Found then
         if Session.Path_Info.Path_Count >= Natural (Max_Paths) then
            return;
         end if;
         declare
            Idx : constant Path_Index := Path_Index (Session.Path_Info.Path_Count);
         begin
            Session.Path_Info.Paths (Idx) := (
               Hash      => Hash,
               Hit_Count => 1,
               Length    => Session.Current_Trace.Length,
               Is_Hot    => False
            );
            Session.Path_Info.Path_Count := Session.Path_Info.Path_Count + 1;
         end;
      end if;
   end Record_Path;

   procedure Record_Opcode (
      Session : in Out Coverage_Session;
      Opcode  : in     Byte
   ) is
      Idx : constant Opcode_Index := Opcode_Index (Opcode);
   begin
      if not Session.Is_Collecting then
         return;
      end if;

      if Session.Opcode_Hits (Idx) < Natural'Last then
         Session.Opcode_Hits (Idx) := Session.Opcode_Hits (Idx) + 1;
      end if;
   end Record_Opcode;

   ---------------------------------------------------------------------------
   --  Coverage Queries
   ---------------------------------------------------------------------------

   function Get_Stats (
      Session : Coverage_Session
   ) return Coverage_Stats is
   begin
      return Session.Stats;
   end Get_Stats;

   procedure Get_Line_Coverage (
      Session  : in  Coverage_Session;
      Map      : out Coverage_Map;
      Success  : out Boolean
   ) is
   begin
      Map := Session.Line_Map;
      Success := Session.Has_Target;
   end Get_Line_Coverage;

   procedure Get_Branch_Coverage (
      Session : in  Coverage_Session;
      Map     : out Branch_Map;
      Success : out Boolean
   ) is
   begin
      Map := Session.Branch_Info;
      Success := Session.Has_Target;
   end Get_Branch_Coverage;

   procedure Get_Path_Coverage (
      Session : in  Coverage_Session;
      Map     : out Path_Map;
      Success : out Boolean
   ) is
   begin
      Map := Session.Path_Info;
      Success := Session.Has_Target;
   end Get_Path_Coverage;

   function Is_Covered (
      Session : Coverage_Session;
      PC      : Natural
   ) return Boolean is
   begin
      if PC >= Session.Line_Map.Point_Count then
         return False;
      end if;
      return Session.Line_Map.Points (Coverage_Index (PC)).Hit_Count > 0;
   end Is_Covered;

   function Get_Hit_Count (
      Session : Coverage_Session;
      PC      : Natural
   ) return Natural is
   begin
      if PC >= Session.Line_Map.Point_Count then
         return 0;
      end if;
      return Session.Line_Map.Points (Coverage_Index (PC)).Hit_Count;
   end Get_Hit_Count;

   ---------------------------------------------------------------------------
   --  Coverage Analysis
   ---------------------------------------------------------------------------

   procedure Find_Uncovered (
      Session   : in  Coverage_Session;
      Uncovered : out Coverage_Point_Array;
      Count     : out Natural
   ) is
   begin
      Uncovered := (others => (
         PC           => 0,
         Point_Type   => Coverage_Line,
         Hit_Count    => 0,
         State        => State_Not_Covered,
         Is_Branch    => False,
         Branch_Taken => False
      ));
      Count := 0;

      for I in 0 .. Session.Line_Map.Point_Count - 1 loop
         declare
            Idx : constant Coverage_Index := Coverage_Index (I);
         begin
            if Session.Line_Map.Points (Idx).Hit_Count = 0 then
               if Count < Natural (Max_Coverage_Points) then
                  Uncovered (Coverage_Index (Count)) := Session.Line_Map.Points (Idx);
                  Count := Count + 1;
               end if;
            end if;
         end;
      end loop;
   end Find_Uncovered;

   procedure Find_Hot_Spots (
      Session  : in  Coverage_Session;
      Hot      : out Coverage_Point_Array;
      Count    : out Natural;
      Min_Hits : in  Natural := 100
   ) is
   begin
      Hot := (others => (
         PC           => 0,
         Point_Type   => Coverage_Line,
         Hit_Count    => 0,
         State        => State_Not_Covered,
         Is_Branch    => False,
         Branch_Taken => False
      ));
      Count := 0;

      for I in 0 .. Session.Line_Map.Point_Count - 1 loop
         declare
            Idx : constant Coverage_Index := Coverage_Index (I);
         begin
            if Session.Line_Map.Points (Idx).Hit_Count >= Min_Hits then
               if Count < Natural (Max_Coverage_Points) then
                  Hot (Coverage_Index (Count)) := Session.Line_Map.Points (Idx);
                  Count := Count + 1;
               end if;
            end if;
         end;
      end loop;
   end Find_Hot_Spots;

   procedure Find_Cold_Spots (
      Session  : in  Coverage_Session;
      Cold     : out Coverage_Point_Array;
      Count    : out Natural;
      Max_Hits : in  Natural := 5
   ) is
   begin
      Cold := (others => (
         PC           => 0,
         Point_Type   => Coverage_Line,
         Hit_Count    => 0,
         State        => State_Not_Covered,
         Is_Branch    => False,
         Branch_Taken => False
      ));
      Count := 0;

      for I in 0 .. Session.Line_Map.Point_Count - 1 loop
         declare
            Idx : constant Coverage_Index := Coverage_Index (I);
         begin
            if Session.Line_Map.Points (Idx).Hit_Count > 0 and
               Session.Line_Map.Points (Idx).Hit_Count <= Max_Hits
            then
               if Count < Natural (Max_Coverage_Points) then
                  Cold (Coverage_Index (Count)) := Session.Line_Map.Points (Idx);
                  Count := Count + 1;
               end if;
            end if;
         end;
      end loop;
   end Find_Cold_Spots;

   function Coverage_Delta (
      Before : Coverage_Stats;
      After  : Coverage_Stats
   ) return Coverage_Stats is
   begin
      return (
         Line_Coverage   => After.Line_Coverage - Before.Line_Coverage,
         Branch_Coverage => After.Branch_Coverage - Before.Branch_Coverage,
         Path_Coverage   => After.Path_Coverage - Before.Path_Coverage,
         Opcode_Coverage => After.Opcode_Coverage - Before.Opcode_Coverage,
         Total_Points    => After.Total_Points,
         Covered_Points  => After.Covered_Points - Before.Covered_Points,
         Hot_Points      => After.Hot_Points - Before.Hot_Points,
         Cold_Points     => After.Cold_Points - Before.Cold_Points
      );
   end Coverage_Delta;

   ---------------------------------------------------------------------------
   --  Coverage-Guided Feedback
   ---------------------------------------------------------------------------

   function Calculate_Feedback (
      Session : Coverage_Session;
      Prev_Stats : Coverage_Stats
   ) return Coverage_Feedback is
      Current : constant Coverage_Stats := Session.Stats;
      New_Cov : constant Boolean :=
         Current.Covered_Points > Prev_Stats.Covered_Points;
      New_Branches : constant Natural :=
         (if Current.Branch_Coverage > Prev_Stats.Branch_Coverage
          then Current.Branch_Coverage - Prev_Stats.Branch_Coverage
          else 0);
      New_Paths : constant Natural :=
         (if Session.Path_Info.Path_Count > 0 then 1 else 0);
      Score : Natural;
   begin
      --  Calculate interest score
      Score := 0;
      if New_Cov then
         Score := Score + 30;
      end if;
      Score := Score + New_Branches * 20;
      Score := Score + New_Paths * 50;

      if Score > 100 then
         Score := 100;
      end if;

      return (
         New_Coverage   => New_Cov,
         New_Branches   => New_Branches,
         New_Paths      => New_Paths,
         Interest_Score => Score,
         Should_Save    => Score >= 20
      );
   end Calculate_Feedback;

   procedure Merge_Coverage (
      Target : in Out Coverage_Session;
      Source : in     Coverage_Session
   ) is
   begin
      --  Merge line coverage
      for I in 0 .. Min (Target.Line_Map.Point_Count, Source.Line_Map.Point_Count) - 1 loop
         declare
            Idx : constant Coverage_Index := Coverage_Index (I);
         begin
            if Source.Line_Map.Points (Idx).Hit_Count > 0 then
               Target.Line_Map.Points (Idx).Hit_Count :=
                  Target.Line_Map.Points (Idx).Hit_Count +
                  Source.Line_Map.Points (Idx).Hit_Count;
               if Target.Line_Map.Points (Idx).State = State_Not_Covered then
                  Target.Line_Map.Points (Idx).State := State_Covered;
               end if;
            end if;
         end;
      end loop;

      --  Merge branch coverage
      for I in 0 .. Source.Branch_Info.Branch_Count - 1 loop
         declare
            Src_Idx : constant Branch_Index := Branch_Index (I);
            Found : Boolean := False;
         begin
            --  Find matching branch in target
            for J in 0 .. Target.Branch_Info.Branch_Count - 1 loop
               declare
                  Tgt_Idx : constant Branch_Index := Branch_Index (J);
               begin
                  if Target.Branch_Info.Branches (Tgt_Idx).PC =
                     Source.Branch_Info.Branches (Src_Idx).PC
                  then
                     Found := True;
                     if Source.Branch_Info.Branches (Src_Idx).Taken_True then
                        Target.Branch_Info.Branches (Tgt_Idx).Taken_True := True;
                     end if;
                     if Source.Branch_Info.Branches (Src_Idx).Taken_False then
                        Target.Branch_Info.Branches (Tgt_Idx).Taken_False := True;
                     end if;
                     exit;
                  end if;
               end;
            end loop;

            --  Add new branch if not found
            if not Found and Target.Branch_Info.Branch_Count < Natural (Max_Branches) then
               declare
                  New_Idx : constant Branch_Index :=
                     Branch_Index (Target.Branch_Info.Branch_Count);
               begin
                  Target.Branch_Info.Branches (New_Idx) :=
                     Source.Branch_Info.Branches (Src_Idx);
                  Target.Branch_Info.Branch_Count :=
                     Target.Branch_Info.Branch_Count + 1;
               end;
            end if;
         end;
      end loop;

      --  Merge opcode coverage
      for I in Opcode_Index'Range loop
         if Source.Opcode_Hits (I) > 0 then
            if Target.Opcode_Hits (I) <= Natural'Last - Source.Opcode_Hits (I) then
               Target.Opcode_Hits (I) := Target.Opcode_Hits (I) + Source.Opcode_Hits (I);
            else
               Target.Opcode_Hits (I) := Natural'Last;
            end if;
         end if;
      end loop;
   end Merge_Coverage;

   ---------------------------------------------------------------------------
   --  Opcode Coverage
   ---------------------------------------------------------------------------

   function Get_Opcode_Coverage (
      Session : Coverage_Session
   ) return Opcode_Coverage is
      Count : Natural := 0;
   begin
      for I in Opcode_Index'Range loop
         if Session.Opcode_Hits (I) > 0 then
            Count := Count + 1;
         end if;
      end loop;

      return (
         Hits        => Session.Opcode_Hits,
         Covered     => Count,
         Total_Valid => 256  --  All byte values possible
      );
   end Get_Opcode_Coverage;

   ---------------------------------------------------------------------------
   --  Report Generation
   ---------------------------------------------------------------------------

   procedure Generate_Report (
      Session : in     Coverage_Session;
      Format  : in     Coverage_Report_Format;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) is
      pragma Unreferenced (Format);
   begin
      Output := (others => 0);

      declare
         Header : constant String := "KHEPRI Coverage Report" & ASCII.LF &
            "======================" & ASCII.LF &
            "Line Coverage: " & ASCII.LF;
         Pos : Natural := 0;

         --  Helper to append string to output
         procedure Append_String (S : String) is
         begin
            for I in S'Range loop
               if Pos < Output'Length then
                  Output (Output'First + Pos) := Byte (Character'Pos (S (I)));
                  Pos := Pos + 1;
               end if;
            end loop;
         end Append_String;

         --  Helper to append natural as string
         procedure Append_Natural (N : Natural) is
            Str : constant String := Natural'Image (N);
         begin
            Append_String (Str);
         end Append_Natural;

      begin
         --  Generate report with actual statistics
         --  In a full implementation, this would format based on Format parameter
         --  and generate detailed coverage data per function/line

         Append_String (Header);
         Append_String ("  Covered Points: ");
         Append_Natural (Session.Stats.Covered_Points);
         Append_String (" / ");
         Append_Natural (Session.Stats.Total_Points);
         Append_String (ASCII.LF);

         Append_String ("  Line Coverage:   ");
         Append_Natural (Session.Stats.Line_Coverage);
         Append_String ("%" & ASCII.LF);

         Append_String ("  Branch Coverage: ");
         Append_Natural (Session.Stats.Branch_Coverage);
         Append_String ("%" & ASCII.LF);

         Append_String ("  Path Coverage:   ");
         Append_Natural (Session.Stats.Path_Coverage);
         Append_String ("%" & ASCII.LF);

         Append_String ("  Opcode Coverage: ");
         Append_Natural (Session.Stats.Opcode_Coverage);
         Append_String ("%" & ASCII.LF);

         Append_String (ASCII.LF & "Hot Spots: ");
         Append_Natural (Session.Stats.Hot_Points);
         Append_String (ASCII.LF);

         Append_String ("Cold Spots: ");
         Append_Natural (Session.Stats.Cold_Points);
         Append_String (ASCII.LF);

         Size := Pos;
      end;

      Success := Session.Has_Target;
   end Generate_Report;

   function Summary_Line (
      Session : Coverage_Session
   ) return String is
   begin
      if Session.Stats.Line_Coverage >= 80 then
         return "Coverage: Good (" & Natural'Image (Session.Stats.Line_Coverage) & "%)";
      elsif Session.Stats.Line_Coverage >= 50 then
         return "Coverage: Moderate (" & Natural'Image (Session.Stats.Line_Coverage) & "%)";
      else
         return "Coverage: Low (" & Natural'Image (Session.Stats.Line_Coverage) & "%)";
      end if;
   end Summary_Line;

end Khepri_Coverage;
