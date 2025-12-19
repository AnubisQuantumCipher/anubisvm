-------------------------------------------------------------------------------
--  P2P_Discovery: Peer Discovery Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body P2P_Discovery is

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Find bootstrap node by Node ID
   procedure Find_Bootstrap (
      State   : in     Discovery_State;
      Node_ID : in     Hash256;
      Index   : out    Bootstrap_Index;
      Found   : out    Boolean
   ) with
      Global => null,
      Pre    => State.Is_Initialized
   is
      Match : Boolean;
   begin
      Found := False;
      Index := 0;

      for I in Bootstrap_Index loop
         pragma Loop_Invariant (not Found);
         if State.Bootstrap_Nodes (I).Is_Valid then
            Match := True;
            for J in Node_ID'Range loop
               pragma Loop_Invariant (J >= Node_ID'First and J <= Node_ID'Last);
               if State.Bootstrap_Nodes (I).Node_ID (J) /= Node_ID (J) then
                  Match := False;
                  exit;
               end if;
            end loop;
            if Match then
               Index := I;
               Found := True;
               return;
            end if;
         end if;
      end loop;
   end Find_Bootstrap;

   --  Find discovered peer by Node ID
   procedure Find_Discovered (
      State   : in     Discovery_State;
      Node_ID : in     Hash256;
      Index   : out    Discovery_Index;
      Found   : out    Boolean
   ) with
      Global => null,
      Pre    => State.Is_Initialized
   is
      Match : Boolean;
   begin
      Found := False;
      Index := 0;

      for I in Discovery_Index loop
         pragma Loop_Invariant (not Found);
         if State.Discovered_Peers (I).Is_Valid then
            Match := True;
            for J in Node_ID'Range loop
               pragma Loop_Invariant (J >= Node_ID'First and J <= Node_ID'Last);
               if State.Discovered_Peers (I).Node_ID (J) /= Node_ID (J) then
                  Match := False;
                  exit;
               end if;
            end loop;
            if Match then
               Index := I;
               Found := True;
               return;
            end if;
         end if;
      end loop;
   end Find_Discovered;

   --  Find empty slot in discovered peers
   procedure Find_Empty_Slot (
      State : in     Discovery_State;
      Index : out    Discovery_Index;
      Found : out    Boolean
   ) with
      Global => null,
      Pre    => State.Is_Initialized
   is
   begin
      Found := False;
      Index := 0;

      for I in Discovery_Index loop
         pragma Loop_Invariant (not Found);
         if not State.Discovered_Peers (I).Is_Valid then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Empty_Slot;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      State : out Discovery_State
   ) is
      Empty_Bootstrap : constant Bootstrap_Node := (
         Is_Valid => False,
         Address  => (
            Is_Valid => False,
            Address  => (others => ' '),
            Addr_Len => 0,
            Port     => 0
         ),
         Node_ID  => (others => 0)
      );
      Empty_Peer : constant Discovered_Peer := (
         Is_Valid      => False,
         Address       => (
            Is_Valid => False,
            Address  => (others => ' '),
            Addr_Len => 0,
            Port     => 0
         ),
         Node_ID       => (others => 0),
         Discovered_At => 0,
         Last_Seen     => 0,
         Attempts      => 0,
         Connected     => False
      );
   begin
      --  Initialize bootstrap list
      for I in Bootstrap_Index loop
         State.Bootstrap_Nodes (I) := Empty_Bootstrap;
      end loop;
      State.Bootstrap_Count := 0;

      --  Initialize discovered peers
      for I in Discovery_Index loop
         State.Discovered_Peers (I) := Empty_Peer;
      end loop;
      State.Discovered_Count := 0;

      State.Last_Refresh := 0;
      State.Is_Initialized := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Bootstrap Management
   ---------------------------------------------------------------------------

   procedure Add_Bootstrap_Node (
      State   : in out Discovery_State;
      Address : in     Peer_Address;
      Node_ID : in     Hash256;
      Result  : out    Discovery_Result
   ) is
      Index : Bootstrap_Index;
      Found : Boolean;
   begin
      --  Check if already exists
      Find_Bootstrap (State, Node_ID, Index, Found);
      if Found then
         Result := Discovery_Already_Known;
         return;
      end if;

      --  Check capacity
      if State.Bootstrap_Count >= Max_Bootstrap_Nodes then
         Result := Discovery_Bootstrap_Full;
         return;
      end if;

      --  Add to list
      Index := Bootstrap_Index (State.Bootstrap_Count);
      State.Bootstrap_Nodes (Index) := (
         Is_Valid => True,
         Address  => Address,
         Node_ID  => Node_ID
      );
      State.Bootstrap_Count := State.Bootstrap_Count + 1;
      Result := Discovery_OK;
   end Add_Bootstrap_Node;

   procedure Get_Bootstrap_Nodes (
      State : in     Discovery_State;
      Nodes : out    Bootstrap_List;
      Count : out    Natural
   ) is
   begin
      Nodes := State.Bootstrap_Nodes;
      Count := State.Bootstrap_Count;
   end Get_Bootstrap_Nodes;

   ---------------------------------------------------------------------------
   --  Peer Discovery
   ---------------------------------------------------------------------------

   procedure Add_Discovered_Peer (
      State       : in out Discovery_State;
      Address     : in     Peer_Address;
      Node_ID     : in     Hash256;
      Current_Time : in     Unsigned_64;
      Result      : out    Discovery_Result
   ) is
      Index : Discovery_Index;
      Found : Boolean;
   begin
      --  Check if already known
      Find_Discovered (State, Node_ID, Index, Found);
      if Found then
         --  Update last seen
         State.Discovered_Peers (Index).Last_Seen := Current_Time;
         Result := Discovery_Already_Known;
         return;
      end if;

      --  Find empty slot
      Find_Empty_Slot (State, Index, Found);
      if not Found then
         Result := Discovery_Peer_Full;
         return;
      end if;

      --  Add peer
      State.Discovered_Peers (Index) := (
         Is_Valid      => True,
         Address       => Address,
         Node_ID       => Node_ID,
         Discovered_At => Current_Time,
         Last_Seen     => Current_Time,
         Attempts      => 0,
         Connected     => False
      );
      State.Discovered_Count := State.Discovered_Count + 1;
      Result := Discovery_OK;
   end Add_Discovered_Peer;

   procedure Mark_Peer_Connected (
      State   : in out Discovery_State;
      Node_ID : in     Hash256;
      Result  : out    Discovery_Result
   ) is
      Index : Discovery_Index;
      Found : Boolean;
   begin
      Find_Discovered (State, Node_ID, Index, Found);
      if not Found then
         Result := Discovery_Not_Found;
         return;
      end if;

      State.Discovered_Peers (Index).Connected := True;
      Result := Discovery_OK;
   end Mark_Peer_Connected;

   procedure Update_Peer_Seen (
      State       : in out Discovery_State;
      Node_ID     : in     Hash256;
      Current_Time : in     Unsigned_64;
      Result      : out    Discovery_Result
   ) is
      Index : Discovery_Index;
      Found : Boolean;
   begin
      Find_Discovered (State, Node_ID, Index, Found);
      if not Found then
         Result := Discovery_Not_Found;
         return;
      end if;

      State.Discovered_Peers (Index).Last_Seen := Current_Time;
      Result := Discovery_OK;
   end Update_Peer_Seen;

   procedure Get_Next_Peer (
      State       : in     Discovery_State;
      Current_Time : in     Unsigned_64;
      Address     : out    Peer_Address;
      Node_ID     : out    Hash256;
      Found       : out    Boolean
   ) is
      pragma Unreferenced (Current_Time);
      Best_Index : Discovery_Index := 0;
      Best_Score : Integer := -1000;
      Score : Integer;
   begin
      Found := False;
      Address := (
         Is_Valid => False,
         Address  => (others => ' '),
         Addr_Len => 0,
         Port     => 0
      );
      Node_ID := (others => 0);

      --  Find best peer to connect to
      --  Simple scoring: prefer not connected, fewer attempts
      for I in Discovery_Index loop
         pragma Loop_Invariant (Best_Score >= -1000);
         if State.Discovered_Peers (I).Is_Valid and not
            State.Discovered_Peers (I).Connected then
            Score := 100 - State.Discovered_Peers (I).Attempts * 10;
            if Score > Best_Score then
               Best_Score := Score;
               Best_Index := I;
               Found := True;
            end if;
         end if;
      end loop;

      if Found then
         Address := State.Discovered_Peers (Best_Index).Address;
         Node_ID := State.Discovered_Peers (Best_Index).Node_ID;
      end if;
   end Get_Next_Peer;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Is_Peer_Known (
      State   : Discovery_State;
      Node_ID : Hash256
   ) return Boolean is
      Index : Discovery_Index;
      Found : Boolean;
   begin
      Find_Discovered (State, Node_ID, Index, Found);
      return Found;
   end Is_Peer_Known;

   ---------------------------------------------------------------------------
   --  Maintenance
   ---------------------------------------------------------------------------

   procedure Remove_Stale_Peers (
      State        : in out Discovery_State;
      Current_Time : in     Unsigned_64;
      Timeout      : in     Unsigned_64;
      Removed      : out    Natural
   ) is
   begin
      Removed := 0;

      for I in Discovery_Index loop
         if State.Discovered_Peers (I).Is_Valid then
            if Current_Time - State.Discovered_Peers (I).Last_Seen >= Timeout then
               State.Discovered_Peers (I).Is_Valid := False;
               State.Discovered_Count := State.Discovered_Count - 1;
               Removed := Removed + 1;
            end if;
         end if;
      end loop;
   end Remove_Stale_Peers;

   procedure Refresh_Peers (
      State        : in out Discovery_State;
      Current_Time : in     Unsigned_64
   ) is
   begin
      State.Last_Refresh := Current_Time;
   end Refresh_Peers;

end P2P_Discovery;
