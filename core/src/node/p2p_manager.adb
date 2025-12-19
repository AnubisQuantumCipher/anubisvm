-------------------------------------------------------------------------------
--  P2P_Manager: Comprehensive P2P Network Management Implementation
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);  --  Implementation uses non-SPARK network operations

with Ada.Text_IO;
with P2P_Sockets;

package body P2P_Manager is

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Hash node ID for discovery (simple XOR hash)
   function Hash_Node_ID (Node_ID : Hash256) return Unsigned_32 is
      Result : Unsigned_32 := 0;
   begin
      for I in 0 .. 7 loop
         Result := Result xor Unsigned_32 (Node_ID (I * 4)) or
                   Shift_Left (Unsigned_32 (Node_ID (I * 4 + 1)), 8) or
                   Shift_Left (Unsigned_32 (Node_ID (I * 4 + 2)), 16) or
                   Shift_Left (Unsigned_32 (Node_ID (I * 4 + 3)), 24);
      end loop;
      return Result;
   end Hash_Node_ID;

   ---------------------------------------------------------------------------
   --  Initialization and Lifecycle
   ---------------------------------------------------------------------------

   procedure Initialize (
      Mgr       : out Manager_State;
      Node_ID   : in  Hash256;
      Node_Key  : in  Anubis_MLDSA_Types.Public_Key;
      Node_SK   : in  Anubis_MLDSA_Types.ML_DSA_Secret_Key;
      Chain_ID  : in  U256;
      Port      : in  Unsigned_16
   ) is
   begin
      --  Initialize network layer
      P2P_Network.Initialize (
         Net      => Mgr.Net,
         Node_ID  => Node_ID,
         Node_Key => Node_Key,
         Chain_ID => Chain_ID,
         Port     => Port
      );

      --  Initialize discovery layer
      P2P_Discovery.Initialize (Mgr.Discovery);

      --  Store node secret key for signing
      Mgr.Node_SK := Node_SK;

      Mgr.Is_Initialized := True;
      Mgr.Is_Running := False;

      Ada.Text_IO.Put_Line ("P2P_Manager: Initialized on port " &
         Unsigned_16'Image (Port));
   end Initialize;

   procedure Start (
      Mgr    : in out Manager_State;
      Result : out    Manager_Result
   ) is
      Net_Result : Network_Result;
   begin
      if Mgr.Is_Running then
         Result := Manager_Already_Running;
         return;
      end if;

      --  Start network layer
      P2P_Network.Start (Mgr.Net, Net_Result);
      if Net_Result /= Network_OK then
         Result := Manager_Network_Error;
         return;
      end if;

      Mgr.Is_Running := True;
      Result := Manager_OK;

      Ada.Text_IO.Put_Line ("P2P_Manager: Network started");
   end Start;

   procedure Stop (
      Mgr : in out Manager_State
   ) is
   begin
      if not Mgr.Is_Running then
         return;
      end if;

      --  Stop network layer
      P2P_Network.Stop (Mgr.Net);

      Mgr.Is_Running := False;

      Ada.Text_IO.Put_Line ("P2P_Manager: Network stopped");
   end Stop;

   procedure Shutdown (
      Mgr : in out Manager_State
   ) is
   begin
      --  Stop if running
      if Mgr.Is_Running then
         Stop (Mgr);
      end if;

      Mgr.Is_Initialized := False;

      Ada.Text_IO.Put_Line ("P2P_Manager: Shutdown complete");
   end Shutdown;

   ---------------------------------------------------------------------------
   --  Bootstrap Configuration
   ---------------------------------------------------------------------------

   procedure Add_Bootstrap (
      Mgr        : in out Manager_State;
      Address_Str : in     String;
      Port       : in     Unsigned_16;
      Result     : out    Manager_Result
   ) is
      Address : Peer_Address;
      Success : Boolean;
      Node_ID : Hash256;
      Disc_Result : Discovery_Result;
   begin
      --  Parse address
      P2P_Sockets.Parse_Address (
         Addr_Str => Address_Str,
         Port     => Port,
         Address  => Address,
         Success  => Success
      );

      if not Success then
         Result := Manager_Discovery_Error;
         return;
      end if;

      --  Generate deterministic Node_ID from address (hash of address)
      declare
         Hash : Unsigned_32 := 0;
      begin
         for I in 1 .. Address.Addr_Len loop
            Hash := Hash xor Unsigned_32 (Character'Pos (Address.Address (I)));
            Hash := Hash * 16777619;  --  FNV-1a prime
         end loop;
         --  Spread hash across Node_ID
         for I in 0 .. 7 loop
            Node_ID (I * 4) := Byte (Shift_Right (Hash, 0) and 16#FF#);
            Node_ID (I * 4 + 1) := Byte (Shift_Right (Hash, 8) and 16#FF#);
            Node_ID (I * 4 + 2) := Byte (Shift_Right (Hash, 16) and 16#FF#);
            Node_ID (I * 4 + 3) := Byte (Shift_Right (Hash, 24) and 16#FF#);
         end loop;
      end;

      --  Add to discovery
      P2P_Discovery.Add_Bootstrap_Node (
         State   => Mgr.Discovery,
         Address => Address,
         Node_ID => Node_ID,
         Result  => Disc_Result
      );

      if Disc_Result /= Discovery_OK then
         Result := Manager_Discovery_Error;
         return;
      end if;

      Result := Manager_OK;

      Ada.Text_IO.Put_Line ("P2P_Manager: Added bootstrap node " &
         Address_Str & ":" & Unsigned_16'Image (Port));
   end Add_Bootstrap;

   ---------------------------------------------------------------------------
   --  Connection Management
   ---------------------------------------------------------------------------

   procedure Connect_To_Peer (
      Mgr        : in out Manager_State;
      Address_Str : in     String;
      Port       : in     Unsigned_16;
      Result     : out    Manager_Result
   ) is
      Address : Peer_Address;
      Success : Boolean;
      Net_Result : Network_Result;
   begin
      --  Parse address
      P2P_Sockets.Parse_Address (
         Addr_Str => Address_Str,
         Port     => Port,
         Address  => Address,
         Success  => Success
      );

      if not Success then
         Result := Manager_Peer_Error;
         return;
      end if;

      --  Connect via network layer
      P2P_Network.Connect_Peer (
         Net     => Mgr.Net,
         Address => Address,
         Result  => Net_Result
      );

      if Net_Result /= Network_OK then
         Result := Manager_Network_Error;
         return;
      end if;

      Result := Manager_OK;

      Ada.Text_IO.Put_Line ("P2P_Manager: Connecting to " &
         Address_Str & ":" & Unsigned_16'Image (Port));
   end Connect_To_Peer;

   procedure Disconnect_From_Peer (
      Mgr     : in out Manager_State;
      Node_ID : in     Hash256;
      Result  : out    Manager_Result
   ) is
      Net_Result : Network_Result;
   begin
      P2P_Network.Disconnect_Peer (
         Net     => Mgr.Net,
         Node_ID => Node_ID,
         Result  => Net_Result
      );

      if Net_Result = Network_OK or Net_Result = Network_Not_Found then
         Result := Manager_OK;
      else
         Result := Manager_Network_Error;
      end if;
   end Disconnect_From_Peer;

   ---------------------------------------------------------------------------
   --  Data Propagation
   ---------------------------------------------------------------------------

   procedure Gossip_Transaction (
      Mgr     : in out Manager_State;
      TX_Hash : in     Hash256;
      Sent    : out    Natural
   ) is
   begin
      P2P_Network.Gossip_TX (
         Net     => Mgr.Net,
         TX_Hash => TX_Hash,
         Sent    => Sent
      );

      if Sent > 0 then
         Ada.Text_IO.Put_Line ("P2P_Manager: Gossiped TX to " &
            Natural'Image (Sent) & " peers");
      end if;
   end Gossip_Transaction;

   procedure Gossip_Block (
      Mgr        : in out Manager_State;
      Block_Hash : in     Hash256;
      Height     : in     U256;
      Sent       : out    Natural
   ) is
   begin
      P2P_Network.Gossip_Block (
         Net        => Mgr.Net,
         Block_Hash => Block_Hash,
         Height     => Height,
         Sent       => Sent
      );

      if Sent > 0 then
         Ada.Text_IO.Put_Line ("P2P_Manager: Gossiped block to " &
            Natural'Image (Sent) & " peers");
      end if;
   end Gossip_Block;

   procedure Request_Transaction (
      Mgr     : in out Manager_State;
      TX_Hash : in     Hash256;
      Result  : out    Manager_Result
   ) is
      Net_Result : Network_Result;
   begin
      P2P_Network.Request_TX (
         Net     => Mgr.Net,
         TX_Hash => TX_Hash,
         Result  => Net_Result
      );

      if Net_Result = Network_OK then
         Result := Manager_OK;
      else
         Result := Manager_Network_Error;
      end if;
   end Request_Transaction;

   procedure Request_Block (
      Mgr        : in out Manager_State;
      Block_Hash : in     Hash256;
      Result     : out    Manager_Result
   ) is
      Net_Result : Network_Result;
   begin
      P2P_Network.Request_Block (
         Net        => Mgr.Net,
         Block_Hash => Block_Hash,
         Result     => Net_Result
      );

      if Net_Result = Network_OK then
         Result := Manager_OK;
      else
         Result := Manager_Network_Error;
      end if;
   end Request_Block;

   ---------------------------------------------------------------------------
   --  Event Processing
   ---------------------------------------------------------------------------

   procedure Process_Events (
      Mgr          : in out Manager_State;
      Current_Time : in     Unsigned_64
   ) is
      pragma Unreferenced (Current_Time);
   begin
      --  Process network events (incoming connections, messages, etc.)
      P2P_Network.Process_Events (Mgr.Net);

      --  In a real implementation, this would:
      --  1. Accept incoming connections
      --  2. Perform ML-KEM handshakes with new peers
      --  3. Read and decrypt incoming messages
      --  4. Route messages to appropriate handlers
      --  5. Handle peer disconnections
   end Process_Events;

   procedure Perform_Maintenance (
      Mgr          : in out Manager_State;
      Current_Time : in     Unsigned_64
   ) is
      Expired : Natural;
      Removed : Natural;
      Need_Discovery : Boolean;
      Next_Address : Peer_Address;
      Next_Node_ID : Hash256;
      Found : Boolean;
      Mgr_Result : Manager_Result;
   begin
      --  Send keepalive pings
      P2P_Network.Send_Keepalives (Mgr.Net, Current_Time);

      --  Expire stale peers
      P2P_Network.Expire_Stale_Peers (Mgr.Net, Current_Time, Expired);
      if Expired > 0 then
         Ada.Text_IO.Put_Line ("P2P_Manager: Expired " &
            Natural'Image (Expired) & " stale peers");
      end if;

      --  Clean up expired bans
      P2P_Network.Cleanup_Bans (Mgr.Net, Current_Time, Removed);

      --  Remove stale discovered peers
      P2P_Discovery.Remove_Stale_Peers (
         State        => Mgr.Discovery,
         Current_Time => Current_Time,
         Timeout      => 3600,  --  1 hour
         Removed      => Removed
      );

      --  Check if we need more peers
      Need_Discovery := P2P_Network.Get_Peer_Count (Mgr.Net) < Min_Peers_Threshold;

      --  Trigger peer discovery if needed
      if Need_Discovery or P2P_Discovery.Need_Refresh (Mgr.Discovery, Current_Time) then
         P2P_Discovery.Refresh_Peers (Mgr.Discovery, Current_Time);

         --  Try to connect to discovered peers
         P2P_Discovery.Get_Next_Peer (
            State       => Mgr.Discovery,
            Current_Time => Current_Time,
            Address     => Next_Address,
            Node_ID     => Next_Node_ID,
            Found       => Found
         );

         if Found and Next_Address.Is_Valid then
            P2P_Network.Connect_Peer (
               Net     => Mgr.Net,
               Address => Next_Address,
               Result  => Mgr_Result
            );
            pragma Unreferenced (Mgr_Result);
         end if;
      end if;
   end Perform_Maintenance;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Peer_Count (Mgr : Manager_State) return Natural is
   begin
      return P2P_Network.Get_Peer_Count (Mgr.Net);
   end Get_Peer_Count;

   function Get_Discovered_Count (Mgr : Manager_State) return Natural is
   begin
      return P2P_Discovery.Get_Discovered_Count (Mgr.Discovery);
   end Get_Discovered_Count;

   function Is_Peer_Connected (
      Mgr     : Manager_State;
      Node_ID : Hash256
   ) return Boolean is
   begin
      return P2P_Network.Is_Peer_Connected (Mgr.Net, Node_ID);
   end Is_Peer_Connected;

   function Get_Network_Stats (Mgr : Manager_State) return Network_Stats is
   begin
      return P2P_Network.Get_Stats (Mgr.Net);
   end Get_Network_Stats;

end P2P_Manager;
