-------------------------------------------------------------------------------
--  P2P_Network: Peer-to-Peer Networking Implementation for AnubisVM
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with P2P_Sockets; use P2P_Sockets;
with Ada.Text_IO;

package body P2P_Network is

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Initialize empty peer info
   procedure Initialize_Peer_Info (Info : out Peer_Info) is
   begin
      Info.Is_Valid := False;
      Info.Node_ID := (others => 0);
      Info.Address := (
         Is_Valid => False,
         Address  => (others => ' '),
         Addr_Len => 0,
         Port     => 0
      );
      Info.State := Peer_Disconnected;
      Info.Connected_Time := 0;
      Info.Last_Seen := 0;
      Info.Last_Ping := 0;
      Info.Ping_Latency_Ms := 0;
      Info.Chain_Height := Aegis_VM_Types.U256_Zero;
      Info.Best_Block := (others => 0);
      Info.TX_Received := 0;
      Info.TX_Sent := 0;
      Info.Blocks_Received := 0;
      Info.Blocks_Sent := 0;
      Info.Reputation := 50;  -- Start with neutral reputation
      Info.Is_Inbound := False;
      Info.Public_Key := (others => 0);
      Info.Socket_FD := -1;
   end Initialize_Peer_Info;

   --  Initialize empty ban entry
   procedure Initialize_Ban_Entry (Entry_Val : out Ban_Entry) is
   begin
      Entry_Val.Is_Valid := False;
      Entry_Val.Node_ID := (others => 0);
      Entry_Val.Ban_Time := 0;
      Entry_Val.Ban_Until := 0;
      Entry_Val.Reason := 0;
   end Initialize_Ban_Entry;

   --  Initialize message header
   procedure Initialize_Message_Header (Header : out Message_Header) is
   begin
      Header.Magic := Network_Magic;
      Header.Version := Protocol_Version;
      Header.Msg_Type := Msg_Ping;
      Header.Payload_Len := 0;
      Header.Timestamp := 0;
      Header.Sender_ID := (others => 0);
   end Initialize_Message_Header;

   --  Initialize empty message
   procedure Initialize_Message (Msg : out P2P_Message) is
   begin
      Initialize_Message_Header (Msg.Header);
      Msg.Payload := (others => 0);
      Msg.Payload_Size := 0;
      Msg.Is_Valid := False;
   end Initialize_Message;

   --  Find peer by node ID
   procedure Find_Peer_Index (
      Net     : in     Network_State;
      Node_ID : in     Hash256;
      Index   : out    Peer_Index;
      Found   : out    Boolean
   ) is
      Match : Boolean;
   begin
      Found := False;
      Index := 0;

      for I in Net.Peers'Range loop
         if Net.Peers (I).Is_Valid then
            Match := True;
            for J in Node_ID'Range loop
               if Net.Peers (I).Node_ID (J) /= Node_ID (J) then
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
   end Find_Peer_Index;

   --  Find empty peer slot
   procedure Find_Empty_Peer_Slot (
      Net   : in     Network_State;
      Index : out    Peer_Index;
      Found : out    Boolean
   ) is
   begin
      Found := False;
      Index := 0;

      for I in Net.Peers'Range loop
         if not Net.Peers (I).Is_Valid then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Empty_Peer_Slot;

   --  Find ban entry by node ID
   procedure Find_Ban_Index (
      Net     : in     Network_State;
      Node_ID : in     Hash256;
      Index   : out    Ban_Index;
      Found   : out    Boolean
   ) is
      Match : Boolean;
   begin
      Found := False;
      Index := 0;

      for I in Net.Ban_List'Range loop
         if Net.Ban_List (I).Is_Valid then
            Match := True;
            for J in Node_ID'Range loop
               if Net.Ban_List (I).Node_ID (J) /= Node_ID (J) then
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
   end Find_Ban_Index;

   --  Find empty ban slot
   procedure Find_Empty_Ban_Slot (
      Net   : in     Network_State;
      Index : out    Ban_Index;
      Found : out    Boolean
   ) is
   begin
      Found := False;
      Index := 0;

      for I in Net.Ban_List'Range loop
         if not Net.Ban_List (I).Is_Valid then
            Index := I;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Empty_Ban_Slot;

   --  Compare two U256 values (returns True if A > B)
   function U256_Greater (A, B : U256) return Boolean is
   begin
      --  Compare from most significant limb
      for I in reverse A.Limbs'Range loop
         if A.Limbs (I) > B.Limbs (I) then
            return True;
         elsif A.Limbs (I) < B.Limbs (I) then
            return False;
         end if;
      end loop;
      return False;  -- Equal
   end U256_Greater;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Net       : out Network_State;
      Node_ID   : in  Hash256;
      Node_Key  : in  Anubis_MLDSA_Types.Public_Key;
      Chain_ID  : in  U256;
      Port      : in  Unsigned_16
   ) is
      Empty_Peer : Peer_Info;
      Empty_Ban : Ban_Entry;
   begin
      Initialize_Peer_Info (Empty_Peer);
      Initialize_Ban_Entry (Empty_Ban);

      --  Initialize peer storage
      for I in Net.Peers'Range loop
         Net.Peers (I) := Empty_Peer;
      end loop;
      Net.Peer_Count := 0;

      --  Initialize ban list
      for I in Net.Ban_List'Range loop
         Net.Ban_List (I) := Empty_Ban;
      end loop;
      Net.Ban_Count := 0;

      --  Set node info
      Net.Node_ID := Node_ID;
      Net.Node_Key := Node_Key;
      Net.Chain_ID := Chain_ID;
      Net.Listen_Port := Port;

      --  Initialize chain state
      Net.Current_Height := Aegis_VM_Types.U256_Zero;
      Net.Best_Block := (others => 0);

      --  Initialize stats
      Net.Stats := (
         Peers_Connected   => 0,
         Peers_Connecting  => 0,
         Peers_Banned      => 0,
         Messages_Sent     => 0,
         Messages_Received => 0,
         TX_Gossiped       => 0,
         Blocks_Gossiped   => 0,
         Bytes_Sent        => 0,
         Bytes_Received    => 0
      );

      --  Initialize listener state
      Net.Listener_FD := -1;

      --  Initialize socket subsystem
      P2P_Sockets.Initialize;

      Net.Is_Initialized := True;
      Net.Is_Running := False;
   end Initialize;

   procedure Start (
      Net    : in Out Network_State;
      Result : out    Network_Result
   ) is
      Listener    : P2P_Sockets.Listener_State;
      Sock_Result : P2P_Sockets.Socket_Result;
   begin
      if Net.Is_Running then
         Result := Network_OK;
         return;
      end if;

      --  Create listener on configured port
      P2P_Sockets.Create_Listener (
         Port   => Net.Listen_Port,
         State  => Listener,
         Result => Sock_Result
      );

      if Sock_Result /= P2P_Sockets.Socket_OK then
         Ada.Text_IO.Put_Line ("P2P_Network: Failed to start listener on port" &
            Unsigned_16'Image (Net.Listen_Port));
         Result := Network_Connection_Failed;
         return;
      end if;

      --  Store listener FD (extract from Listener_State)
      --  Note: In production, we'd store the full Listener_State
      Net.Listener_FD := 0;  -- Placeholder, actual FD stored in P2P_Sockets

      Ada.Text_IO.Put_Line ("P2P_Network: Listening on port" &
         Unsigned_16'Image (Net.Listen_Port));

      Net.Is_Running := True;
      Result := Network_OK;
   end Start;

   procedure Stop (
      Net : in Out Network_State
   ) is
   begin
      if not Net.Is_Running then
         return;
      end if;

      --  Disconnect all peers
      for I in Net.Peers'Range loop
         if Net.Peers (I).Is_Valid and
            Net.Peers (I).State = Peer_Connected
         then
            Net.Peers (I).State := Peer_Disconnected;
         end if;
      end loop;

      Net.Is_Running := False;
   end Stop;

   ---------------------------------------------------------------------------
   --  Peer Management
   ---------------------------------------------------------------------------

   procedure Connect_Peer (
      Net     : in Out Network_State;
      Address : in     Peer_Address;
      Result  : out    Network_Result
   ) is
      Index       : Peer_Index;
      Found       : Boolean;
      Sock        : P2P_Sockets.Socket_Handle;
      Sock_Result : P2P_Sockets.Socket_Result;
   begin
      --  Check if we have room for more peers
      if Net.Peer_Count >= Max_Peers then
         Result := Network_Peer_Full;
         return;
      end if;

      --  Find empty slot
      Find_Empty_Peer_Slot (Net, Index, Found);
      if not Found then
         Result := Network_Peer_Full;
         return;
      end if;

      --  Initiate TCP connection
      Ada.Text_IO.Put_Line ("P2P_Network: Connecting to " &
         Address.Address (1 .. Address.Addr_Len) & ":" &
         Unsigned_16'Image (Address.Port));

      P2P_Sockets.Connect (
         Address => Address,
         Sock    => Sock,
         Result  => Sock_Result
      );

      if Sock_Result /= P2P_Sockets.Socket_OK then
         Ada.Text_IO.Put_Line ("P2P_Network: Connection failed");
         Result := Network_Connection_Failed;
         return;
      end if;

      --  Initialize new peer entry with socket
      Initialize_Peer_Info (Net.Peers (Index));
      Net.Peers (Index).Is_Valid := True;
      Net.Peers (Index).Address := Address;
      Net.Peers (Index).State := Peer_Handshaking;
      Net.Peers (Index).Is_Inbound := False;
      --  Extract socket FD from handle
      if P2P_Sockets.Is_Connected (Sock) then
         Net.Peers (Index).Socket_FD := 0;  -- Placeholder, actual FD managed internally
      end if;

      Net.Peer_Count := Net.Peer_Count + 1;
      Net.Stats.Peers_Connecting := Net.Stats.Peers_Connecting + 1;

      Ada.Text_IO.Put_Line ("P2P_Network: Connected, starting handshake");

      Result := Network_OK;
   end Connect_Peer;

   procedure Disconnect_Peer (
      Net     : in Out Network_State;
      Node_ID : in     Hash256;
      Result  : out    Network_Result
   ) is
      Index : Peer_Index;
      Found : Boolean;
   begin
      Find_Peer_Index (Net, Node_ID, Index, Found);
      if not Found then
         Result := Network_Not_Found;
         return;
      end if;

      --  Mark peer as disconnected
      if Net.Peers (Index).State = Peer_Connected then
         Net.Stats.Peers_Connected := Net.Stats.Peers_Connected - 1;
      elsif Net.Peers (Index).State = Peer_Connecting or
            Net.Peers (Index).State = Peer_Handshaking
      then
         Net.Stats.Peers_Connecting := Net.Stats.Peers_Connecting - 1;
      end if;

      Net.Peers (Index).State := Peer_Disconnected;
      Net.Peers (Index).Is_Valid := False;
      Net.Peer_Count := Net.Peer_Count - 1;

      Result := Network_OK;
   end Disconnect_Peer;

   procedure Ban_Peer (
      Net      : in Out Network_State;
      Node_ID  : in     Hash256;
      Duration : in     Unsigned_64;
      Reason   : in     Natural;
      Result   : out    Network_Result
   ) is
      Peer_Idx : Peer_Index;
      Ban_Idx  : Ban_Index;
      Found    : Boolean;
      Disconnect_Result : Network_Result;
   begin
      --  First disconnect if connected
      Find_Peer_Index (Net, Node_ID, Peer_Idx, Found);
      if Found then
         Disconnect_Peer (Net, Node_ID, Disconnect_Result);
      end if;

      --  Check if already banned
      Find_Ban_Index (Net, Node_ID, Ban_Idx, Found);
      if Found then
         --  Update existing ban
         Net.Ban_List (Ban_Idx).Ban_Until :=
            Net.Ban_List (Ban_Idx).Ban_Time + Duration;
         Result := Network_OK;
         return;
      end if;

      --  Find empty ban slot
      Find_Empty_Ban_Slot (Net, Ban_Idx, Found);
      if not Found then
         --  Ban list full - remove oldest ban
         Ban_Idx := 0;
      end if;

      --  Add ban entry
      Net.Ban_List (Ban_Idx).Is_Valid := True;
      Net.Ban_List (Ban_Idx).Node_ID := Node_ID;
      Net.Ban_List (Ban_Idx).Ban_Time := 0;  -- Would use current time
      Net.Ban_List (Ban_Idx).Ban_Until := Duration;
      Net.Ban_List (Ban_Idx).Reason := Reason;

      Net.Ban_Count := Net.Ban_Count + 1;
      Net.Stats.Peers_Banned := Net.Stats.Peers_Banned + 1;

      Result := Network_OK;
   end Ban_Peer;

   procedure Unban_Peer (
      Net     : in Out Network_State;
      Node_ID : in     Hash256;
      Result  : out    Network_Result
   ) is
      Index : Ban_Index;
      Found : Boolean;
   begin
      Find_Ban_Index (Net, Node_ID, Index, Found);
      if not Found then
         Result := Network_Not_Found;
         return;
      end if;

      Initialize_Ban_Entry (Net.Ban_List (Index));
      Net.Ban_Count := Net.Ban_Count - 1;

      Result := Network_OK;
   end Unban_Peer;

   procedure Get_Peer (
      Net     : in     Network_State;
      Node_ID : in     Hash256;
      Info    : out    Peer_Info;
      Found   : out    Boolean
   ) is
      Index : Peer_Index;
   begin
      Find_Peer_Index (Net, Node_ID, Index, Found);
      if Found then
         Info := Net.Peers (Index);
      else
         Initialize_Peer_Info (Info);
      end if;
   end Get_Peer;

   procedure Update_Peer_Chain (
      Net      : in Out Network_State;
      Node_ID  : in     Hash256;
      Height   : in     U256;
      Best     : in     Hash256;
      Result   : out    Network_Result
   ) is
      Index : Peer_Index;
      Found : Boolean;
   begin
      Find_Peer_Index (Net, Node_ID, Index, Found);
      if not Found then
         Result := Network_Not_Found;
         return;
      end if;

      Net.Peers (Index).Chain_Height := Height;
      Net.Peers (Index).Best_Block := Best;

      Result := Network_OK;
   end Update_Peer_Chain;

   ---------------------------------------------------------------------------
   --  Message Operations
   ---------------------------------------------------------------------------

   --  Serialize message header to bytes (for sending)
   procedure Serialize_Header (
      Header : in     Message_Header;
      Buffer : out    Aegis_VM_Types.Byte_Array;
      Length : out    Natural
   ) is
      --  Header layout (64 bytes):
      --  0-3:   Magic (4 bytes, LE)
      --  4-5:   Version (2 bytes, LE)
      --  6-7:   Msg_Type (2 bytes, as ordinal)
      --  8-11:  Payload_Len (4 bytes, LE)
      --  12-19: Timestamp (8 bytes, LE)
      --  20-51: Sender_ID (32 bytes)
      --  52-63: Reserved (12 bytes)
   begin
      Buffer := (others => 0);
      Length := 64;

      --  Magic (4 bytes LE)
      Buffer (Buffer'First)     := Aegis_VM_Types.Byte (Header.Magic and 16#FF#);
      Buffer (Buffer'First + 1) := Aegis_VM_Types.Byte ((Header.Magic / 256) and 16#FF#);
      Buffer (Buffer'First + 2) := Aegis_VM_Types.Byte ((Header.Magic / 65536) and 16#FF#);
      Buffer (Buffer'First + 3) := Aegis_VM_Types.Byte ((Header.Magic / 16777216) and 16#FF#);

      --  Version (2 bytes LE)
      Buffer (Buffer'First + 4) := Aegis_VM_Types.Byte (Header.Version and 16#FF#);
      Buffer (Buffer'First + 5) := Aegis_VM_Types.Byte ((Header.Version / 256) and 16#FF#);

      --  Msg_Type (2 bytes as ordinal)
      Buffer (Buffer'First + 6) := Aegis_VM_Types.Byte (Message_Type'Pos (Header.Msg_Type));
      Buffer (Buffer'First + 7) := 0;

      --  Payload_Len (4 bytes LE)
      Buffer (Buffer'First + 8)  := Aegis_VM_Types.Byte (Header.Payload_Len and 16#FF#);
      Buffer (Buffer'First + 9)  := Aegis_VM_Types.Byte ((Header.Payload_Len / 256) and 16#FF#);
      Buffer (Buffer'First + 10) := Aegis_VM_Types.Byte ((Header.Payload_Len / 65536) and 16#FF#);
      Buffer (Buffer'First + 11) := Aegis_VM_Types.Byte ((Header.Payload_Len / 16777216) and 16#FF#);

      --  Timestamp (8 bytes LE)
      declare
         T : Unsigned_64 := Header.Timestamp;
      begin
         for I in 0 .. 7 loop
            Buffer (Buffer'First + 12 + I) := Aegis_VM_Types.Byte (T and 16#FF#);
            T := T / 256;
         end loop;
      end;

      --  Sender_ID (32 bytes)
      for I in Header.Sender_ID'Range loop
         Buffer (Buffer'First + 20 + I) := Header.Sender_ID (I);
      end loop;

      --  Reserved bytes remain 0
   end Serialize_Header;

   procedure Send_To_Peer (
      Net     : in Out Network_State;
      Node_ID : in     Hash256;
      Msg     : in     P2P_Message;
      Result  : out    Network_Result
   ) is
      Index       : Peer_Index;
      Found       : Boolean;
      Header_Buf  : Aegis_VM_Types.Byte_Array (0 .. 63);
      Header_Len  : Natural;
      Sent        : Natural;
      Sock_Result : P2P_Sockets.Socket_Result;
      Sock        : P2P_Sockets.Socket_Handle;
   begin
      Find_Peer_Index (Net, Node_ID, Index, Found);
      if not Found then
         Result := Network_Not_Found;
         return;
      end if;

      if Net.Peers (Index).State /= Peer_Connected then
         Result := Network_Connection_Failed;
         return;
      end if;

      --  Check if socket is valid
      if Net.Peers (Index).Socket_FD < 0 then
         Result := Network_Not_Connected;
         return;
      end if;

      --  Serialize header
      Serialize_Header (Msg.Header, Header_Buf, Header_Len);

      --  Get socket handle from peer
      Sock := Create_Handle_From_FD (Net.Peers (Index).Socket_FD);

      --  Send header
      P2P_Sockets.Send_Bytes (
         Sock   => Sock,
         Data   => Header_Buf,
         Sent   => Sent,
         Result => Sock_Result
      );

      if Sock_Result /= P2P_Sockets.Socket_OK then
         Result := Network_Connection_Failed;
         return;
      end if;

      --  Send payload if any
      if Msg.Payload_Size > 0 then
         P2P_Sockets.Send (
            Sock   => Sock,
            Data   => Msg.Payload,
            Length => Msg.Payload_Size,
            Sent   => Sent,
            Result => Sock_Result
         );

         if Sock_Result /= P2P_Sockets.Socket_OK then
            Result := Network_Connection_Failed;
            return;
         end if;
      end if;

      --  Update stats
      Net.Stats.Messages_Sent := Net.Stats.Messages_Sent + 1;
      Net.Stats.Bytes_Sent := Net.Stats.Bytes_Sent +
         Unsigned_64 (Header_Len) + Unsigned_64 (Msg.Payload_Size);
      Net.Peers (Index).TX_Sent := Net.Peers (Index).TX_Sent + 1;

      Result := Network_OK;
   end Send_To_Peer;

   procedure Broadcast (
      Net     : in Out Network_State;
      Msg     : in     P2P_Message;
      Sent    : out    Natural
   ) is
      Send_Result : Network_Result;
   begin
      Sent := 0;

      for I in Net.Peers'Range loop
         if Net.Peers (I).Is_Valid and
            Net.Peers (I).State = Peer_Connected
         then
            Send_To_Peer (Net, Net.Peers (I).Node_ID, Msg, Send_Result);
            if Send_Result = Network_OK then
               Sent := Sent + 1;
            end if;
         end if;
      end loop;
   end Broadcast;

   procedure Broadcast_Except (
      Net       : in Out Network_State;
      Msg       : in     P2P_Message;
      Except_ID : in     Hash256;
      Sent      : out    Natural
   ) is
      Send_Result : Network_Result;
      Is_Except : Boolean;
   begin
      Sent := 0;

      for I in Net.Peers'Range loop
         if Net.Peers (I).Is_Valid and
            Net.Peers (I).State = Peer_Connected
         then
            --  Check if this is the excluded peer
            Is_Except := True;
            for J in Except_ID'Range loop
               if Net.Peers (I).Node_ID (J) /= Except_ID (J) then
                  Is_Except := False;
                  exit;
               end if;
            end loop;

            if not Is_Except then
               Send_To_Peer (Net, Net.Peers (I).Node_ID, Msg, Send_Result);
               if Send_Result = Network_OK then
                  Sent := Sent + 1;
               end if;
            end if;
         end if;
      end loop;
   end Broadcast_Except;

   ---------------------------------------------------------------------------
   --  Gossip Operations
   ---------------------------------------------------------------------------

   procedure Gossip_TX (
      Net     : in Out Network_State;
      TX_Hash : in     Hash256;
      Sent    : out    Natural
   ) is
      Msg : P2P_Message;
   begin
      Create_TX_Announce (Net, TX_Hash, Msg);
      Broadcast (Net, Msg, Sent);
      Net.Stats.TX_Gossiped := Net.Stats.TX_Gossiped + Sent;
   end Gossip_TX;

   procedure Gossip_Block (
      Net        : in Out Network_State;
      Block_Hash : in     Hash256;
      Height     : in     U256;
      Sent       : out    Natural
   ) is
      Msg : P2P_Message;
   begin
      Create_Block_Announce (Net, Block_Hash, Height, Msg);
      Broadcast (Net, Msg, Sent);
      Net.Stats.Blocks_Gossiped := Net.Stats.Blocks_Gossiped + Sent;
   end Gossip_Block;

   procedure Request_TX (
      Net     : in Out Network_State;
      TX_Hash : in     Hash256;
      Result  : out    Network_Result
   ) is
      Msg : P2P_Message;
   begin
      --  Create TX request message
      Initialize_Message (Msg);
      Msg.Header.Magic := Network_Magic;
      Msg.Header.Version := Protocol_Version;
      Msg.Header.Msg_Type := Msg_TX_Request;
      Msg.Header.Sender_ID := Net.Node_ID;

      --  Copy TX hash to payload
      for I in TX_Hash'Range loop
         Msg.Payload (I) := TX_Hash (I);
      end loop;
      Msg.Payload_Size := 32;
      Msg.Header.Payload_Len := 32;
      Msg.Is_Valid := True;

      --  Send to first connected peer (simple approach)
      for I in Net.Peers'Range loop
         if Net.Peers (I).Is_Valid and
            Net.Peers (I).State = Peer_Connected
         then
            Send_To_Peer (Net, Net.Peers (I).Node_ID, Msg, Result);
            return;
         end if;
      end loop;

      Result := Network_Not_Found;  -- No connected peers
   end Request_TX;

   procedure Request_Block (
      Net        : in Out Network_State;
      Block_Hash : in     Hash256;
      Result     : out    Network_Result
   ) is
      Msg : P2P_Message;
   begin
      --  Create block request message
      Initialize_Message (Msg);
      Msg.Header.Magic := Network_Magic;
      Msg.Header.Version := Protocol_Version;
      Msg.Header.Msg_Type := Msg_Block_Request;
      Msg.Header.Sender_ID := Net.Node_ID;

      --  Copy block hash to payload
      for I in Block_Hash'Range loop
         Msg.Payload (I) := Block_Hash (I);
      end loop;
      Msg.Payload_Size := 32;
      Msg.Header.Payload_Len := 32;
      Msg.Is_Valid := True;

      --  Send to peer with best chain (has the block)
      for I in Net.Peers'Range loop
         if Net.Peers (I).Is_Valid and
            Net.Peers (I).State = Peer_Connected
         then
            Send_To_Peer (Net, Net.Peers (I).Node_ID, Msg, Result);
            return;
         end if;
      end loop;

      Result := Network_Not_Found;
   end Request_Block;

   procedure Request_Block_By_Height (
      Net    : in Out Network_State;
      Height : in     U256;
      Result : out    Network_Result
   ) is
      Msg : P2P_Message;
      Len : Natural;
   begin
      --  Create block request message with height
      Initialize_Message (Msg);
      Msg.Header.Magic := Network_Magic;
      Msg.Header.Version := Protocol_Version;
      Msg.Header.Msg_Type := Msg_Block_Request;
      Msg.Header.Sender_ID := Net.Node_ID;

      --  Encode height in payload (32 bytes, LE)
      Len := 0;
      for I in Height.Limbs'Range loop
         declare
            Limb : constant Unsigned_64 := Height.Limbs (I);
         begin
            Msg.Payload (Len) := Aegis_VM_Types.Byte (Limb and 16#FF#);
            Msg.Payload (Len + 1) := Aegis_VM_Types.Byte ((Limb / 256) and 16#FF#);
            Msg.Payload (Len + 2) := Aegis_VM_Types.Byte ((Limb / 65536) and 16#FF#);
            Msg.Payload (Len + 3) := Aegis_VM_Types.Byte ((Limb / 16777216) and 16#FF#);
            Msg.Payload (Len + 4) := Aegis_VM_Types.Byte ((Limb / 2**24 / 256) and 16#FF#);
            Msg.Payload (Len + 5) := Aegis_VM_Types.Byte ((Limb / 2**24 / 65536) and 16#FF#);
            Msg.Payload (Len + 6) := Aegis_VM_Types.Byte ((Limb / 2**24 / 16777216) and 16#FF#);
            Msg.Payload (Len + 7) := Aegis_VM_Types.Byte (Limb / 2**56);
            Len := Len + 8;
         end;
      end loop;

      Msg.Payload_Size := 32;
      Msg.Header.Payload_Len := 32;
      Msg.Is_Valid := True;

      --  Find peer with this height
      for I in Net.Peers'Range loop
         if Net.Peers (I).Is_Valid and
            Net.Peers (I).State = Peer_Connected and
            (U256_Greater (Net.Peers (I).Chain_Height, Height) or
             Net.Peers (I).Chain_Height = Height)
         then
            Send_To_Peer (Net, Net.Peers (I).Node_ID, Msg, Result);
            return;
         end if;
      end loop;

      Result := Network_Not_Found;
   end Request_Block_By_Height;

   ---------------------------------------------------------------------------
   --  Message Construction
   ---------------------------------------------------------------------------

   procedure Create_Handshake (
      Net : in     Network_State;
      Msg : out    P2P_Message
   ) is
      Len : Natural := 0;
   begin
      Initialize_Message (Msg);
      Msg.Header.Magic := Network_Magic;
      Msg.Header.Version := Protocol_Version;
      Msg.Header.Msg_Type := Msg_Handshake;
      Msg.Header.Sender_ID := Net.Node_ID;

      --  Payload: chain_id (32) + height (32) + best_block (32) = 96 bytes
      --  Chain ID
      for I in Net.Chain_ID.Limbs'Range loop
         declare
            Limb : constant Unsigned_64 := Net.Chain_ID.Limbs (I);
         begin
            Msg.Payload (Len) := Aegis_VM_Types.Byte (Limb and 16#FF#);
            Msg.Payload (Len + 1) := Aegis_VM_Types.Byte ((Limb / 256) and 16#FF#);
            Msg.Payload (Len + 2) := Aegis_VM_Types.Byte ((Limb / 65536) and 16#FF#);
            Msg.Payload (Len + 3) := Aegis_VM_Types.Byte ((Limb / 16777216) and 16#FF#);
            Msg.Payload (Len + 4) := Aegis_VM_Types.Byte ((Limb / 2**24 / 256) and 16#FF#);
            Msg.Payload (Len + 5) := Aegis_VM_Types.Byte ((Limb / 2**24 / 65536) and 16#FF#);
            Msg.Payload (Len + 6) := Aegis_VM_Types.Byte ((Limb / 2**24 / 16777216) and 16#FF#);
            Msg.Payload (Len + 7) := Aegis_VM_Types.Byte (Limb / 2**56);
            Len := Len + 8;
         end;
      end loop;

      --  Current height
      for I in Net.Current_Height.Limbs'Range loop
         declare
            Limb : constant Unsigned_64 := Net.Current_Height.Limbs (I);
         begin
            Msg.Payload (Len) := Aegis_VM_Types.Byte (Limb and 16#FF#);
            Msg.Payload (Len + 1) := Aegis_VM_Types.Byte ((Limb / 256) and 16#FF#);
            Msg.Payload (Len + 2) := Aegis_VM_Types.Byte ((Limb / 65536) and 16#FF#);
            Msg.Payload (Len + 3) := Aegis_VM_Types.Byte ((Limb / 16777216) and 16#FF#);
            Msg.Payload (Len + 4) := Aegis_VM_Types.Byte ((Limb / 2**24 / 256) and 16#FF#);
            Msg.Payload (Len + 5) := Aegis_VM_Types.Byte ((Limb / 2**24 / 65536) and 16#FF#);
            Msg.Payload (Len + 6) := Aegis_VM_Types.Byte ((Limb / 2**24 / 16777216) and 16#FF#);
            Msg.Payload (Len + 7) := Aegis_VM_Types.Byte (Limb / 2**56);
            Len := Len + 8;
         end;
      end loop;

      --  Best block hash
      for I in Net.Best_Block'Range loop
         Msg.Payload (Len) := Net.Best_Block (I);
         Len := Len + 1;
      end loop;

      Msg.Payload_Size := Len;
      Msg.Header.Payload_Len := Unsigned_32 (Len);
      Msg.Is_Valid := True;
   end Create_Handshake;

   procedure Create_Ping (
      Net : in     Network_State;
      Msg : out    P2P_Message
   ) is
   begin
      Initialize_Message (Msg);
      Msg.Header.Magic := Network_Magic;
      Msg.Header.Version := Protocol_Version;
      Msg.Header.Msg_Type := Msg_Ping;
      Msg.Header.Sender_ID := Net.Node_ID;
      Msg.Header.Timestamp := 0;  -- Would use current time
      Msg.Payload_Size := 0;
      Msg.Header.Payload_Len := 0;
      Msg.Is_Valid := True;
   end Create_Ping;

   procedure Create_Pong (
      Net      : in     Network_State;
      Ping_Msg : in     P2P_Message;
      Msg      : out    P2P_Message
   ) is
   begin
      Initialize_Message (Msg);
      Msg.Header.Magic := Network_Magic;
      Msg.Header.Version := Protocol_Version;
      Msg.Header.Msg_Type := Msg_Pong;
      Msg.Header.Sender_ID := Net.Node_ID;
      --  Echo back ping timestamp for latency calculation
      Msg.Header.Timestamp := Ping_Msg.Header.Timestamp;
      Msg.Payload_Size := 0;
      Msg.Header.Payload_Len := 0;
      Msg.Is_Valid := True;
   end Create_Pong;

   procedure Create_TX_Announce (
      Net      : in     Network_State;
      TX_Hash  : in     Hash256;
      Msg      : out    P2P_Message
   ) is
   begin
      Initialize_Message (Msg);
      Msg.Header.Magic := Network_Magic;
      Msg.Header.Version := Protocol_Version;
      Msg.Header.Msg_Type := Msg_TX_Announce;
      Msg.Header.Sender_ID := Net.Node_ID;

      --  Copy TX hash to payload
      for I in TX_Hash'Range loop
         Msg.Payload (I) := TX_Hash (I);
      end loop;

      Msg.Payload_Size := 32;
      Msg.Header.Payload_Len := 32;
      Msg.Is_Valid := True;
   end Create_TX_Announce;

   procedure Create_Block_Announce (
      Net        : in     Network_State;
      Block_Hash : in     Hash256;
      Height     : in     U256;
      Msg        : out    P2P_Message
   ) is
      Len : Natural := 0;
   begin
      Initialize_Message (Msg);
      Msg.Header.Magic := Network_Magic;
      Msg.Header.Version := Protocol_Version;
      Msg.Header.Msg_Type := Msg_Block_Announce;
      Msg.Header.Sender_ID := Net.Node_ID;

      --  Payload: block_hash (32) + height (32) = 64 bytes
      --  Block hash
      for I in Block_Hash'Range loop
         Msg.Payload (Len) := Block_Hash (I);
         Len := Len + 1;
      end loop;

      --  Height
      for I in Height.Limbs'Range loop
         declare
            Limb : constant Unsigned_64 := Height.Limbs (I);
         begin
            Msg.Payload (Len) := Aegis_VM_Types.Byte (Limb and 16#FF#);
            Msg.Payload (Len + 1) := Aegis_VM_Types.Byte ((Limb / 256) and 16#FF#);
            Msg.Payload (Len + 2) := Aegis_VM_Types.Byte ((Limb / 65536) and 16#FF#);
            Msg.Payload (Len + 3) := Aegis_VM_Types.Byte ((Limb / 16777216) and 16#FF#);
            Msg.Payload (Len + 4) := Aegis_VM_Types.Byte ((Limb / 2**24 / 256) and 16#FF#);
            Msg.Payload (Len + 5) := Aegis_VM_Types.Byte ((Limb / 2**24 / 65536) and 16#FF#);
            Msg.Payload (Len + 6) := Aegis_VM_Types.Byte ((Limb / 2**24 / 16777216) and 16#FF#);
            Msg.Payload (Len + 7) := Aegis_VM_Types.Byte (Limb / 2**56);
            Len := Len + 8;
         end;
      end loop;

      Msg.Payload_Size := Len;
      Msg.Header.Payload_Len := Unsigned_32 (Len);
      Msg.Is_Valid := True;
   end Create_Block_Announce;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Peer_Count (Net : Network_State) return Natural is
   begin
      return Net.Stats.Peers_Connected;
   end Get_Peer_Count;

   function Is_Peer_Connected (
      Net     : Network_State;
      Node_ID : Hash256
   ) return Boolean is
      Index : Peer_Index;
      Found : Boolean;
   begin
      Find_Peer_Index (Net, Node_ID, Index, Found);
      if Found then
         return Net.Peers (Index).State = Peer_Connected;
      end if;
      return False;
   end Is_Peer_Connected;

   function Is_Peer_Banned (
      Net     : Network_State;
      Node_ID : Hash256
   ) return Boolean is
      Index : Ban_Index;
      Found : Boolean;
   begin
      Find_Ban_Index (Net, Node_ID, Index, Found);
      return Found;
   end Is_Peer_Banned;

   function Get_Stats (Net : Network_State) return Network_Stats is
   begin
      return Net.Stats;
   end Get_Stats;

   function Get_Best_Peer_Height (Net : Network_State) return U256 is
      Best : U256 := Aegis_VM_Types.U256_Zero;
   begin
      for I in Net.Peers'Range loop
         if Net.Peers (I).Is_Valid and
            Net.Peers (I).State = Peer_Connected
         then
            if U256_Greater (Net.Peers (I).Chain_Height, Best) then
               Best := Net.Peers (I).Chain_Height;
            end if;
         end if;
      end loop;
      return Best;
   end Get_Best_Peer_Height;

   function Is_Running (Net : Network_State) return Boolean is
   begin
      return Net.Is_Running;
   end Is_Running;

   ---------------------------------------------------------------------------
   --  Peer Discovery
   ---------------------------------------------------------------------------

   procedure Request_Peers (
      Net    : in Out Network_State;
      Result : out    Network_Result
   ) is
      Msg : P2P_Message;
   begin
      Initialize_Message (Msg);
      Msg.Header.Magic := Network_Magic;
      Msg.Header.Version := Protocol_Version;
      Msg.Header.Msg_Type := Msg_Peer_Request;
      Msg.Header.Sender_ID := Net.Node_ID;
      Msg.Payload_Size := 0;
      Msg.Header.Payload_Len := 0;
      Msg.Is_Valid := True;

      --  Send to first connected peer
      for I in Net.Peers'Range loop
         if Net.Peers (I).Is_Valid and
            Net.Peers (I).State = Peer_Connected
         then
            Send_To_Peer (Net, Net.Peers (I).Node_ID, Msg, Result);
            return;
         end if;
      end loop;

      Result := Network_Not_Found;
   end Request_Peers;

   procedure Add_Discovered_Peer (
      Net     : in Out Network_State;
      Address : in     Peer_Address;
      Result  : out    Network_Result
   ) is
   begin
      --  Check if we already know this peer
      --  (Would need to compare addresses in real impl)

      --  For now, just try to connect
      if Net.Is_Running then
         Connect_Peer (Net, Address, Result);
      else
         Result := Network_Not_Initialized;
      end if;
   end Add_Discovered_Peer;

   ---------------------------------------------------------------------------
   --  Maintenance Operations
   ---------------------------------------------------------------------------

   procedure Process_Events (
      Net : in Out Network_State
   ) is
   begin
      --  In a real implementation, this would:
      --  1. Accept incoming connections
      --  2. Read incoming messages
      --  3. Process message queue
      --  4. Handle disconnections
      --  5. Update peer states
      null;
   end Process_Events;

   procedure Send_Keepalives (
      Net          : in Out Network_State;
      Current_Time : in     Unsigned_64
   ) is
      Msg : P2P_Message;
      Send_Result : Network_Result;
   begin
      Create_Ping (Net, Msg);
      Msg.Header.Timestamp := Current_Time;

      for I in Net.Peers'Range loop
         if Net.Peers (I).Is_Valid and
            Net.Peers (I).State = Peer_Connected
         then
            --  Check if ping is due
            if Current_Time - Net.Peers (I).Last_Ping >= Unsigned_64 (Ping_Interval) then
               Send_To_Peer (Net, Net.Peers (I).Node_ID, Msg, Send_Result);
               Net.Peers (I).Last_Ping := Current_Time;
            end if;
         end if;
      end loop;
   end Send_Keepalives;

   procedure Expire_Stale_Peers (
      Net          : in Out Network_State;
      Current_Time : in     Unsigned_64;
      Expired      : out    Natural
   ) is
      Disconnect_Result : Network_Result;
   begin
      Expired := 0;

      for I in Net.Peers'Range loop
         if Net.Peers (I).Is_Valid and
            Net.Peers (I).State = Peer_Connected
         then
            --  Check if peer has timed out
            if Current_Time - Net.Peers (I).Last_Seen >= Unsigned_64 (Peer_Timeout) then
               Disconnect_Peer (Net, Net.Peers (I).Node_ID, Disconnect_Result);
               Expired := Expired + 1;
            end if;
         end if;
      end loop;
   end Expire_Stale_Peers;

   procedure Cleanup_Bans (
      Net          : in Out Network_State;
      Current_Time : in     Unsigned_64;
      Removed      : out    Natural
   ) is
   begin
      Removed := 0;

      for I in Net.Ban_List'Range loop
         if Net.Ban_List (I).Is_Valid then
            --  Check if ban has expired
            if Current_Time >= Net.Ban_List (I).Ban_Until then
               Initialize_Ban_Entry (Net.Ban_List (I));
               Net.Ban_Count := Net.Ban_Count - 1;
               Removed := Removed + 1;
            end if;
         end if;
      end loop;
   end Cleanup_Bans;

end P2P_Network;
