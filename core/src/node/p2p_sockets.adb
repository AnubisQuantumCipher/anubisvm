-------------------------------------------------------------------------------
--  P2P_Sockets: Low-Level Socket Operations Implementation
--
--  Uses a socket pool pattern to avoid exposing GNAT.Sockets types in the
--  public interface. Sockets are stored internally and accessed via index.
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Ada.Streams;           use Ada.Streams;
with Ada.Exceptions;
with GNAT.Sockets;          use GNAT.Sockets;

package body P2P_Sockets is

   ---------------------------------------------------------------------------
   --  Internal Socket Pool
   ---------------------------------------------------------------------------

   --  Socket pool entry
   type Socket_Entry is record
      In_Use : Boolean := False;
      Sock   : Socket_Type;
   end record;

   --  Socket pool (index 1 .. Max_Socket_Connections)
   subtype Pool_Index is Positive range 1 .. Max_Socket_Connections;
   Socket_Pool : array (Pool_Index) of Socket_Entry;

   --  Listener pool (separate for listener sockets)
   Max_Listeners : constant := 4;
   subtype Listener_Pool_Index is Positive range 1 .. Max_Listeners;
   Listener_Pool : array (Listener_Pool_Index) of Socket_Entry;

   Initialized : Boolean := False;

   --  Internal receive buffer
   Recv_Buffer : Stream_Element_Array (1 .. Stream_Element_Offset (Socket_Buffer_Size));

   ---------------------------------------------------------------------------
   --  Internal Pool Management
   ---------------------------------------------------------------------------

   --  Allocate a slot in the socket pool
   function Allocate_Socket_Slot return Natural is
   begin
      for I in Pool_Index loop
         if not Socket_Pool (I).In_Use then
            Socket_Pool (I).In_Use := True;
            return I;
         end if;
      end loop;
      return 0;  -- No slot available
   end Allocate_Socket_Slot;

   --  Free a slot in the socket pool
   procedure Free_Socket_Slot (Index : Natural) is
   begin
      if Index in Pool_Index then
         Socket_Pool (Index).In_Use := False;
      end if;
   end Free_Socket_Slot;

   --  Allocate a slot in the listener pool
   function Allocate_Listener_Slot return Natural is
   begin
      for I in Listener_Pool_Index loop
         if not Listener_Pool (I).In_Use then
            Listener_Pool (I).In_Use := True;
            return I;
         end if;
      end loop;
      return 0;  -- No slot available
   end Allocate_Listener_Slot;

   --  Free a slot in the listener pool
   procedure Free_Listener_Slot (Index : Natural) is
   begin
      if Index in Listener_Pool_Index then
         Listener_Pool (Index).In_Use := False;
      end if;
   end Free_Listener_Slot;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Convert GNAT.Sockets exception to Socket_Result
   function Exception_To_Result (E : Ada.Exceptions.Exception_Occurrence) return Socket_Result is
      Msg : constant String := Ada.Exceptions.Exception_Message (E);
   begin
      if Msg'Length >= 11 and then Msg (Msg'First .. Msg'First + 10) = "Connection " then
         return Socket_Connection_Refused;
      elsif Msg'Length >= 7 and then Msg (Msg'First .. Msg'First + 6) = "Timeout" then
         return Socket_Timeout;
      else
         return Socket_Error;
      end if;
   end Exception_To_Result;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize is
   begin
      if not Initialized then
         --  Initialize socket pool
         for I in Pool_Index loop
            Socket_Pool (I).In_Use := False;
         end loop;
         for I in Listener_Pool_Index loop
            Listener_Pool (I).In_Use := False;
         end loop;
         Initialized := True;
      end if;
   end Initialize;

   procedure Shutdown is
   begin
      --  Close all active sockets
      for I in Pool_Index loop
         if Socket_Pool (I).In_Use then
            begin
               Close_Socket (Socket_Pool (I).Sock);
            exception
               when others => null;
            end;
            Socket_Pool (I).In_Use := False;
         end if;
      end loop;
      --  Close all listeners
      for I in Listener_Pool_Index loop
         if Listener_Pool (I).In_Use then
            begin
               Close_Socket (Listener_Pool (I).Sock);
            exception
               when others => null;
            end;
            Listener_Pool (I).In_Use := False;
         end if;
      end loop;
      Initialized := False;
   end Shutdown;

   ---------------------------------------------------------------------------
   --  Listener Operations
   ---------------------------------------------------------------------------

   procedure Create_Listener (
      Port    : in     Unsigned_16;
      State   : out    Listener_State;
      Result  : out    Socket_Result
   ) is
      Sock  : Socket_Type;
      Addr  : Sock_Addr_Type;
      Index : Natural;
   begin
      State := (Is_Valid => False, Index => 0, Port => 0);
      Result := Socket_Error;

      if not Initialized then
         return;
      end if;

      --  Allocate listener slot
      Index := Allocate_Listener_Slot;
      if Index = 0 then
         Result := Socket_Buffer_Full;
         return;
      end if;

      begin
         --  Create TCP socket
         Create_Socket (Sock);

         --  Set reuse address option
         Set_Socket_Option (Sock, Socket_Level, (Reuse_Address, True));

         --  Bind to port
         Addr := (
            Family => Family_Inet,
            Addr   => Any_Inet_Addr,
            Port   => Port_Type (Port)
         );
         Bind_Socket (Sock, Addr);

         --  Start listening
         Listen_Socket (Sock);

         --  Store in pool
         Listener_Pool (Index).Sock := Sock;

         --  Return state
         State := (
            Is_Valid => True,
            Index    => Index,
            Port     => Port
         );
         Result := Socket_OK;

      exception
         when E : GNAT.Sockets.Socket_Error =>
            Free_Listener_Slot (Index);
            Result := Exception_To_Result (E);
      end;
   end Create_Listener;

   procedure Accept_Connection (
      Listener  : in     Listener_State;
      Sock      : out    Socket_Handle;
      Peer_Addr : out    P2P_Network.Peer_Address;
      Result    : out    Socket_Result
   ) is
      Client_Sock : Socket_Type;
      Client_Addr : Sock_Addr_Type;
      Index       : Natural;
   begin
      Sock := Null_Socket;
      Peer_Addr := (
         Is_Valid => False,
         Address  => (others => ' '),
         Addr_Len => 0,
         Port     => 0
      );
      Result := Socket_Error;

      if not Listener.Is_Valid or else
         Listener.Index not in Listener_Pool_Index or else
         not Listener_Pool (Listener.Index).In_Use
      then
         return;
      end if;

      --  Allocate socket slot for client
      Index := Allocate_Socket_Slot;
      if Index = 0 then
         Result := Socket_Buffer_Full;
         return;
      end if;

      begin
         --  Accept connection
         Accept_Socket (Listener_Pool (Listener.Index).Sock, Client_Sock, Client_Addr);

         --  Store client socket in pool
         Socket_Pool (Index).Sock := Client_Sock;

         --  Return handle
         Sock := (Is_Valid => True, Index => Index);

         --  Convert address
         declare
            Addr_Str : constant String := Image (Client_Addr.Addr);
            Len      : constant Natural := Natural'Min (Addr_Str'Length,
                                                        P2P_Network.Max_Address_Len);
         begin
            Peer_Addr.Is_Valid := True;
            Peer_Addr.Address (1 .. Len) := Addr_Str (Addr_Str'First .. Addr_Str'First + Len - 1);
            Peer_Addr.Addr_Len := Len;
            Peer_Addr.Port := Unsigned_16 (Client_Addr.Port);
         end;

         Result := Socket_OK;

      exception
         when E : GNAT.Sockets.Socket_Error =>
            Free_Socket_Slot (Index);
            Result := Exception_To_Result (E);
      end;
   end Accept_Connection;

   procedure Close_Listener (
      State : in Out Listener_State
   ) is
   begin
      if State.Is_Valid and then
         State.Index in Listener_Pool_Index and then
         Listener_Pool (State.Index).In_Use
      then
         begin
            Close_Socket (Listener_Pool (State.Index).Sock);
         exception
            when others => null;  -- Ignore close errors
         end;
         Free_Listener_Slot (State.Index);
      end if;
      State := (Is_Valid => False, Index => 0, Port => 0);
   end Close_Listener;

   ---------------------------------------------------------------------------
   --  Connection Operations
   ---------------------------------------------------------------------------

   procedure Connect (
      Address : in     P2P_Network.Peer_Address;
      Sock    : out    Socket_Handle;
      Result  : out    Socket_Result
   ) is
      New_Sock    : Socket_Type;
      Server_Addr : Sock_Addr_Type;
      Addr_Str    : constant String := Address.Address (1 .. Address.Addr_Len);
      Index       : Natural;
   begin
      Sock := Null_Socket;
      Result := Socket_Error;

      if not Initialized or not Address.Is_Valid then
         return;
      end if;

      --  Allocate socket slot
      Index := Allocate_Socket_Slot;
      if Index = 0 then
         Result := Socket_Buffer_Full;
         return;
      end if;

      begin
         --  Create TCP socket
         Create_Socket (New_Sock);

         --  Resolve address
         Server_Addr := (
            Family => Family_Inet,
            Addr   => Inet_Addr (Addr_Str),
            Port   => Port_Type (Address.Port)
         );

         --  Connect (blocking)
         Connect_Socket (New_Sock, Server_Addr);

         --  Store in pool
         Socket_Pool (Index).Sock := New_Sock;

         --  Return handle
         Sock := (Is_Valid => True, Index => Index);
         Result := Socket_OK;

      exception
         when E : GNAT.Sockets.Socket_Error =>
            Free_Socket_Slot (Index);
            Result := Exception_To_Result (E);
         when GNAT.Sockets.Host_Error =>
            Free_Socket_Slot (Index);
            Result := Socket_Host_Not_Found;
      end;
   end Connect;

   procedure Close (
      Sock : in Out Socket_Handle
   ) is
   begin
      if Sock.Is_Valid and then
         Sock.Index in Pool_Index and then
         Socket_Pool (Sock.Index).In_Use
      then
         begin
            Close_Socket (Socket_Pool (Sock.Index).Sock);
         exception
            when others => null;  -- Ignore close errors
         end;
         Free_Socket_Slot (Sock.Index);
      end if;
      Sock := Null_Socket;
   end Close;

   function Is_Connected (Sock : Socket_Handle) return Boolean is
   begin
      return Sock.Is_Valid and then
             Sock.Index in Pool_Index and then
             Socket_Pool (Sock.Index).In_Use;
   end Is_Connected;

   ---------------------------------------------------------------------------
   --  Data Transfer Operations
   ---------------------------------------------------------------------------

   procedure Send (
      Sock   : in     Socket_Handle;
      Data   : in     P2P_Network.Payload_Buffer;
      Length : in     Natural;
      Sent   : out    Natural;
      Result : out    Socket_Result
   ) is
      Send_Buf  : Stream_Element_Array (1 .. Stream_Element_Offset (Length));
      Last_Sent : Stream_Element_Offset;
   begin
      Sent := 0;
      Result := Socket_Error;

      if not Sock.Is_Valid or else
         Sock.Index not in Pool_Index or else
         not Socket_Pool (Sock.Index).In_Use or else
         Length = 0
      then
         return;
      end if;

      begin
         --  Convert data to stream elements
         for I in 1 .. Length loop
            Send_Buf (Stream_Element_Offset (I)) :=
               Stream_Element (Data (P2P_Network.Payload_Index (I - 1)));
         end loop;

         --  Send data
         Send_Socket (Socket_Pool (Sock.Index).Sock, Send_Buf, Last_Sent);

         Sent := Natural (Last_Sent);
         Result := Socket_OK;

      exception
         when E : GNAT.Sockets.Socket_Error =>
            Result := Exception_To_Result (E);
      end;
   end Send;

   procedure Send_Bytes (
      Sock   : in     Socket_Handle;
      Data   : in     Aegis_VM_Types.Byte_Array;
      Sent   : out    Natural;
      Result : out    Socket_Result
   ) is
      Send_Buf  : Stream_Element_Array (1 .. Stream_Element_Offset (Data'Length));
      Last_Sent : Stream_Element_Offset;
   begin
      Sent := 0;
      Result := Socket_Error;

      if not Sock.Is_Valid or else
         Sock.Index not in Pool_Index or else
         not Socket_Pool (Sock.Index).In_Use or else
         Data'Length = 0
      then
         return;
      end if;

      begin
         --  Convert data to stream elements
         for I in Data'Range loop
            Send_Buf (Stream_Element_Offset (I - Data'First + 1)) :=
               Stream_Element (Data (I));
         end loop;

         --  Send data
         Send_Socket (Socket_Pool (Sock.Index).Sock, Send_Buf, Last_Sent);

         Sent := Natural (Last_Sent);
         Result := Socket_OK;

      exception
         when E : GNAT.Sockets.Socket_Error =>
            Result := Exception_To_Result (E);
      end;
   end Send_Bytes;

   procedure Receive (
      Sock     : in     Socket_Handle;
      Data     : out    P2P_Network.Payload_Buffer;
      Length   : out    Natural;
      Result   : out    Socket_Result
   ) is
      Last_Recv : Stream_Element_Offset;
   begin
      Data := (others => 0);
      Length := 0;
      Result := Socket_Error;

      if not Sock.Is_Valid or else
         Sock.Index not in Pool_Index or else
         not Socket_Pool (Sock.Index).In_Use
      then
         return;
      end if;

      begin
         --  Receive data
         Receive_Socket (Socket_Pool (Sock.Index).Sock, Recv_Buffer, Last_Recv);

         if Last_Recv < 1 then
            Result := Socket_Closed;
            return;
         end if;

         --  Copy to output buffer
         Length := Natural'Min (Natural (Last_Recv), P2P_Network.Max_Payload_Len);
         for I in 1 .. Length loop
            Data (P2P_Network.Payload_Index (I - 1)) :=
               Aegis_VM_Types.Byte (Recv_Buffer (Stream_Element_Offset (I)));
         end loop;

         Result := Socket_OK;

      exception
         when E : GNAT.Sockets.Socket_Error =>
            Result := Exception_To_Result (E);
      end;
   end Receive;

   procedure Receive_Bytes (
      Sock       : in     Socket_Handle;
      Data       : out    Aegis_VM_Types.Byte_Array;
      Max_Length : in     Natural;
      Length     : out    Natural;
      Result     : out    Socket_Result
   ) is
      Last_Recv : Stream_Element_Offset;
      Recv_Len  : Natural;
   begin
      Data := (others => 0);
      Length := 0;
      Result := Socket_Error;

      if not Sock.Is_Valid or else
         Sock.Index not in Pool_Index or else
         not Socket_Pool (Sock.Index).In_Use or else
         Max_Length = 0
      then
         return;
      end if;

      begin
         --  Limit receive size
         Recv_Len := Natural'Min (Max_Length, Socket_Buffer_Size);

         --  Receive data
         Receive_Socket (Socket_Pool (Sock.Index).Sock,
                         Recv_Buffer (1 .. Stream_Element_Offset (Recv_Len)),
                         Last_Recv);

         if Last_Recv < 1 then
            Result := Socket_Closed;
            return;
         end if;

         --  Copy to output buffer
         Length := Natural'Min (Natural (Last_Recv), Data'Length);
         for I in 1 .. Length loop
            Data (Data'First + I - 1) :=
               Aegis_VM_Types.Byte (Recv_Buffer (Stream_Element_Offset (I)));
         end loop;

         Result := Socket_OK;

      exception
         when E : GNAT.Sockets.Socket_Error =>
            Result := Exception_To_Result (E);
      end;
   end Receive_Bytes;

   ---------------------------------------------------------------------------
   --  Utility Operations
   ---------------------------------------------------------------------------

   procedure Set_Non_Blocking (
      Sock : in Socket_Handle
   ) is
      Request : Request_Type := (Name => Non_Blocking_IO, Enabled => True);
   begin
      if Sock.Is_Valid and then
         Sock.Index in Pool_Index and then
         Socket_Pool (Sock.Index).In_Use
      then
         Control_Socket (Socket_Pool (Sock.Index).Sock, Request);
      end if;
   exception
      when others => null;  -- Ignore errors
   end Set_Non_Blocking;

   procedure Set_Blocking (
      Sock : in Socket_Handle
   ) is
      Request : Request_Type := (Name => Non_Blocking_IO, Enabled => False);
   begin
      if Sock.Is_Valid and then
         Sock.Index in Pool_Index and then
         Socket_Pool (Sock.Index).In_Use
      then
         Control_Socket (Socket_Pool (Sock.Index).Sock, Request);
      end if;
   exception
      when others => null;  -- Ignore errors
   end Set_Blocking;

   procedure Get_Peer_Address (
      Sock    : in     Socket_Handle;
      Address : out    P2P_Network.Peer_Address
   ) is
      Addr : Sock_Addr_Type;
   begin
      Address := (
         Is_Valid => False,
         Address  => (others => ' '),
         Addr_Len => 0,
         Port     => 0
      );

      if not Sock.Is_Valid or else
         Sock.Index not in Pool_Index or else
         not Socket_Pool (Sock.Index).In_Use
      then
         return;
      end if;

      begin
         Addr := Get_Peer_Name (Socket_Pool (Sock.Index).Sock);

         declare
            Addr_Str : constant String := Image (Addr.Addr);
            Len      : constant Natural := Natural'Min (Addr_Str'Length,
                                                        P2P_Network.Max_Address_Len);
         begin
            Address.Is_Valid := True;
            Address.Address (1 .. Len) := Addr_Str (Addr_Str'First .. Addr_Str'First + Len - 1);
            Address.Addr_Len := Len;
            Address.Port := Unsigned_16 (Addr.Port);
         end;
      exception
         when others => null;  -- Leave as invalid
      end;
   end Get_Peer_Address;

   procedure Parse_Address (
      Addr_Str : in     String;
      Port     : in     Unsigned_16;
      Address  : out    P2P_Network.Peer_Address;
      Success  : out    Boolean
   ) is
      Len : constant Natural := Natural'Min (Addr_Str'Length, P2P_Network.Max_Address_Len);
   begin
      Address := (
         Is_Valid => False,
         Address  => (others => ' '),
         Addr_Len => 0,
         Port     => 0
      );
      Success := False;

      if Addr_Str'Length = 0 then
         return;
      end if;

      --  Validate address format (basic check)
      begin
         --  Try to parse as IP address
         declare
            Test_Addr : constant Inet_Addr_Type := Inet_Addr (Addr_Str);
            pragma Unreferenced (Test_Addr);
         begin
            --  Valid IP address
            Address.Is_Valid := True;
            Address.Address (1 .. Len) := Addr_Str (Addr_Str'First .. Addr_Str'First + Len - 1);
            Address.Addr_Len := Len;
            Address.Port := Port;
            Success := True;
         end;
      exception
         when others =>
            --  Try as hostname (let connect resolve it)
            Address.Is_Valid := True;
            Address.Address (1 .. Len) := Addr_Str (Addr_Str'First .. Addr_Str'First + Len - 1);
            Address.Addr_Len := Len;
            Address.Port := Port;
            Success := True;
      end;
   end Parse_Address;

   ---------------------------------------------------------------------------
   --  Handle Creation (for interop with FD-based systems)
   ---------------------------------------------------------------------------

   function Create_Handle_From_FD (FD : Integer) return Socket_Handle is
      pragma Unreferenced (FD);
   begin
      --  This function is deprecated in the pool-based design.
      --  For FD interop, we would need to store the socket in the pool,
      --  but we don't have the Socket_Type from just an FD.
      --  Return null for now - callers should use Connect instead.
      return Null_Socket;
   end Create_Handle_From_FD;

   function Get_FD (Sock : Socket_Handle) return Integer is
   begin
      --  Return the pool index as a pseudo-FD for tracking purposes
      if Sock.Is_Valid then
         return Sock.Index;
      else
         return -1;
      end if;
   end Get_FD;

end P2P_Sockets;
