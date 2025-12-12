-------------------------------------------------------------------------------
--  P2P_Sockets: Low-Level Socket Operations for P2P Network
--
--  This package provides the actual TCP socket implementation for P2P
--  communication. It wraps GNAT.Sockets with a simplified interface suitable
--  for peer-to-peer blockchain networking.
--
--  Features:
--  - Non-blocking socket operations
--  - Connection management with timeouts
--  - Buffer-based send/receive
--  - Socket listener for incoming connections
--
--  Note: This package has SPARK_Mode Off since GNAT.Sockets is not SPARK-compatible.
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (Off);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with P2P_Network;

package P2P_Sockets is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum concurrent peer connections
   Max_Socket_Connections : constant := 64;

   --  Socket receive buffer size
   Socket_Buffer_Size : constant := 65536;  -- 64 KB

   --  Connection timeout (milliseconds)
   Connect_Timeout_Ms : constant := 10000;  -- 10 seconds

   --  Read timeout (milliseconds)
   Read_Timeout_Ms : constant := 5000;  -- 5 seconds

   ---------------------------------------------------------------------------
   --  Socket Handle Type
   ---------------------------------------------------------------------------

   --  Opaque socket handle
   type Socket_Handle is private;

   --  Invalid socket constant
   Null_Socket : constant Socket_Handle;

   ---------------------------------------------------------------------------
   --  Result Types
   ---------------------------------------------------------------------------

   type Socket_Result is (
      Socket_OK,
      Socket_Error,
      Socket_Closed,
      Socket_Timeout,
      Socket_Would_Block,
      Socket_Connection_Refused,
      Socket_Host_Not_Found,
      Socket_Already_Connected,
      Socket_Not_Connected,
      Socket_Buffer_Full
   );

   ---------------------------------------------------------------------------
   --  Socket Listener State
   ---------------------------------------------------------------------------

   type Listener_State is private;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize the socket subsystem
   procedure Initialize;

   --  Shutdown the socket subsystem
   procedure Shutdown;

   ---------------------------------------------------------------------------
   --  Listener Operations (for incoming connections)
   ---------------------------------------------------------------------------

   --  Create a listener on specified port
   procedure Create_Listener (
      Port    : in     Unsigned_16;
      State   : out    Listener_State;
      Result  : out    Socket_Result
   );

   --  Accept an incoming connection (non-blocking)
   procedure Accept_Connection (
      Listener  : in     Listener_State;
      Sock      : out    Socket_Handle;
      Peer_Addr : out    P2P_Network.Peer_Address;
      Result    : out    Socket_Result
   );

   --  Close the listener
   procedure Close_Listener (
      State : in out Listener_State
   );

   ---------------------------------------------------------------------------
   --  Connection Operations
   ---------------------------------------------------------------------------

   --  Connect to a remote peer (blocking with timeout)
   procedure Connect (
      Address : in     P2P_Network.Peer_Address;
      Sock    : out    Socket_Handle;
      Result  : out    Socket_Result
   );

   --  Close a connection
   procedure Close (
      Sock : in Out Socket_Handle
   );

   --  Check if socket is connected
   function Is_Connected (Sock : Socket_Handle) return Boolean;

   ---------------------------------------------------------------------------
   --  Data Transfer Operations
   ---------------------------------------------------------------------------

   --  Send data (blocking)
   procedure Send (
      Sock   : in     Socket_Handle;
      Data   : in     P2P_Network.Payload_Buffer;
      Length : in     Natural;
      Sent   : out    Natural;
      Result : out    Socket_Result
   );

   --  Send raw bytes
   procedure Send_Bytes (
      Sock   : in     Socket_Handle;
      Data   : in     Aegis_VM_Types.Byte_Array;
      Sent   : out    Natural;
      Result : out    Socket_Result
   );

   --  Receive data (blocking with timeout)
   procedure Receive (
      Sock     : in     Socket_Handle;
      Data     : out    P2P_Network.Payload_Buffer;
      Length   : out    Natural;
      Result   : out    Socket_Result
   );

   --  Receive up to Max_Length bytes
   procedure Receive_Bytes (
      Sock       : in     Socket_Handle;
      Data       : out    Aegis_VM_Types.Byte_Array;
      Max_Length : in     Natural;
      Length     : out    Natural;
      Result     : out    Socket_Result
   );

   ---------------------------------------------------------------------------
   --  Utility Operations
   ---------------------------------------------------------------------------

   --  Set socket to non-blocking mode
   procedure Set_Non_Blocking (
      Sock : in Socket_Handle
   );

   --  Set socket to blocking mode
   procedure Set_Blocking (
      Sock : in Socket_Handle
   );

   --  Get peer address from connected socket
   procedure Get_Peer_Address (
      Sock    : in     Socket_Handle;
      Address : out    P2P_Network.Peer_Address
   );

   --  Parse address string to Peer_Address
   procedure Parse_Address (
      Addr_Str : in     String;
      Port     : in     Unsigned_16;
      Address  : out    P2P_Network.Peer_Address;
      Success  : out    Boolean
   );

   --  Create a socket handle from file descriptor (for internal tracking)
   function Create_Handle_From_FD (FD : Integer) return Socket_Handle;

   --  Get file descriptor from socket handle
   function Get_FD (Sock : Socket_Handle) return Integer;

private

   --  We use an index into a socket pool to avoid exposing GNAT.Sockets types
   type Socket_Handle is record
      Is_Valid : Boolean := False;
      Index    : Natural := 0;  -- Index into socket pool
   end record;

   Null_Socket : constant Socket_Handle := (Is_Valid => False, Index => 0);

   type Listener_State is record
      Is_Valid : Boolean := False;
      Index    : Natural := 0;  -- Index into socket pool
      Port     : Unsigned_16 := 0;
   end record;

end P2P_Sockets;
