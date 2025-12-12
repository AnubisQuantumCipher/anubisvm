--  Anubis Main Buffers: Static buffers for RPC handling
--
--  These buffers are declared at package level to avoid stack allocation
--  of large arrays during procedure elaboration. Using (others => 0)
--  on procedure-local 256KB+ arrays causes stack overflow.

pragma SPARK_Mode (Off);  -- Uses GNAT.Sockets types

with Ada.Streams;
with Anubis_Node_Types; use Anubis_Node_Types;
with Aegis_VM_Types; use Aegis_VM_Types;
with Node_Contract_Registry; use Node_Contract_Registry;

package Anubis_Main_Buffers is

   ---------------------------------------------------------------------------
   --  Contract Code Buffer (64KB, shrunk from 256KB)
   ---------------------------------------------------------------------------

   Deploy_Code_Buffer : Node_Code_Buffer;

   ---------------------------------------------------------------------------
   --  RPC Request/Response Buffers (128KB each, shrunk from 512KB)
   ---------------------------------------------------------------------------

   --  Request buffer for incoming JSON-RPC
   RPC_Buffer : String (1 .. 131072);  -- 128KB

   --  Receive buffer for socket data
   RPC_Recv_Buf : Ada.Streams.Stream_Element_Array (1 .. 131072);  -- 128KB

   --  Params extraction buffer
   RPC_Params_Str : String (1 .. 131072);  -- 128KB
   RPC_Params_Str_Len : Natural := 0;

   ---------------------------------------------------------------------------
   --  Static RPC Request/Response (100KB total)
   ---------------------------------------------------------------------------

   RPC_Request_Static : RPC_Request;
   RPC_Response_Static : RPC_Response;

   ---------------------------------------------------------------------------
   --  Return Data Buffer (32KB)
   ---------------------------------------------------------------------------

   RPC_Return_Data : Return_Buffer;

   ---------------------------------------------------------------------------
   --  Contract Index (avoids passing 256KB Stored_Contract)
   ---------------------------------------------------------------------------

   RPC_Contract_Index : Stored_Contract_Index := 0;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize all buffers with loop-based zeroing (no stack temporaries)
   procedure Initialize_Buffers;

end Anubis_Main_Buffers;
