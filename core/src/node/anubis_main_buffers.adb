--  Anubis Main Buffers: Implementation
--
--  Uses loop-based initialization to avoid stack temporaries
--  from aggregate assignments like (others => 0).

pragma SPARK_Mode (Off);

with Ada.Streams;
with Anubis_Node_Types; use Anubis_Node_Types;
with Aegis_VM_Types; use Aegis_VM_Types;

package body Anubis_Main_Buffers is

   procedure Initialize_Buffers is
   begin
      --  Zero Deploy_Code_Buffer (256KB) with loop
      for I in Deploy_Code_Buffer'Range loop
         Deploy_Code_Buffer (I) := 0;
      end loop;

      --  Clear RPC_Buffer (512KB) with loop
      for I in RPC_Buffer'Range loop
         RPC_Buffer (I) := ' ';
      end loop;

      --  Zero RPC_Recv_Buf (512KB) with loop
      for I in RPC_Recv_Buf'Range loop
         RPC_Recv_Buf (I) := 0;
      end loop;

      --  Clear RPC_Params_Str (512KB) with loop
      for I in RPC_Params_Str'Range loop
         RPC_Params_Str (I) := ' ';
      end loop;
      RPC_Params_Str_Len := 0;

      --  Initialize RPC_Request_Static field by field
      RPC_Request_Static.Version := RPC_Version_2_0;
      RPC_Request_Static.Method := Method_Unknown;
      for I in RPC_Request_Static.Method_Str'Range loop
         RPC_Request_Static.Method_Str (I) := ' ';
      end loop;
      RPC_Request_Static.Method_Len := 0;
      for I in RPC_Request_Static.ID'Range loop
         RPC_Request_Static.ID (I) := ' ';
      end loop;
      RPC_Request_Static.ID_Len := 0;
      for I in RPC_Request_Static.Params'Range loop
         RPC_Request_Static.Params (I) := 0;
      end loop;
      RPC_Request_Static.Params_Size := 0;

      --  Initialize RPC_Response_Static field by field
      RPC_Response_Static.Version := RPC_Version_2_0;
      for I in RPC_Response_Static.ID'Range loop
         RPC_Response_Static.ID (I) := ' ';
      end loop;
      RPC_Response_Static.ID_Len := 0;
      RPC_Response_Static.Has_Result := False;
      for I in RPC_Response_Static.Result'Range loop
         RPC_Response_Static.Result (I) := 0;
      end loop;
      RPC_Response_Static.Result_Size := 0;
      RPC_Response_Static.Error_Code := Error_None;
      for I in RPC_Response_Static.Error_Msg'Range loop
         RPC_Response_Static.Error_Msg (I) := ' ';
      end loop;
      RPC_Response_Static.Error_Msg_Len := 0;

      --  Zero RPC_Return_Data (32KB) with loop
      for I in RPC_Return_Data'Range loop
         RPC_Return_Data (I) := 0;
      end loop;

      RPC_Contract_Index := 0;
   end Initialize_Buffers;

end Anubis_Main_Buffers;
