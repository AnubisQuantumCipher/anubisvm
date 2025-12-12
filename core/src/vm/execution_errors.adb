pragma SPARK_Mode (On);

package body Execution_Errors with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Error Categories
   ---------------------------------------------------------------------------

   function Get_Category (Code : VM_Error_Code) return Error_Category is
   begin
      case Code is
         when Err_None =>
            return Cat_None;

         when Err_Invalid_Signature .. Err_Invalid_Recipient =>
            return Cat_Validation;

         when Err_Out_Of_Gas .. Err_Call_Depth_Exceeded =>
            return Cat_Execution;

         when Err_Memory_Access .. Err_Return_Data_OOB =>
            return Cat_Memory;

         when Err_Storage_Access .. Err_Storage_Key_Invalid =>
            return Cat_Storage;

         when Err_Contract_Not_Found .. Err_Return_Type_Mismatch =>
            return Cat_Contract;

         when Err_Crypto_Signature .. Err_PQ_Verify_Failed =>
            return Cat_Crypto;

         when Err_Code_Size_Exceeded .. Err_Max_Logs =>
            return Cat_Resource;

         when Err_Internal .. Err_State_Corruption =>
            return Cat_System;
      end case;
   end Get_Category;

   ---------------------------------------------------------------------------
   --  JSON-RPC Error Mapping
   ---------------------------------------------------------------------------

   function To_JSONRPC_Code (Code : VM_Error_Code) return Integer is
   begin
      case Code is
         when Err_None =>
            return 0;

         when Err_Invalid_Signature | Err_PQ_Verify_Failed =>
            return JSONRPC_Invalid_Signature;

         when Err_Invalid_Nonce =>
            return JSONRPC_Nonce_Too_Low;

         when Err_Insufficient_Balance =>
            return JSONRPC_Insufficient_Funds;

         when Err_Out_Of_Gas | Err_Intrinsic_Gas_Too_Low | Err_Gas_Limit_Exceeded =>
            return JSONRPC_Out_Of_Gas;

         when Err_Contract_Reverted | Err_Contract_Panic | Err_Require_Failed =>
            return JSONRPC_Contract_Revert;

         when Err_Contract_Not_Found =>
            return JSONRPC_Contract_Not_Found;

         when Err_Invalid_Args | Err_Return_Type_Mismatch =>
            return JSONRPC_Invalid_Params;

         when Err_Internal | Err_State_Corruption | Err_Not_Implemented =>
            return JSONRPC_Internal_Error;

         when others =>
            return JSONRPC_Execution_Error;
      end case;
   end To_JSONRPC_Code;

   ---------------------------------------------------------------------------
   --  Error Construction
   ---------------------------------------------------------------------------

   function Make_Error (
      Code     : VM_Error_Code;
      Gas_Used : Gas_Amount
   ) return Error_Context is
   begin
      return (
         Code       => Code,
         Message    => (others => ' '),
         Msg_Len    => 0,
         Data       => (others => 0),
         Data_Len   => 0,
         Gas_Used   => Gas_Used,
         PC         => 0,
         Call_Depth => 0
      );
   end Make_Error;

   procedure Make_Error_With_Msg (
      Code     : in     VM_Error_Code;
      Msg      : in     String;
      Gas_Used : in     Gas_Amount;
      Context  : out    Error_Context
   ) is
   begin
      Context := (
         Code       => Code,
         Message    => (others => ' '),
         Msg_Len    => Msg'Length,
         Data       => (others => 0),
         Data_Len   => 0,
         Gas_Used   => Gas_Used,
         PC         => 0,
         Call_Depth => 0
      );

      --  Copy message
      for I in 1 .. Msg'Length loop
         Context.Message (I) := Msg (Msg'First + I - 1);
      end loop;
   end Make_Error_With_Msg;

   procedure Make_Revert_Error (
      Data     : in     Error_Data_Buffer;
      Data_Len : in     Natural;
      Gas_Used : in     Gas_Amount;
      Context  : out    Error_Context
   ) is
   begin
      Context := (
         Code       => Err_Contract_Reverted,
         Message    => (others => ' '),
         Msg_Len    => 0,
         Data       => Data,
         Data_Len   => Data_Len,
         Gas_Used   => Gas_Used,
         PC         => 0,
         Call_Depth => 0
      );
   end Make_Revert_Error;

   ---------------------------------------------------------------------------
   --  Error Checking
   ---------------------------------------------------------------------------

   function Is_Recoverable (Code : VM_Error_Code) return Boolean is
   begin
      case Code is
         --  Validation errors: no state was modified
         when Err_Invalid_Signature | Err_Invalid_Nonce | Err_Invalid_Chain_ID |
              Err_Insufficient_Balance | Err_Intrinsic_Gas_Too_Low |
              Err_Gas_Limit_Exceeded | Err_Invalid_Sender |
              Err_Tx_Size_Exceeded | Err_Invalid_Recipient =>
            return True;

         --  Contract reverts: state rolled back by design
         when Err_Contract_Reverted | Err_Require_Failed =>
            return True;

         --  Execution errors: depends on error type
         when Err_Out_Of_Gas | Err_Stack_Overflow | Err_Call_Depth_Exceeded =>
            return True;  -- State is rolled back

         --  System errors: not recoverable
         when Err_Internal | Err_State_Corruption =>
            return False;

         --  All others: typically recoverable
         when others =>
            return True;
      end case;
   end Is_Recoverable;

   function Consumes_All_Gas (Code : VM_Error_Code) return Boolean is
   begin
      case Code is
         --  Fatal errors that consume all remaining gas
         when Err_Out_Of_Gas | Err_Invalid_Opcode | Err_Stack_Overflow |
              Err_Stack_Underflow | Err_Invalid_Jump | Err_Write_Protection |
              Err_Call_Depth_Exceeded | Err_Memory_Access | Err_Memory_Limit |
              Err_Storage_Access | Err_Division_By_Zero | Err_Contract_Panic =>
            return True;

         --  Contract reverts refund remaining gas
         when Err_Contract_Reverted | Err_Require_Failed =>
            return False;

         --  Validation errors don't consume gas
         when Err_Invalid_Signature | Err_Invalid_Nonce | Err_Invalid_Chain_ID |
              Err_Insufficient_Balance | Err_Intrinsic_Gas_Too_Low |
              Err_Gas_Limit_Exceeded | Err_Invalid_Sender |
              Err_Tx_Size_Exceeded | Err_Invalid_Recipient =>
            return False;

         --  Default: consume all gas for safety
         when others =>
            return True;
      end case;
   end Consumes_All_Gas;

   ---------------------------------------------------------------------------
   --  Standard Error Messages
   ---------------------------------------------------------------------------

   function Default_Message (Code : VM_Error_Code) return String is
   begin
      case Code is
         when Err_None =>
            return "Success";

         --  Transaction validation
         when Err_Invalid_Signature =>
            return "ML-DSA-87 signature verification failed";
         when Err_Invalid_Nonce =>
            return "Invalid nonce";
         when Err_Invalid_Chain_ID =>
            return "Invalid chain ID";
         when Err_Insufficient_Balance =>
            return "Insufficient balance";
         when Err_Intrinsic_Gas_Too_Low =>
            return "Intrinsic gas too low";
         when Err_Gas_Limit_Exceeded =>
            return "Gas limit exceeds block limit";
         when Err_Invalid_Sender =>
            return "Invalid sender address";
         when Err_Tx_Size_Exceeded =>
            return "Transaction size exceeded";
         when Err_Invalid_Recipient =>
            return "Invalid recipient address";

         --  Execution
         when Err_Out_Of_Gas =>
            return "Out of gas";
         when Err_Stack_Overflow =>
            return "Stack overflow";
         when Err_Stack_Underflow =>
            return "Stack underflow";
         when Err_Invalid_Opcode =>
            return "Invalid opcode";
         when Err_Invalid_Jump =>
            return "Invalid jump destination";
         when Err_Write_Protection =>
            return "Write protection violation";
         when Err_Call_Depth_Exceeded =>
            return "Call depth exceeded";

         --  Memory
         when Err_Memory_Access =>
            return "Memory access out of bounds";
         when Err_Memory_Limit =>
            return "Memory limit exceeded";
         when Err_Invalid_Offset =>
            return "Invalid memory offset";
         when Err_Return_Data_OOB =>
            return "Return data out of bounds";

         --  Storage
         when Err_Storage_Access =>
            return "Storage access denied";
         when Err_Storage_Read_Only =>
            return "Storage is read-only";
         when Err_Storage_Key_Invalid =>
            return "Invalid storage key";

         --  Contract
         when Err_Contract_Not_Found =>
            return "Contract not found";
         when Err_Entry_Point_Not_Found =>
            return "Entry point not found";
         when Err_Contract_Reverted =>
            return "Execution reverted";
         when Err_Contract_Panic =>
            return "Panic";
         when Err_Require_Failed =>
            return "Require condition failed";
         when Err_Division_By_Zero =>
            return "Division by zero";
         when Err_Invalid_Args =>
            return "Invalid arguments";
         when Err_Return_Type_Mismatch =>
            return "Return type mismatch";

         --  Crypto
         when Err_Crypto_Signature =>
            return "Signature operation failed";
         when Err_Crypto_Hash =>
            return "Hash operation failed";
         when Err_Crypto_Encryption =>
            return "Encryption failed";
         when Err_Crypto_Decryption =>
            return "Decryption failed";
         when Err_Crypto_Key_Invalid =>
            return "Invalid cryptographic key";
         when Err_PQ_Verify_Failed =>
            return "Post-quantum verification failed";

         --  Resource
         when Err_Code_Size_Exceeded =>
            return "Contract code size exceeded";
         when Err_Init_Code_Exceeded =>
            return "Init code size exceeded";
         when Err_Max_Contracts =>
            return "Maximum contracts reached";
         when Err_Max_Events =>
            return "Maximum events exceeded";
         when Err_Max_Logs =>
            return "Maximum logs exceeded";

         --  System
         when Err_Internal =>
            return "Internal error";
         when Err_Not_Implemented =>
            return "Not implemented";
         when Err_State_Corruption =>
            return "State corruption detected";
      end case;
   end Default_Message;

end Execution_Errors;
