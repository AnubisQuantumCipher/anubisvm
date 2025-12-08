pragma SPARK_Mode (On);

with Anubis_Node_Types; use Anubis_Node_Types;
with Aegis_VM_Types; use Aegis_VM_Types;

--  Node JSON Parser: Simple JSON parsing for RPC params
--
--  Extracts known fields from JSON objects without a full parser.
--  Sufficient for the fixed vm_deployContract/vm_invoke schemas.

package Node_JSON_Parser with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Hex Conversion
   ---------------------------------------------------------------------------

   --  Maximum hex string length (2MB code = 4MB hex)
   Max_Hex_Length : constant := 4 * 1024 * 1024;

   --  Convert hex string to bytes
   procedure Hex_To_Bytes (
      Hex       : in     String;
      Bytes     : out    Node_Code_Buffer;
      Byte_Len  : out    Natural;
      Success   : out    Boolean
   ) with
      Global => null,
      Pre    => Hex'Length <= Max_Hex_Length,
      Post   => (Success and then Byte_Len <= Node_Max_Code_Size)
                or not Success;

   --  Convert bytes to hex string (for response)
   procedure Bytes_To_Hex (
      Bytes     : in     Hash256;
      Hex       : out    String;
      Hex_Len   : out    Natural
   ) with
      Global => null,
      Pre    => Hex'Length >= 64,
      Post   => Hex_Len = 64;

   ---------------------------------------------------------------------------
   --  JSON Field Extraction
   ---------------------------------------------------------------------------

   --  Extract string value for a key from JSON
   procedure Extract_String (
      Json      : in     String;
      Key       : in     String;
      Value     : out    String;
      Value_Len : out    Natural;
      Found     : out    Boolean
   ) with
      Global => null,
      Pre    => Json'Length > 0 and Key'Length > 0;

   --  Extract integer value for a key from JSON
   procedure Extract_Integer (
      Json      : in     String;
      Key       : in     String;
      Value     : out    Natural;
      Found     : out    Boolean
   ) with
      Global => null,
      Pre    => Json'Length > 0 and Key'Length > 0;

   ---------------------------------------------------------------------------
   --  Deploy Request Parsing
   ---------------------------------------------------------------------------

   --  Parse vm_deployContract params
   procedure Parse_Deploy_Params (
      Params_Json : in     String;
      From_Addr   : out    Contract_Address;
      Code        : out    Node_Code_Buffer;
      Code_Size   : out    Natural;
      Manifest    : out    Node_Contract_Manifest;
      Gas_Limit   : out    Gas_Amount;
      Success     : out    Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Invoke Request Parsing
   ---------------------------------------------------------------------------

   --  Parse vm_invoke/vm_call params
   procedure Parse_Invoke_Params (
      Params_Json : in     String;
      Request     : out    Invoke_Request;
      Success     : out    Boolean
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Response Formatting
   ---------------------------------------------------------------------------

   --  Format deploy result as JSON
   procedure Format_Deploy_Result (
      Contract_ID : in     Contract_Address;
      Code_Hash   : in     Hash256;
      Gas_Used    : in     Gas_Amount;
      Result_Str  : out    String;
      Result_Len  : out    Natural
   ) with
      Global => null,
      Pre    => Result_Str'Length >= 256;

   --  Format invoke result as JSON
   procedure Format_Invoke_Result (
      Success_Flag : in     Boolean;
      Gas_Used     : in     Gas_Amount;
      Return_Data  : in     Return_Buffer;
      Return_Size  : in     Natural;
      Error_Msg    : in     String;
      Result_Str   : out    String;
      Result_Len   : out    Natural
   ) with
      Global => null,
      Pre    => Result_Str'Length >= 512;

end Node_JSON_Parser;
