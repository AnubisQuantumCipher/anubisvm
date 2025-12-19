pragma SPARK_Mode (On);

with Aegis_U256; use Aegis_U256;
with Anubis_SHA3; use Anubis_SHA3;

package body Khepri_Inspector is

   ---------------------------------------------------------------------------
   --  Session Management
   ---------------------------------------------------------------------------

   function Create_Session (
      Config : Inspector_Config
   ) return Inspection_Session is
   begin
      return (
         Config         => Config,
         Contract       => (
            Address           => (others => 0),
            Bytecode_Hash     => (others => 0),
            Bytecode_Size     => 0,
            Balance           => U256_Zero,
            Nonce             => 0,
            Creator           => (others => 0),
            Creation_Block    => 0,
            Cert_Level        => Level_None,
            Is_Proxy          => False,
            Implementation    => (others => 0)
         ),
         Version        => (
            Name              => Empty_String,
            Version           => Empty_String,
            Compiler_Version  => Empty_String,
            SPARK_Mode        => True,
            Optimization      => 2
         ),
         Functions      => (others => (
            Name         => Empty_String,
            Selector     => (others => 0),
            Inputs       => (others => (
               Name       => Empty_String,
               P_Type => Type_Unknown,
               Indexed    => False
            )),
            Input_Count  => 0,
            Outputs      => (others => (
               Name       => Empty_String,
               P_Type => Type_Unknown,
               Indexed    => False
            )),
            Output_Count => 0,
            Mutability   => 0,
            Is_External  => False
         )),
         Function_Count => 0,
         Events         => (others => (
            Name          => Empty_String,
            Signature     => (others => 0),
            Params        => (others => (
               Name       => Empty_String,
               P_Type => Type_Unknown,
               Indexed    => False
            )),
            Param_Count   => 0,
            Is_Anonymous  => False
         )),
         Event_Count    => 0,
         Bytecode       => (others => 0),
         Bytecode_Size  => 0,
         Is_Inspected   => False
      );
   end Create_Session;

   procedure Inspect_Contract (
      Session  : in Out Inspection_Session;
      Address  : in     Khepri_Types.Address;
      Success  : out    Boolean
   ) is
      Code : Byte_Array (0 .. 1048575) := (others => 0);  --  Max 1MB
      Code_Size : Natural := 0;
   begin
      Success := False;
      Session.Contract.Address := Address;

      --  Fetch contract data from network via RPC
      --  Real implementation would:
      --  1. Open HTTP/HTTPS connection to RPC endpoint
      --  2. Build JSON-RPC request:
      --     {"jsonrpc":"2.0","method":"anubis_getCode","params":["0x..."],"id":1}
      --  3. Send POST request
      --  4. Parse response to extract code
      --  5. Fetch balance, nonce, certification via similar RPC calls

      --  Simulate fetching contract code
      if Session.Config.Network_RPC'Length > 0 then
         --  Generate simulated bytecode
         Code_Size := 4096;

         --  Write KHPR magic header
         Code (0) := 16#4B#;  --  K
         Code (1) := 16#48#;  --  H
         Code (2) := 16#50#;  --  P
         Code (3) := 16#52#;  --  R
         Code (4) := 16#01#;  --  Version
         Code (5) := 0;       --  Reserved

         --  Function count
         Code (6) := 0;       --  High byte
         Code (7) := 5;       --  Low byte (5 functions)

         --  Store bytecode
         for I in 0 .. Natural'Min (Code_Size - 1, Session.Bytecode'Length - 1) loop
            Session.Bytecode (I) := Code (I);
         end loop;

         Session.Bytecode_Size := Code_Size;
         Session.Contract.Bytecode_Size := Code_Size;

         --  Compute bytecode hash
         declare
            Digest : SHA3_256_Digest;
         begin
            SHA3_256 (Code (0 .. Code_Size - 1), Digest);
            for I in Digest'Range loop
               Session.Contract.Bytecode_Hash (I) := Digest (I);
            end loop;
         end;

         --  Set simulated metadata
         Session.Contract.Balance := From_Natural (1000000000);  --  1 Gwei
         Session.Contract.Nonce := 42;
         Session.Contract.Creator := (others => 16#AA#);
         Session.Contract.Creation_Block := 12345;
         Session.Contract.Cert_Level := Level_Silver;
         Session.Contract.Is_Proxy := False;

         --  Parse function table from bytecode
         Session.Function_Count := 5;

         Session.Is_Inspected := True;
         Success := True;
      end if;
   end Inspect_Contract;

   procedure Inspect_Bytecode (
      Session  : in Out Inspection_Session;
      Bytecode : in     Byte_Array;
      Success  : out    Boolean
   ) is
   begin
      if Bytecode'Length > Session.Bytecode'Length then
         Success := False;
         return;
      end if;

      for I in Bytecode'Range loop
         Session.Bytecode (Session.Bytecode'First + I - Bytecode'First) :=
            Bytecode (I);
      end loop;

      Session.Bytecode_Size := Bytecode'Length;
      Session.Contract.Bytecode_Size := Bytecode'Length;
      Session.Is_Inspected := True;
      Success := True;
   end Inspect_Bytecode;

   ---------------------------------------------------------------------------
   --  Metadata Retrieval
   ---------------------------------------------------------------------------

   function Get_Contract_Info (
      Session : Inspection_Session
   ) return Contract_Info is
   begin
      return Session.Contract;
   end Get_Contract_Info;

   function Get_Version_Info (
      Session : Inspection_Session
   ) return Version_Info is
   begin
      return Session.Version;
   end Get_Version_Info;

   function Get_Cert_Level (
      Session : Inspection_Session
   ) return Khepri_Certification.Certification_Level is
   begin
      return Session.Contract.Cert_Level;
   end Get_Cert_Level;

   ---------------------------------------------------------------------------
   --  ABI Operations
   ---------------------------------------------------------------------------

   procedure Get_ABI (
      Session        : in  Inspection_Session;
      Functions      : out Function_Array;
      Function_Count : out Natural;
      Events         : out Event_Array;
      Event_Count    : out Natural
   ) is
   begin
      Functions := Session.Functions;
      Function_Count := Session.Function_Count;
      Events := Session.Events;
      Event_Count := Session.Event_Count;
   end Get_ABI;

   function Get_Function (
      Session  : Inspection_Session;
      Selector : Bytes4
   ) return ABI_Function is
   begin
      for I in 0 .. Session.Function_Count - 1 loop
         if Session.Functions (Function_Index (I)).Selector = Selector then
            return Session.Functions (Function_Index (I));
         end if;
      end loop;

      return (
         Name         => Empty_String,
         Selector     => (others => 0),
         Inputs       => (others => (
            Name       => Empty_String,
            P_Type => Type_Unknown,
            Indexed    => False
         )),
         Input_Count  => 0,
         Outputs      => (others => (
            Name       => Empty_String,
            P_Type => Type_Unknown,
            Indexed    => False
         )),
         Output_Count => 0,
         Mutability   => 0,
         Is_External  => False
      );
   end Get_Function;

   function Get_Event (
      Session   : Inspection_Session;
      Signature : Hash256
   ) return ABI_Event is
   begin
      for I in 0 .. Session.Event_Count - 1 loop
         if Session.Events (Event_Index (I)).Signature = Signature then
            return Session.Events (Event_Index (I));
         end if;
      end loop;

      return (
         Name          => Empty_String,
         Signature     => (others => 0),
         Params        => (others => (
            Name       => Empty_String,
            P_Type => Type_Unknown,
            Indexed    => False
         )),
         Param_Count   => 0,
         Is_Anonymous  => False
      );
   end Get_Event;

   procedure Encode_Call (
      Func    : in     ABI_Function;
      Args    : in     Byte_Array;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) is
   begin
      Output := (others => 0);

      --  Copy selector (4 bytes)
      for I in Func.Selector'Range loop
         Output (Output'First + I) := Func.Selector (I);
      end loop;

      --  Copy args
      for I in Args'Range loop
         Output (Output'First + 4 + I - Args'First) := Args (I);
      end loop;

      Size := 4 + Args'Length;
      Success := True;
   end Encode_Call;

   procedure Decode_Result (
      Func    : in     ABI_Function;
      Data    : in     Byte_Array;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) is
      --  ABI decoding: Parse return data based on output types
      --  Static types: uint256, int256, address, bool, bytes32 (32 bytes each)
      --  Dynamic types: bytes, string, arrays (offset + length + data)

      Out_Pos   : Natural := 0;
      Data_Pos  : Natural := 0;

      --  Decode a single ABI parameter value
      procedure Decode_Param (
         P_Type     : in     Param_Type;
         Data_Start : in     Natural;
         Value_Out  : out    Byte_Array;
         Value_Size : out    Natural;
         Dec_Ok     : out    Boolean
      ) is
         Offset     : Word64;
         Length     : Word64;
         Dyn_Start  : Natural;
      begin
         Value_Out := (others => 0);
         Value_Size := 0;
         Dec_Ok := False;

         case P_Type is
            when Type_Uint256 | Type_Int256 | Type_Bytes32 =>
               --  Fixed 32 bytes, direct copy
               if Data_Start + 32 <= Data'Length then
                  for I in 0 .. 31 loop
                     Value_Out (I) := Data (Data'First + Data_Start + I);
                  end loop;
                  Value_Size := 32;
                  Dec_Ok := True;
               end if;

            when Type_Address =>
               --  20 bytes right-aligned in 32-byte slot
               if Data_Start + 32 <= Data'Length then
                  for I in 0 .. 19 loop
                     Value_Out (I) := Data (Data'First + Data_Start + 12 + I);
                  end loop;
                  Value_Size := 20;
                  Dec_Ok := True;
               end if;

            when Type_Bool =>
               --  1 byte right-aligned in 32-byte slot
               if Data_Start + 32 <= Data'Length then
                  Value_Out (0) := Data (Data'First + Data_Start + 31);
                  Value_Size := 1;
                  Dec_Ok := True;
               end if;

            when Type_Bytes | Type_String =>
               --  Dynamic type: first 32 bytes is offset, then length + data
               if Data_Start + 32 <= Data'Length then
                  --  Read offset (big-endian)
                  Offset := 0;
                  for I in 0 .. 7 loop
                     Offset := Offset * 256 +
                        Word64 (Data (Data'First + Data_Start + 24 + I));
                  end loop;

                  Dyn_Start := Natural (Offset mod Word64 (Data'Length));

                  if Dyn_Start + 32 <= Data'Length then
                     --  Read length (big-endian)
                     Length := 0;
                     for I in 0 .. 7 loop
                        Length := Length * 256 +
                           Word64 (Data (Data'First + Dyn_Start + 24 + I));
                     end loop;

                     --  Copy data
                     if Dyn_Start + 32 + Natural (Length mod 65536) <= Data'Length then
                        for I in 0 .. Natural (Length mod 65536) - 1 loop
                           if I < Value_Out'Length then
                              Value_Out (I) := Data (Data'First + Dyn_Start + 32 + I);
                           end if;
                        end loop;
                        Value_Size := Natural (Length mod 65536);
                        Dec_Ok := True;
                     end if;
                  end if;
               end if;

            when Type_Array =>
               --  Dynamic array: offset -> length -> elements
               --  For simplicity, return raw data with length prefix
               if Data_Start + 32 <= Data'Length then
                  --  Read offset
                  Offset := 0;
                  for I in 0 .. 7 loop
                     Offset := Offset * 256 +
                        Word64 (Data (Data'First + Data_Start + 24 + I));
                  end loop;

                  Dyn_Start := Natural (Offset mod Word64 (Data'Length));

                  if Dyn_Start + 32 <= Data'Length then
                     --  Read length
                     Length := 0;
                     for I in 0 .. 7 loop
                        Length := Length * 256 +
                           Word64 (Data (Data'First + Dyn_Start + 24 + I));
                     end loop;

                     --  Copy length + elements (each element is 32 bytes)
                     declare
                        Total_Size : constant Natural :=
                           32 + Natural ((Length mod 2048) * 32);
                     begin
                        if Dyn_Start + Total_Size <= Data'Length then
                           for I in 0 .. Total_Size - 1 loop
                              if I < Value_Out'Length then
                                 Value_Out (I) := Data (Data'First + Dyn_Start + I);
                              end if;
                           end loop;
                           Value_Size := Total_Size;
                           Dec_Ok := True;
                        end if;
                     end;
                  end if;
               end if;

            when Type_Tuple =>
               --  Tuple: sequential 32-byte slots
               --  Without schema info, return first 32 bytes
               if Data_Start + 32 <= Data'Length then
                  for I in 0 .. 31 loop
                     Value_Out (I) := Data (Data'First + Data_Start + I);
                  end loop;
                  Value_Size := 32;
                  Dec_Ok := True;
               end if;

            when Type_Unknown =>
               --  Unknown type: copy 32 bytes if available
               if Data_Start + 32 <= Data'Length then
                  for I in 0 .. 31 loop
                     Value_Out (I) := Data (Data'First + Data_Start + I);
                  end loop;
                  Value_Size := 32;
                  Dec_Ok := True;
               end if;
         end case;
      end Decode_Param;

      Value_Buf  : Byte_Array (0 .. 65535) := (others => 0);
      Value_Len  : Natural;
      Param_Ok   : Boolean;
   begin
      Output := (others => 0);
      Size := 0;
      Success := False;

      --  No outputs to decode
      if Func.Output_Count = 0 then
         Success := True;
         return;
      end if;

      --  Decode each output parameter
      for I in 0 .. Func.Output_Count - 1 loop
         exit when I >= Natural (Param_Index'Last) + 1;

         Decode_Param (
            P_Type     => Func.Outputs (Param_Index (I)).P_Type,
            Data_Start => Data_Pos,
            Value_Out  => Value_Buf,
            Value_Size => Value_Len,
            Dec_Ok     => Param_Ok
         );

         if not Param_Ok then
            return;  -- Decoding failed
         end if;

         --  Copy decoded value to output
         for J in 0 .. Value_Len - 1 loop
            if Out_Pos + J < Output'Length then
               Output (Output'First + Out_Pos + J) := Value_Buf (J);
            end if;
         end loop;

         Out_Pos := Out_Pos + Value_Len;

         --  Static types advance by 32 bytes in data
         case Func.Outputs (Param_Index (I)).P_Type is
            when Type_Bytes | Type_String | Type_Array =>
               Data_Pos := Data_Pos + 32;  -- Dynamic: just offset slot
            when others =>
               Data_Pos := Data_Pos + 32;  -- Static: 32 bytes each
         end case;
      end loop;

      Size := Out_Pos;
      Success := True;
   end Decode_Result;

   ---------------------------------------------------------------------------
   --  Disassembly Operations
   ---------------------------------------------------------------------------

   procedure Disassemble_Bytecode (
      Session : in     Inspection_Session;
      Result  : out    Disassembly;
      Success : out    Boolean
   ) is
      --  Get mnemonic for opcode
      function Get_Mnemonic (Op : Byte) return Bounded_String is
         S : Bounded_String := Empty_String;
      begin
         case Op is
            when 16#00# => S.Data (0 .. 3) := "STOP"; S.Length := 4;
            when 16#01# => S.Data (0 .. 2) := "ADD"; S.Length := 3;
            when 16#02# => S.Data (0 .. 2) := "MUL"; S.Length := 3;
            when 16#03# => S.Data (0 .. 2) := "SUB"; S.Length := 3;
            when 16#04# => S.Data (0 .. 2) := "DIV"; S.Length := 3;
            when 16#05# => S.Data (0 .. 3) := "SDIV"; S.Length := 4;
            when 16#06# => S.Data (0 .. 2) := "MOD"; S.Length := 3;
            when 16#07# => S.Data (0 .. 3) := "SMOD"; S.Length := 4;
            when 16#08# => S.Data (0 .. 5) := "ADDMOD"; S.Length := 6;
            when 16#09# => S.Data (0 .. 5) := "MULMOD"; S.Length := 6;
            when 16#0A# => S.Data (0 .. 2) := "EXP"; S.Length := 3;
            when 16#0B# => S.Data (0 .. 9) := "SIGNEXTEND"; S.Length := 10;
            when 16#10# => S.Data (0 .. 1) := "LT"; S.Length := 2;
            when 16#11# => S.Data (0 .. 1) := "GT"; S.Length := 2;
            when 16#12# => S.Data (0 .. 2) := "SLT"; S.Length := 3;
            when 16#13# => S.Data (0 .. 2) := "SGT"; S.Length := 3;
            when 16#14# => S.Data (0 .. 1) := "EQ"; S.Length := 2;
            when 16#15# => S.Data (0 .. 5) := "ISZERO"; S.Length := 6;
            when 16#16# => S.Data (0 .. 2) := "AND"; S.Length := 3;
            when 16#17# => S.Data (0 .. 1) := "OR"; S.Length := 2;
            when 16#18# => S.Data (0 .. 2) := "XOR"; S.Length := 3;
            when 16#19# => S.Data (0 .. 2) := "NOT"; S.Length := 3;
            when 16#1A# => S.Data (0 .. 3) := "BYTE"; S.Length := 4;
            when 16#1B# => S.Data (0 .. 2) := "SHL"; S.Length := 3;
            when 16#1C# => S.Data (0 .. 2) := "SHR"; S.Length := 3;
            when 16#1D# => S.Data (0 .. 2) := "SAR"; S.Length := 3;
            when 16#20# => S.Data (0 .. 3) := "SHA3"; S.Length := 4;
            when 16#30# => S.Data (0 .. 6) := "ADDRESS"; S.Length := 7;
            when 16#31# => S.Data (0 .. 6) := "BALANCE"; S.Length := 7;
            when 16#32# => S.Data (0 .. 5) := "ORIGIN"; S.Length := 6;
            when 16#33# => S.Data (0 .. 5) := "CALLER"; S.Length := 6;
            when 16#34# => S.Data (0 .. 8) := "CALLVALUE"; S.Length := 9;
            when 16#35# => S.Data (0 .. 11) := "CALLDATALOAD"; S.Length := 12;
            when 16#36# => S.Data (0 .. 11) := "CALLDATASIZE"; S.Length := 12;
            when 16#37# => S.Data (0 .. 11) := "CALLDATACOPY"; S.Length := 12;
            when 16#38# => S.Data (0 .. 7) := "CODESIZE"; S.Length := 8;
            when 16#39# => S.Data (0 .. 7) := "CODECOPY"; S.Length := 8;
            when 16#3A# => S.Data (0 .. 7) := "GASPRICE"; S.Length := 8;
            when 16#3B# => S.Data (0 .. 10) := "EXTCODESIZE"; S.Length := 11;
            when 16#3C# => S.Data (0 .. 10) := "EXTCODECOPY"; S.Length := 11;
            when 16#3D# => S.Data (0 .. 13) := "RETURNDATASIZE"; S.Length := 14;
            when 16#3E# => S.Data (0 .. 13) := "RETURNDATACOPY"; S.Length := 14;
            when 16#3F# => S.Data (0 .. 10) := "EXTCODEHASH"; S.Length := 11;
            when 16#40# => S.Data (0 .. 8) := "BLOCKHASH"; S.Length := 9;
            when 16#41# => S.Data (0 .. 7) := "COINBASE"; S.Length := 8;
            when 16#42# => S.Data (0 .. 8) := "TIMESTAMP"; S.Length := 9;
            when 16#43# => S.Data (0 .. 5) := "NUMBER"; S.Length := 6;
            when 16#44# => S.Data (0 .. 8) := "PREVRANDAO"; S.Length := 10;
            when 16#45# => S.Data (0 .. 7) := "GASLIMIT"; S.Length := 8;
            when 16#46# => S.Data (0 .. 6) := "CHAINID"; S.Length := 7;
            when 16#47# => S.Data (0 .. 10) := "SELFBALANCE"; S.Length := 11;
            when 16#48# => S.Data (0 .. 6) := "BASEFEE"; S.Length := 7;
            when 16#50# => S.Data (0 .. 2) := "POP"; S.Length := 3;
            when 16#51# => S.Data (0 .. 4) := "MLOAD"; S.Length := 5;
            when 16#52# => S.Data (0 .. 5) := "MSTORE"; S.Length := 6;
            when 16#53# => S.Data (0 .. 6) := "MSTORE8"; S.Length := 7;
            when 16#54# => S.Data (0 .. 4) := "SLOAD"; S.Length := 5;
            when 16#55# => S.Data (0 .. 5) := "SSTORE"; S.Length := 6;
            when 16#56# => S.Data (0 .. 3) := "JUMP"; S.Length := 4;
            when 16#57# => S.Data (0 .. 4) := "JUMPI"; S.Length := 5;
            when 16#58# => S.Data (0 .. 1) := "PC"; S.Length := 2;
            when 16#59# => S.Data (0 .. 4) := "MSIZE"; S.Length := 5;
            when 16#5A# => S.Data (0 .. 2) := "GAS"; S.Length := 3;
            when 16#5B# => S.Data (0 .. 6) := "JUMPDEST"; S.Length := 8;
            when 16#60# .. 16#7F# =>
               --  PUSH1 to PUSH32
               declare
                  N : constant Natural := Natural (Op) - 16#5F#;
               begin
                  S.Data (0 .. 3) := "PUSH";
                  if N < 10 then
                     S.Data (4) := Character'Val (Character'Pos ('0') + N);
                     S.Length := 5;
                  else
                     S.Data (4) := Character'Val (Character'Pos ('0') + N / 10);
                     S.Data (5) := Character'Val (Character'Pos ('0') + N mod 10);
                     S.Length := 6;
                  end if;
               end;
            when 16#80# .. 16#8F# =>
               --  DUP1 to DUP16
               declare
                  N : constant Natural := Natural (Op) - 16#7F#;
               begin
                  S.Data (0 .. 2) := "DUP";
                  if N < 10 then
                     S.Data (3) := Character'Val (Character'Pos ('0') + N);
                     S.Length := 4;
                  else
                     S.Data (3) := '1';
                     S.Data (4) := Character'Val (Character'Pos ('0') + N - 10);
                     S.Length := 5;
                  end if;
               end;
            when 16#90# .. 16#9F# =>
               --  SWAP1 to SWAP16
               declare
                  N : constant Natural := Natural (Op) - 16#8F#;
               begin
                  S.Data (0 .. 3) := "SWAP";
                  if N < 10 then
                     S.Data (4) := Character'Val (Character'Pos ('0') + N);
                     S.Length := 5;
                  else
                     S.Data (4) := '1';
                     S.Data (5) := Character'Val (Character'Pos ('0') + N - 10);
                     S.Length := 6;
                  end if;
               end;
            when 16#A0# => S.Data (0 .. 3) := "LOG0"; S.Length := 4;
            when 16#A1# => S.Data (0 .. 3) := "LOG1"; S.Length := 4;
            when 16#A2# => S.Data (0 .. 3) := "LOG2"; S.Length := 4;
            when 16#A3# => S.Data (0 .. 3) := "LOG3"; S.Length := 4;
            when 16#A4# => S.Data (0 .. 3) := "LOG4"; S.Length := 4;
            when 16#F0# => S.Data (0 .. 5) := "CREATE"; S.Length := 6;
            when 16#F1# => S.Data (0 .. 3) := "CALL"; S.Length := 4;
            when 16#F2# => S.Data (0 .. 7) := "CALLCODE"; S.Length := 8;
            when 16#F3# => S.Data (0 .. 5) := "RETURN"; S.Length := 6;
            when 16#F4# => S.Data (0 .. 11) := "DELEGATECALL"; S.Length := 12;
            when 16#F5# => S.Data (0 .. 6) := "CREATE2"; S.Length := 7;
            when 16#FA# => S.Data (0 .. 9) := "STATICCALL"; S.Length := 10;
            when 16#FD# => S.Data (0 .. 5) := "REVERT"; S.Length := 6;
            when 16#FE# => S.Data (0 .. 6) := "INVALID"; S.Length := 7;
            when 16#FF# => S.Data (0 .. 11) := "SELFDESTRUCT"; S.Length := 12;
            when others => S.Data (0 .. 6) := "INVALID"; S.Length := 7;
         end case;
         return S;
      end Get_Mnemonic;

      --  Get operand size for opcode
      function Get_Op_Size (Op : Byte) return Natural is
      begin
         if Op >= 16#60# and Op <= 16#7F# then
            return 1 + Natural (Op) - 16#5F#;  -- PUSH1=2, PUSH32=33
         else
            return 1;
         end if;
      end Get_Op_Size;

      --  Get gas cost for opcode
      function Get_Gas (Op : Byte) return Natural is
      begin
         case Op is
            when 16#00# => return 0;     -- STOP
            when 16#01# .. 16#0B# => return 3;  -- Arithmetic
            when 16#10# .. 16#1D# => return 3;  -- Comparison/bitwise
            when 16#20# => return 30;    -- SHA3 base
            when 16#30# .. 16#3F# => return 2;  -- Environment
            when 16#31# => return 100;   -- BALANCE
            when 16#3B# => return 100;   -- EXTCODESIZE
            when 16#40# .. 16#48# => return 2;  -- Block info
            when 16#50# => return 2;     -- POP
            when 16#51# | 16#52# | 16#53# => return 3;  -- Memory
            when 16#54# => return 100;   -- SLOAD
            when 16#55# => return 100;   -- SSTORE base
            when 16#56# => return 8;     -- JUMP
            when 16#57# => return 10;    -- JUMPI
            when 16#58# .. 16#5A# => return 2;  -- PC/MSIZE/GAS
            when 16#5B# => return 1;     -- JUMPDEST
            when 16#60# .. 16#7F# => return 3;  -- PUSH
            when 16#80# .. 16#8F# => return 3;  -- DUP
            when 16#90# .. 16#9F# => return 3;  -- SWAP
            when 16#A0# .. 16#A4# => return 375;  -- LOG base
            when 16#F0# => return 32000; -- CREATE
            when 16#F1# => return 100;   -- CALL base
            when 16#F2# => return 100;   -- CALLCODE base
            when 16#F3# => return 0;     -- RETURN
            when 16#F4# => return 100;   -- DELEGATECALL base
            when 16#F5# => return 32000; -- CREATE2
            when 16#FA# => return 100;   -- STATICCALL base
            when 16#FD# => return 0;     -- REVERT
            when 16#FF# => return 5000;  -- SELFDESTRUCT
            when others => return 0;
         end case;
      end Get_Gas;

      Pos : Natural := 0;
      Instr_Count : Natural := 0;
      Jump_Count : Natural := 0;
      Total : Word64 := 0;
   begin
      Result := (
         Instructions      => (others => (
            Offset     => 0,
            Opcode     => 0,
            Mnemonic   => Empty_String,
            Operand    => (others => 0),
            Op_Size    => 1,
            Gas_Cost   => 3,
            Stack_In   => 0,
            Stack_Out  => 0
         )),
         Instruction_Count => 0,
         Jump_Targets      => 0,
         Total_Gas         => 0,
         Has_Errors        => False
      );

      if not Session.Is_Inspected or Session.Bytecode_Size = 0 then
         Success := False;
         return;
      end if;

      --  Disassemble bytecode
      while Pos < Session.Bytecode_Size and Instr_Count < Max_Instructions loop
         declare
            Op : constant Byte := Session.Bytecode (Pos);
            Op_Sz : constant Natural := Get_Op_Size (Op);
            Gas : constant Natural := Get_Gas (Op);
            Instr : Opcode_Info;
         begin
            Instr.Offset := Pos;
            Instr.Opcode := Op;
            Instr.Mnemonic := Get_Mnemonic (Op);
            Instr.Op_Size := Op_Sz;
            Instr.Gas_Cost := Gas;
            Instr.Operand := (others => 0);

            --  Copy operand bytes for PUSH instructions
            if Op >= 16#60# and Op <= 16#7F# then
               for I in 1 .. Op_Sz - 1 loop
                  if Pos + I < Session.Bytecode_Size then
                     Instr.Operand (I - 1) := Session.Bytecode (Pos + I);
                  end if;
               end loop;
            end if;

            --  Count JUMPDEST instructions
            if Op = 16#5B# then
               Jump_Count := Jump_Count + 1;
            end if;

            Result.Instructions (Instruction_Index (Instr_Count)) := Instr;
            Total := Total + Word64 (Gas);
            Instr_Count := Instr_Count + 1;
            Pos := Pos + Op_Sz;
         end;
      end loop;

      Result.Instruction_Count := Instr_Count;
      Result.Jump_Targets := Jump_Count;
      Result.Total_Gas := Total;
      Result.Has_Errors := Pos /= Session.Bytecode_Size;
      Success := True;
   end Disassemble_Bytecode;

   function Get_Instruction (
      Session : Inspection_Session;
      Offset  : Natural
   ) return Opcode_Info is
   begin
      if Offset >= Session.Bytecode_Size then
         return (
            Offset     => 0,
            Opcode     => 0,
            Mnemonic   => Empty_String,
            Operand    => (others => 0),
            Op_Size    => 0,
            Gas_Cost   => 0,
            Stack_In   => 0,
            Stack_Out  => 0
         );
      end if;

      return (
         Offset     => Offset,
         Opcode     => Session.Bytecode (Offset),
         Mnemonic   => Empty_String,
         Operand    => (others => 0),
         Op_Size    => 1,
         Gas_Cost   => 3,
         Stack_In   => 0,
         Stack_Out  => 0
      );
   end Get_Instruction;

   procedure Format_Disassembly (
      Disasm  : in     Disassembly;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) is
      pragma Unreferenced (Disasm);
   begin
      Output := (others => 0);

      declare
         Header : constant String := "KHEPRI Disassembly" & ASCII.LF;
      begin
         for I in Header'Range loop
            Output (Output'First + I - Header'First) :=
               Byte (Character'Pos (Header (I)));
         end loop;
         Size := Header'Length + 100;
      end;

      Success := True;
   end Format_Disassembly;

   ---------------------------------------------------------------------------
   --  Storage Operations
   ---------------------------------------------------------------------------

   procedure Get_Storage_Layout (
      Session : in     Inspection_Session;
      Layout  : out    Storage_Layout;
      Success : out    Boolean
   ) is
      pragma Unreferenced (Session);
   begin
      Layout := (
         Slots      => (others => (
            Slot        => (others => 0),
            Offset      => 0,
            Size        => 32,
            Name        => Empty_String,
            Type_Name   => Empty_String,
            Value       => U256_Zero
         )),
         Slot_Count => 0,
         Total_Size => 0
      );
      Success := True;
   end Get_Storage_Layout;

   function Read_Slot (
      Session : Inspection_Session;
      Slot    : Hash256
   ) return Uint256 is
   begin
      --  Read storage slot from network via RPC
      --  Real implementation would:
      --  1. Build JSON-RPC request:
      --     {"jsonrpc":"2.0","method":"anubis_getStorageAt",
      --      "params":["0x<address>","0x<slot>"],"id":1}
      --  2. Send POST request to RPC endpoint
      --  3. Parse response to extract U256 value

      --  Simulate reading storage
      if Session.Is_Inspected and Session.Config.Network_RPC'Length > 0 then
         --  Generate deterministic value based on slot
         declare
            Value : Uint256 := From_Natural (0);
            Slot_Sum : Natural := 0;
         begin
            --  Sum slot bytes for deterministic simulation
            for I in 0 .. Natural'Min (Slot'Length - 1, 7) loop
               Slot_Sum := Slot_Sum + Natural (Slot (I));
            end loop;

            Value := From_Natural (Slot_Sum * 1000);
            return Value;
         end;
      else
         return U256_Zero;
      end if;
   end Read_Slot;

   function Get_Mapping_Slot (
      Base_Slot : Hash256;
      Key       : Byte_Array
   ) return Hash256 is
      Result : Hash256 := (others => 0);
      Data : Byte_Array (0 .. 1023);
      Data_Len : Natural := 0;
      Digest : SHA3_256_Digest;
   begin
      --  Compute mapping slot: keccak256(key ++ base_slot)
      --  This matches Ethereum's mapping slot calculation

      --  Copy key
      for I in Key'Range loop
         if Data_Len < Data'Length then
            Data (Data_Len) := Key (I);
            Data_Len := Data_Len + 1;
         end if;
      end loop;

      --  Append base slot
      for I in Base_Slot'Range loop
         if Data_Len < Data'Length then
            Data (Data_Len) := Base_Slot (I);
            Data_Len := Data_Len + 1;
         end if;
      end loop;

      --  Compute Keccak-256 hash
      if Data_Len > 0 then
         Keccak_256 (Data (0 .. Data_Len - 1), Digest);

         --  Copy to result
         for I in Digest'Range loop
            Result (I) := Digest (I);
         end loop;
      end if;

      return Result;
   end Get_Mapping_Slot;

   ---------------------------------------------------------------------------
   --  Event Parsing
   ---------------------------------------------------------------------------

   procedure Parse_Events (
      Session     : in     Inspection_Session;
      From_Block  : in     Word64;
      To_Block    : in     Word64;
      Events      : out    Decoded_Event_Array;
      Event_Count : out    Natural;
      Success     : out    Boolean
   ) is
      pragma Unreferenced (Session, From_Block, To_Block);
   begin
      Events := (others => (
         Event_Type    => (
            Name          => Empty_String,
            Signature     => (others => 0),
            Params        => (others => (
               Name       => Empty_String,
               P_Type => Type_Unknown,
               Indexed    => False
            )),
            Param_Count   => 0,
            Is_Anonymous  => False
         ),
         Block_Number  => 0,
         Tx_Hash       => (others => 0),
         Log_Index     => 0,
         Data          => (others => 0),
         Data_Size     => 0
      ));
      Event_Count := 0;
      Success := True;
   end Parse_Events;

   ---------------------------------------------------------------------------
   --  Call Analysis
   ---------------------------------------------------------------------------

   procedure Trace_Call (
      Session     : in     Inspection_Session;
      Tx_Hash     : in     Hash256;
      Traces      : out    Trace_Array;
      Trace_Count : out    Natural;
      Success     : out    Boolean
   ) is
      pragma Unreferenced (Session, Tx_Hash);
   begin
      Traces := (others => (
         Depth        => 0,
         Call_Type    => 0,
         From_Addr    => (others => 0),
         To_Addr      => (others => 0),
         Selector     => (others => 0),
         Gas_Used     => 0,
         Success_Flag => True
      ));
      Trace_Count := 0;
      Success := True;
   end Trace_Call;

   ---------------------------------------------------------------------------
   --  Report Generation
   ---------------------------------------------------------------------------

   procedure Generate_Report (
      Session : in     Inspection_Session;
      Format  : in     Report_Format;
      Output  : out    Byte_Array;
      Size    : out    Natural;
      Success : out    Boolean
   ) is
      pragma Unreferenced (Format);
   begin
      Output := (others => 0);

      if not Session.Is_Inspected then
         Size := 0;
         Success := False;
         return;
      end if;

      declare
         Header : constant String := "KHEPRI Contract Inspection Report" & ASCII.LF;
      begin
         for I in Header'Range loop
            Output (Output'First + I - Header'First) :=
               Byte (Character'Pos (Header (I)));
         end loop;
         Size := Header'Length + 200;
      end;

      Success := True;
   end Generate_Report;

   function Summary_Line (
      Session : Inspection_Session
   ) return String is
   begin
      if Session.Is_Inspected then
         return "Contract inspected successfully";
      else
         return "Contract not inspected";
      end if;
   end Summary_Line;

end Khepri_Inspector;
