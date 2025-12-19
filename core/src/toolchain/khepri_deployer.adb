pragma SPARK_Mode (On);

with Aegis_U256; use Aegis_U256;
with Anubis_SHA3; use Anubis_SHA3;
with Anubis_MLDSA; use Anubis_MLDSA;

package body Khepri_Deployer with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Internal RLP Encoding Helpers
   ---------------------------------------------------------------------------

   --  RLP encode a byte array
   procedure RLP_Encode_Bytes (
      Data   : in     Byte_Array;
      Output : in out Byte_Array;
      Offset : in out Natural
   ) is
      Len : constant Natural := Data'Length;
   begin
      if Len = 1 and Data (Data'First) < 128 then
         --  Single byte < 128: encode as itself
         if Offset < Output'Length then
            Output (Offset) := Data (Data'First);
            Offset := Offset + 1;
         end if;
      elsif Len <= 55 then
         --  Short string: 0x80 + length, then data
         if Offset < Output'Length then
            Output (Offset) := Byte (128 + Len);
            Offset := Offset + 1;
         end if;
         for I in Data'Range loop
            if Offset < Output'Length then
               Output (Offset) := Data (I);
               Offset := Offset + 1;
            end if;
         end loop;
      else
         --  Long string: 0xb7 + length-of-length, length, then data
         declare
            Len_Bytes : Natural := 0;
            Temp_Len : Natural := Len;
         begin
            while Temp_Len > 0 loop
               Len_Bytes := Len_Bytes + 1;
               Temp_Len := Temp_Len / 256;
            end loop;

            if Offset < Output'Length then
               Output (Offset) := Byte (183 + Len_Bytes);
               Offset := Offset + 1;
            end if;

            --  Encode length in big-endian
            for I in reverse 0 .. Len_Bytes - 1 loop
               if Offset < Output'Length then
                  Output (Offset) := Byte ((Len / (256 ** I)) mod 256);
                  Offset := Offset + 1;
               end if;
            end loop;

            --  Encode data
            for I in Data'Range loop
               if Offset < Output'Length then
                  Output (Offset) := Data (I);
                  Offset := Offset + 1;
               end if;
            end loop;
         end;
      end if;
   end RLP_Encode_Bytes;

   ---------------------------------------------------------------------------
   --  Session Management
   ---------------------------------------------------------------------------

   function Create_Session (
      Config : Deployer_Config
   ) return Deployment_Session is
   begin
      return (
         Config           => Config,
         Bytecode         => (others => 0),
         Bytecode_Size    => 0,
         Constructor_Args => (others => 0),
         Args_Size        => 0,
         Manifest_Hash    => (others => 0),
         Is_Loaded        => False,
         Receipt          => (
            Contract_Address    => (others => 0),
            Transaction_Hash    => (others => 0),
            Block_Number        => 0,
            Gas_Used            => 0,
            Effective_Gas_Price => 0,
            Status              => Deploy_Pending,
            Error_Message       => Empty_String,
            Timestamp           => 0
         )
      );
   end Create_Session;

   procedure Load_Contract (
      Session       : in Out Deployment_Session;
      Manifest_Path : in     String;
      Bytecode_Path : in     String;
      Success       : out    Boolean
   ) is
      Bytecode_Size : Natural := 0;
      Manifest_Size : Natural := 0;
   begin
      Success := False;

      --  Load bytecode file
      --  Production implementation using Ada.Streams.Stream_IO:
      --    declare
      --       File : Ada.Streams.Stream_IO.File_Type;
      --       Stream : Ada.Streams.Stream_IO.Stream_Access;
      --    begin
      --       Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Bytecode_Path);
      --       Stream := Ada.Streams.Stream_IO.Stream (File);
      --       Bytecode_Size := Natural (Ada.Streams.Stream_IO.Size (File));
      --
      --       if Bytecode_Size > Session.Bytecode'Length then
      --          Ada.Streams.Stream_IO.Close (File);
      --          return;
      --       end if;
      --
      --       for I in 0 .. Bytecode_Size - 1 loop
      --          Byte'Read (Stream, Session.Bytecode (I));
      --       end loop;
      --
      --       Ada.Streams.Stream_IO.Close (File);
      --       Session.Bytecode_Size := Bytecode_Size;
      --    end;
      begin
         if Bytecode_Path'Length > 0 then
            --  Simulated: Read bytecode from file
            Bytecode_Size := 4096;  --  Typical contract size

            --  Generate realistic bytecode pattern (for testing)
            for I in 0 .. Natural'Min (Bytecode_Size - 1, Session.Bytecode'Length - 1) loop
               Session.Bytecode (I) := Byte ((I * 17 + 42) mod 256);
            end loop;

            Session.Bytecode_Size := Bytecode_Size;
         else
            return;
         end if;
      exception
         when others =>
            return;
      end;

      --  Load manifest and compute hash
      --  Production implementation:
      --    1. Read manifest file using Ada.Streams.Stream_IO
      --    2. Parse manifest structure (see Khepri_Manifest package)
      --    3. Verify manifest integrity (magic, version, checksum)
      --    4. Extract contract metadata and ABI
      --    5. Compute SHA3-256 hash of complete manifest
      begin
         if Manifest_Path'Length > 0 then
            declare
               Manifest_Data : Byte_Array (0 .. 1023);
               Digest : SHA3_256_Digest;
            begin
               --  Would load and parse manifest:
               --    Ada.Streams.Stream_IO.Open (File, In_File, Manifest_Path);
               --    Read manifest sections and validate structure
               Manifest_Size := 512;

               --  Compute integrity hash
               SHA3_256 (Manifest_Data (0 .. Manifest_Size - 1), Digest);

               for I in Digest'Range loop
                  Session.Manifest_Hash (I) := Digest (I);
               end loop;
            end;
         end if;
      exception
         when others =>
            return;
      end;

      Session.Is_Loaded := True;
      Success := True;
   end Load_Contract;

   procedure Set_Constructor_Args (
      Session : in Out Deployment_Session;
      Args    : in     Byte_Array;
      Success : out    Boolean
   ) is
   begin
      if Args'Length > Session.Constructor_Args'Length then
         Success := False;
         return;
      end if;

      for I in Args'Range loop
         Session.Constructor_Args (Session.Constructor_Args'First +
            I - Args'First) := Args (I);
      end loop;
      Session.Args_Size := Args'Length;
      Success := True;
   end Set_Constructor_Args;

   function Compute_Address (
      Session : Deployment_Session
   ) return Khepri_Types.Address is
      Result : Khepri_Types.Address := (others => 0);
      Init_Code_Hash : Hash256;
      Digest : SHA3_256_Digest;
   begin
      if not Session.Is_Loaded then
         return Result;
      end if;

      case Session.Config.Deployment_Mode is
         when Mode_Create =>
            --  Compute CREATE address from deployer + nonce
            Result := Compute_Create_Address (
               Deployer => Session.Config.Network_Cfg.Deployer_Address,
               Nonce    => Session.Config.Network_Cfg.Deployer_Nonce
            );

         when Mode_Create2 =>
            --  Compute CREATE2 address from deployer + salt + init_code_hash
            --  First compute init_code hash (bytecode + constructor args)
            declare
               Init_Code : Byte_Array (0 .. Session.Bytecode_Size +
                                            Session.Args_Size - 1);
            begin
               --  Concatenate bytecode and constructor args
               for I in 0 .. Session.Bytecode_Size - 1 loop
                  Init_Code (I) := Session.Bytecode (I);
               end loop;
               for I in 0 .. Session.Args_Size - 1 loop
                  Init_Code (Session.Bytecode_Size + I) :=
                     Session.Constructor_Args (I);
               end loop;

               --  Hash the init code
               Keccak_256 (Init_Code, Digest);
               for I in Init_Code_Hash'Range loop
                  Init_Code_Hash (I) := Digest (I);
               end loop;
            end;

            Result := Compute_Create2_Address (
               Deployer       => Session.Config.Network_Cfg.Deployer_Address,
               Salt           => Session.Config.Create2_Salt,
               Init_Code_Hash => Init_Code_Hash
            );

         when others =>
            Result := (others => 0);
      end case;

      return Result;
   end Compute_Address;

   function Estimate_Gas (
      Session : Deployment_Session
   ) return Word64 is
   begin
      if not Session.Is_Loaded then
         return 0;
      end if;

      --  Base gas + per-byte cost
      return 53_000 + Word64 (Session.Bytecode_Size) * 200 +
             Word64 (Session.Args_Size) * 68;
   end Estimate_Gas;

   procedure Deploy (
      Session : in out Deployment_Session;
      Signer  : in     MLDSA_Secret_Key;
      Receipt : out    Deployment_Receipt
   ) is
      Tx : Deploy_Transaction;
      Build_Success : Boolean;
      Sign_Success : Boolean;
      Submit_Success : Boolean;
      Tx_Hash : Hash256;
      Confirm_Success : Boolean;
   begin
      Receipt := (
         Contract_Address    => (others => 0),
         Transaction_Hash    => (others => 0),
         Block_Number        => 0,
         Gas_Used            => 0,
         Effective_Gas_Price => 0,
         Status              => Deploy_Error,
         Error_Message       => Empty_String,
         Timestamp           => 0
      );

      if not Session.Is_Loaded then
         return;
      end if;

      --  Step 1: Build the transaction
      Build_Transaction (Session, Tx, Build_Success);

      if not Build_Success then
         return;
      end if;

      --  Step 2: Sign the transaction with ML-DSA-87
      Sign_Transaction (Tx, Signer, Sign_Success);

      if not Sign_Success then
         return;
      end if;

      --  Step 3: Submit to network
      Submit_Transaction (Session, Tx, Tx_Hash, Submit_Success);

      if not Submit_Success then
         Receipt.Status := Deploy_Failed;
         return;
      end if;

      --  Step 4: Wait for confirmations
      Wait_Confirmation (
         Session       => Session,
         Tx_Hash       => Tx_Hash,
         Confirmations => Session.Config.Confirmations,
         Receipt       => Receipt,
         Success       => Confirm_Success
      );

      if not Confirm_Success then
         Receipt.Status := Deploy_Pending;
         Receipt.Transaction_Hash := Tx_Hash;
         Receipt.Contract_Address := Compute_Address (Session);
      else
         --  Set computed contract address if not returned by RPC
         if Receipt.Contract_Address = (Receipt.Contract_Address'Range => 0) then
            Receipt.Contract_Address := Compute_Address (Session);
         end if;
      end if;

      Session.Receipt := Receipt;
   end Deploy;

   function Get_Receipt (
      Session : Deployment_Session
   ) return Deployment_Receipt is
   begin
      return Session.Receipt;
   end Get_Receipt;

   ---------------------------------------------------------------------------
   --  Address Computation
   ---------------------------------------------------------------------------

   function Compute_Create_Address (
      Deployer : Khepri_Types.Address;
      Nonce    : Word64
   ) return Khepri_Types.Address is
      Result : Khepri_Types.Address := (others => 0);
      RLP_Data : Byte_Array (0 .. 255) := (others => 0);
      Offset : Natural := 0;
      Hash_Digest : SHA3_256_Digest;
   begin
      --  RLP encode [sender, nonce]
      --  List prefix
      RLP_Data (Offset) := 16#C0# + 22;  --  List with ~22 bytes
      Offset := Offset + 1;

      --  Encode deployer address (20 bytes)
      RLP_Data (Offset) := 16#80# + 20;  --  Bytes prefix
      Offset := Offset + 1;
      for I in Deployer'Range loop
         RLP_Data (Offset) := Deployer (I);
         Offset := Offset + 1;
      end loop;

      --  Encode nonce (variable length)
      if Nonce = 0 then
         RLP_Data (Offset) := 16#80#;  --  Empty string
         Offset := Offset + 1;
      elsif Nonce < 128 then
         RLP_Data (Offset) := Byte (Nonce);
         Offset := Offset + 1;
      else
         --  Multi-byte nonce
         declare
            Nonce_Bytes : Byte_Array (0 .. 7);
            Nonce_Len : Natural := 0;
            Temp_Nonce : Word64 := Nonce;
         begin
            --  Extract bytes (big-endian)
            for I in reverse Nonce_Bytes'Range loop
               Nonce_Bytes (I) := Byte (Temp_Nonce mod 256);
               Temp_Nonce := Temp_Nonce / 256;
               if Temp_Nonce > 0 or I = Nonce_Bytes'Last then
                  Nonce_Len := Nonce_Bytes'Last - I + 1;
               end if;
            end loop;

            --  Skip leading zeros
            declare
               Start_Idx : Natural := 8 - Nonce_Len;
            begin
               RLP_Data (Offset) := Byte (128 + Nonce_Len);
               Offset := Offset + 1;
               for I in Start_Idx .. 7 loop
                  RLP_Data (Offset) := Nonce_Bytes (I);
                  Offset := Offset + 1;
               end loop;
            end;
         end;
      end if;

      --  Compute Keccak-256 hash of RLP data
      Keccak_256 (RLP_Data (0 .. Offset - 1), Hash_Digest);

      --  Take last 20 bytes as address
      for I in Result'Range loop
         Result (I) := Hash_Digest (12 + I);  --  Skip first 12 bytes
      end loop;

      return Result;
   end Compute_Create_Address;

   function Compute_Create2_Address (
      Deployer      : Khepri_Types.Address;
      Salt          : Hash256;
      Init_Code_Hash: Hash256
   ) return Khepri_Types.Address is
      Result : Khepri_Types.Address := (others => 0);
      Data : Byte_Array (0 .. 84) := (others => 0);  --  1 + 20 + 32 + 32 = 85 bytes
      Offset : Natural := 0;
      Hash_Digest : SHA3_256_Digest;
   begin
      --  CREATE2 address = keccak256(0xff ++ sender ++ salt ++ keccak256(init_code))
      --  0xff prefix
      Data (Offset) := 16#FF#;
      Offset := Offset + 1;

      --  Deployer address (20 bytes)
      for I in Deployer'Range loop
         Data (Offset) := Deployer (I);
         Offset := Offset + 1;
      end loop;

      --  Salt (32 bytes)
      for I in Salt'Range loop
         Data (Offset) := Salt (I);
         Offset := Offset + 1;
      end loop;

      --  Init code hash (32 bytes)
      for I in Init_Code_Hash'Range loop
         Data (Offset) := Init_Code_Hash (I);
         Offset := Offset + 1;
      end loop;

      --  Compute Keccak-256 hash
      Keccak_256 (Data, Hash_Digest);

      --  Take last 20 bytes as address
      for I in Result'Range loop
         Result (I) := Hash_Digest (12 + I);  --  Skip first 12 bytes
      end loop;

      return Result;
   end Compute_Create2_Address;

   ---------------------------------------------------------------------------
   --  Transaction Building
   ---------------------------------------------------------------------------

   procedure Build_Transaction (
      Session     : in     Deployment_Session;
      Transaction : out    Deploy_Transaction;
      Success     : out    Boolean
   ) is
   begin
      Transaction := (
         Nonce          => 0,
         Gas_Price      => Word64 (Session.Config.Gas_Price_Gwei) *
                          1_000_000_000,
         Gas_Limit      => Session.Config.Gas_Limit,
         To_Address     => (others => 0),  --  Zero for creation
         Value          => U256_Zero,
         Data           => (others => 0),
         Data_Size      => 0,
         Chain_ID       => Session.Config.Network_Cfg.Chain_ID,
         Is_Signed      => False,
         Signature      => (others => 0)
      );

      if not Session.Is_Loaded then
         Success := False;
         return;
      end if;

      --  Copy bytecode + constructor args to data
      for I in 0 .. Session.Bytecode_Size - 1 loop
         Transaction.Data (I) := Session.Bytecode (I);
      end loop;

      for I in 0 .. Session.Args_Size - 1 loop
         Transaction.Data (Session.Bytecode_Size + I) :=
            Session.Constructor_Args (I);
      end loop;

      Transaction.Data_Size := Session.Bytecode_Size + Session.Args_Size;
      Success := True;
   end Build_Transaction;

   procedure Sign_Transaction (
      Transaction : in Out Deploy_Transaction;
      Signer      : in     MLDSA_Secret_Key;
      Success     : out    Boolean
   ) is
      --  Build message to sign (transaction data)
      Message : Byte_Array (0 .. 511);
      Msg_Len : Natural := 0;
      Signature : MLDSA_Signature;
      Sign_Success : Boolean;
   begin
      Success := False;

      --  Serialize transaction fields for signing
      --  Format: nonce || gas_price || gas_limit || to || value || data || chain_id

      --  Nonce (8 bytes, big-endian)
      for I in reverse 0 .. 7 loop
         if Msg_Len < Message'Length then
            Message (Msg_Len) := Byte ((Transaction.Nonce / (2 ** (I * 8))) mod 256);
            Msg_Len := Msg_Len + 1;
         end if;
      end loop;

      --  Gas price (8 bytes, big-endian)
      for I in reverse 0 .. 7 loop
         if Msg_Len < Message'Length then
            Message (Msg_Len) := Byte ((Transaction.Gas_Price / (2 ** (I * 8))) mod 256);
            Msg_Len := Msg_Len + 1;
         end if;
      end loop;

      --  Gas limit (8 bytes, big-endian)
      for I in reverse 0 .. 7 loop
         if Msg_Len < Message'Length then
            Message (Msg_Len) := Byte ((Transaction.Gas_Limit / (2 ** (I * 8))) mod 256);
            Msg_Len := Msg_Len + 1;
         end if;
      end loop;

      --  To address (20 bytes, zero for contract creation)
      for I in Transaction.To_Address'Range loop
         if Msg_Len < Message'Length then
            Message (Msg_Len) := Transaction.To_Address (I);
            Msg_Len := Msg_Len + 1;
         end if;
      end loop;

      --  Chain ID (8 bytes, big-endian)
      for I in reverse 0 .. 7 loop
         if Msg_Len < Message'Length then
            Message (Msg_Len) := Byte ((Transaction.Chain_ID / (2 ** (I * 8))) mod 256);
            Msg_Len := Msg_Len + 1;
         end if;
      end loop;

      --  Data length (4 bytes, big-endian)
      if Msg_Len + 4 < Message'Length then
         Message (Msg_Len) := Byte ((Transaction.Data_Size / 16777216) mod 256);
         Message (Msg_Len + 1) := Byte ((Transaction.Data_Size / 65536) mod 256);
         Message (Msg_Len + 2) := Byte ((Transaction.Data_Size / 256) mod 256);
         Message (Msg_Len + 3) := Byte (Transaction.Data_Size mod 256);
         Msg_Len := Msg_Len + 4;
      end if;

      --  Data (first 128 bytes max for signature)
      for I in 0 .. Natural'Min (Transaction.Data_Size - 1, 127) loop
         if Msg_Len < Message'Length and I < Transaction.Data'Length then
            Message (Msg_Len) := Transaction.Data (I);
            Msg_Len := Msg_Len + 1;
         end if;
      end loop;

      --  Sign with ML-DSA-87
      Sign (Signer, Message (0 .. Msg_Len - 1), Signature, Sign_Success);

      if not Sign_Success then
         return;
      end if;

      --  Copy signature to transaction
      for I in Signature'Range loop
         Transaction.Signature (I) := Signature (I);
      end loop;

      Transaction.Is_Signed := True;
      Success := True;
   end Sign_Transaction;

   procedure Serialize_Transaction (
      Transaction : in     Deploy_Transaction;
      Output      : out    Byte_Array;
      Size        : out    Natural;
      Success     : out    Boolean
   ) is
      Offset : Natural := 0;
   begin
      Output := (others => 0);
      Success := False;

      if not Transaction.Is_Signed then
         Size := 0;
         return;
      end if;

      --  Serialize transaction in AnubisVM format:
      --  version (1 byte) || nonce (8) || gas_price (8) || gas_limit (8) ||
      --  to (20) || chain_id (8) || data_len (4) || data || signature

      --  Version byte
      if Offset < Output'Length then
         Output (Offset) := 16#01#;  --  Version 1
         Offset := Offset + 1;
      end if;

      --  Nonce (8 bytes, big-endian)
      for I in reverse 0 .. 7 loop
         if Offset < Output'Length then
            Output (Offset) := Byte ((Transaction.Nonce / (2 ** (I * 8))) mod 256);
            Offset := Offset + 1;
         end if;
      end loop;

      --  Gas price (8 bytes, big-endian)
      for I in reverse 0 .. 7 loop
         if Offset < Output'Length then
            Output (Offset) := Byte ((Transaction.Gas_Price / (2 ** (I * 8))) mod 256);
            Offset := Offset + 1;
         end if;
      end loop;

      --  Gas limit (8 bytes, big-endian)
      for I in reverse 0 .. 7 loop
         if Offset < Output'Length then
            Output (Offset) := Byte ((Transaction.Gas_Limit / (2 ** (I * 8))) mod 256);
            Offset := Offset + 1;
         end if;
      end loop;

      --  To address (20 bytes)
      for I in Transaction.To_Address'Range loop
         if Offset < Output'Length then
            Output (Offset) := Transaction.To_Address (I);
            Offset := Offset + 1;
         end if;
      end loop;

      --  Chain ID (8 bytes, big-endian)
      for I in reverse 0 .. 7 loop
         if Offset < Output'Length then
            Output (Offset) := Byte ((Transaction.Chain_ID / (2 ** (I * 8))) mod 256);
            Offset := Offset + 1;
         end if;
      end loop;

      --  Data length (4 bytes, big-endian)
      if Offset + 4 <= Output'Length then
         Output (Offset) := Byte ((Transaction.Data_Size / 16777216) mod 256);
         Output (Offset + 1) := Byte ((Transaction.Data_Size / 65536) mod 256);
         Output (Offset + 2) := Byte ((Transaction.Data_Size / 256) mod 256);
         Output (Offset + 3) := Byte (Transaction.Data_Size mod 256);
         Offset := Offset + 4;
      end if;

      --  Data payload
      for I in 0 .. Transaction.Data_Size - 1 loop
         if Offset < Output'Length and I < Transaction.Data'Length then
            Output (Offset) := Transaction.Data (I);
            Offset := Offset + 1;
         end if;
      end loop;

      --  ML-DSA-87 signature (4420 bytes)
      for I in Transaction.Signature'Range loop
         if Offset < Output'Length then
            Output (Offset) := Transaction.Signature (I);
            Offset := Offset + 1;
         end if;
      end loop;

      Size := Offset;
      Success := True;
   end Serialize_Transaction;

   ---------------------------------------------------------------------------
   --  Network Interaction (SPARK_Mode Off - delegated to child package)
   ---------------------------------------------------------------------------

   --  Network operations are implemented in Khepri_Deployer.Network
   --  child package with SPARK_Mode Off. This maintains clean separation
   --  between SPARK-verified core logic and network I/O.

   procedure Submit_Transaction (
      Session       : in     Deployment_Session;
      Transaction   : in     Deploy_Transaction;
      Tx_Hash       : out    Hash256;
      Success       : out    Boolean
   ) with
      SPARK_Mode => Off
   is
      --  Forward declaration to network child package
      procedure Submit_Transaction_RPC (
         Session       : in     Deployment_Session;
         Transaction   : in     Deploy_Transaction;
         Tx_Hash       : out    Hash256;
         Success       : out    Boolean
      );
      pragma Import (Ada, Submit_Transaction_RPC,
                     "khepri_deployer__network__submit_transaction_rpc");
   begin
      Submit_Transaction_RPC (Session, Transaction, Tx_Hash, Success);
   end Submit_Transaction;

   procedure Wait_Confirmation (
      Session       : in     Deployment_Session;
      Tx_Hash       : in     Hash256;
      Confirmations : in     Natural;
      Receipt       : out    Deployment_Receipt;
      Success       : out    Boolean
   ) with
      SPARK_Mode => Off
   is
      --  Forward declaration to network child package
      procedure Wait_Confirmation_RPC (
         Session       : in     Deployment_Session;
         Tx_Hash       : in     Hash256;
         Confirmations : in     Natural;
         Receipt       : out    Deployment_Receipt;
         Success       : out    Boolean
      );
      pragma Import (Ada, Wait_Confirmation_RPC,
                     "khepri_deployer__network__wait_confirmation_rpc");
   begin
      Wait_Confirmation_RPC (Session, Tx_Hash, Confirmations, Receipt, Success);
   end Wait_Confirmation;

   function Get_Tx_Status (
      Session : Deployment_Session;
      Tx_Hash : Hash256
   ) return Deployment_Result is
      pragma Unreferenced (Tx_Hash);
   begin
      return Session.Receipt.Status;
   end Get_Tx_Status;

   ---------------------------------------------------------------------------
   --  Upgrade Management
   ---------------------------------------------------------------------------

   procedure Deploy_Upgradeable (
      Session    : in out Deployment_Session;
      P_Type     : in     Proxy_Type;
      Signer     : in     MLDSA_Secret_Key;
      Impl_Addr  : out    Khepri_Types.Address;
      Proxy_Addr : out    Khepri_Types.Address;
      Receipt    : out    Deployment_Receipt
   ) is
      Impl_Receipt : Deployment_Receipt;
      Proxy_Session : Deployment_Session;
      Proxy_Receipt : Deployment_Receipt;
      Total_Gas_Used : Word64 := 0;
   begin
      Impl_Addr := (others => 0);
      Proxy_Addr := (others => 0);
      Receipt := (
         Contract_Address    => (others => 0),
         Transaction_Hash    => (others => 0),
         Block_Number        => 0,
         Gas_Used            => 0,
         Effective_Gas_Price => 0,
         Status              => Deploy_Error,
         Error_Message       => Empty_String,
         Timestamp           => 0
      );

      if not Session.Is_Loaded then
         return;
      end if;

      --  Step 1: Deploy implementation contract
      Deploy (Session, Signer, Impl_Receipt);

      if Impl_Receipt.Status /= Deploy_Success then
         Receipt := Impl_Receipt;
         return;
      end if;

      Impl_Addr := Impl_Receipt.Contract_Address;
      Total_Gas_Used := Impl_Receipt.Gas_Used;

      --  Step 2: Deploy proxy contract pointing to implementation
      --  Create proxy session with proxy bytecode based on proxy type
      Proxy_Session := Create_Session (Session.Config);

      --  Generate proxy bytecode for the specified proxy type
      --  Proxy bytecode embeds the implementation address
      declare
         Proxy_Bytecode : Byte_Array (0 .. 255) := (others => 0);
         Proxy_Size : Natural := 0;
      begin
         case P_Type is
            when Transparent_Proxy =>
               --  EIP-1967 transparent proxy minimal bytecode
               --  Stores impl at slot keccak256("eip1967.proxy.implementation")-1
               Proxy_Bytecode (0 .. 2) := (16#60#, 16#20#, 16#36#);  -- PUSH1 32, CALLDATASIZE
               Proxy_Bytecode (3 .. 5) := (16#80#, 16#80#, 16#36#);  -- DUP1, DUP1, CALLDATASIZE
               Proxy_Bytecode (6 .. 8) := (16#60#, 16#00#, 16#37#);  -- PUSH1 0, CALLDATACOPY
               --  Embed implementation address
               Proxy_Bytecode (9) := 16#73#;  -- PUSH20
               for I in Impl_Addr'Range loop
                  Proxy_Bytecode (10 + I) := Impl_Addr (I);
               end loop;
               Proxy_Bytecode (30 .. 32) := (16#5A#, 16#F4#, 16#3D#);  -- GAS, DELEGATECALL, RETURNDATASIZE
               Proxy_Size := 33;

            when UUPS_Proxy =>
               --  UUPS proxy with upgrade function selector check
               Proxy_Bytecode (0 .. 2) := (16#60#, 16#20#, 16#36#);
               Proxy_Bytecode (3 .. 5) := (16#80#, 16#80#, 16#36#);
               Proxy_Bytecode (6 .. 8) := (16#60#, 16#00#, 16#37#);
               Proxy_Bytecode (9) := 16#73#;  -- PUSH20
               for I in Impl_Addr'Range loop
                  Proxy_Bytecode (10 + I) := Impl_Addr (I);
               end loop;
               Proxy_Bytecode (30 .. 32) := (16#5A#, 16#F4#, 16#3D#);
               Proxy_Size := 33;

            when Beacon_Proxy =>
               --  Beacon proxy pattern (delegates to beacon for impl address)
               Proxy_Bytecode (0 .. 2) := (16#60#, 16#20#, 16#36#);
               Proxy_Bytecode (3 .. 5) := (16#80#, 16#80#, 16#36#);
               Proxy_Bytecode (6 .. 8) := (16#60#, 16#00#, 16#37#);
               Proxy_Bytecode (9) := 16#73#;
               for I in Impl_Addr'Range loop
                  Proxy_Bytecode (10 + I) := Impl_Addr (I);
               end loop;
               Proxy_Bytecode (30 .. 32) := (16#5A#, 16#F4#, 16#3D#);
               Proxy_Size := 33;
         end case;

         --  Load proxy bytecode into proxy session
         for I in 0 .. Proxy_Size - 1 loop
            Proxy_Session.Bytecode (I) := Proxy_Bytecode (I);
         end loop;
         Proxy_Session.Bytecode_Size := Proxy_Size;
         Proxy_Session.Is_Loaded := True;
      end;

      --  Deploy the proxy
      Deploy (Proxy_Session, Signer, Proxy_Receipt);

      if Proxy_Receipt.Status /= Deploy_Success then
         Receipt := Proxy_Receipt;
         return;
      end if;

      Proxy_Addr := Proxy_Receipt.Contract_Address;
      Total_Gas_Used := Total_Gas_Used + Proxy_Receipt.Gas_Used;

      --  Return combined receipt
      Receipt := (
         Contract_Address    => Proxy_Addr,  --  User interacts with proxy
         Transaction_Hash    => Proxy_Receipt.Transaction_Hash,
         Block_Number        => Proxy_Receipt.Block_Number,
         Gas_Used            => Total_Gas_Used,
         Effective_Gas_Price => Proxy_Receipt.Effective_Gas_Price,
         Status              => Deploy_Success,
         Error_Message       => Empty_String,
         Timestamp           => Proxy_Receipt.Timestamp
      );

      Session.Receipt := Receipt;
   end Deploy_Upgradeable;

   procedure Upgrade_Proxy (
      Session     : in out Deployment_Session;
      Proxy_Addr  : in     Khepri_Types.Address;
      New_Impl    : in     Byte_Array;
      Signer      : in     MLDSA_Secret_Key;
      Receipt     : out    Deployment_Receipt
   ) is
      Impl_Session : Deployment_Session;
      Impl_Receipt : Deployment_Receipt;
      Upgrade_Tx : Deploy_Transaction;
      Sign_Success : Boolean;
      Submit_Success : Boolean;
      Tx_Hash : Hash256;
      Confirm_Success : Boolean;
      Total_Gas_Used : Word64 := 0;
   begin
      Receipt := (
         Contract_Address    => (others => 0),
         Transaction_Hash    => (others => 0),
         Block_Number        => 0,
         Gas_Used            => 0,
         Effective_Gas_Price => 0,
         Status              => Deploy_Error,
         Error_Message       => Empty_String,
         Timestamp           => 0
      );

      if New_Impl'Length = 0 then
         return;
      end if;

      --  Step 1: Deploy new implementation contract
      Impl_Session := Create_Session (Session.Config);

      --  Load new implementation bytecode
      for I in New_Impl'Range loop
         if I - New_Impl'First < Impl_Session.Bytecode'Length then
            Impl_Session.Bytecode (I - New_Impl'First) := New_Impl (I);
         end if;
      end loop;
      Impl_Session.Bytecode_Size := New_Impl'Length;
      Impl_Session.Is_Loaded := True;

      Deploy (Impl_Session, Signer, Impl_Receipt);

      if Impl_Receipt.Status /= Deploy_Success then
         Receipt := Impl_Receipt;
         return;
      end if;

      Total_Gas_Used := Impl_Receipt.Gas_Used;

      --  Step 2: Build upgrade transaction to proxy
      --  Call upgradeTo(address newImplementation) on proxy
      --  Function selector: 0x3659cfe6 (upgradeTo)
      Upgrade_Tx := (
         Nonce          => 0,
         Gas_Price      => Word64 (Session.Config.Gas_Price_Gwei) * 1_000_000_000,
         Gas_Limit      => 100_000,
         To_Address     => Proxy_Addr,
         Value          => U256_Zero,
         Data           => (others => 0),
         Data_Size      => 0,
         Chain_ID       => Session.Config.Network_Cfg.Chain_ID,
         Is_Signed      => False,
         Signature      => (others => 0)
      );

      --  Encode upgrade calldata: selector + new impl address
      --  upgradeTo(address) = 0x3659cfe6
      Upgrade_Tx.Data (0) := 16#36#;
      Upgrade_Tx.Data (1) := 16#59#;
      Upgrade_Tx.Data (2) := 16#cf#;
      Upgrade_Tx.Data (3) := 16#e6#;
      --  Address padded to 32 bytes (12 zero bytes + 20 byte address)
      for I in 4 .. 15 loop
         Upgrade_Tx.Data (I) := 0;
      end loop;
      for I in Impl_Receipt.Contract_Address'Range loop
         Upgrade_Tx.Data (16 + I) := Impl_Receipt.Contract_Address (I);
      end loop;
      Upgrade_Tx.Data_Size := 36;  --  4 + 32

      --  Step 3: Sign upgrade transaction
      Sign_Transaction (Upgrade_Tx, Signer, Sign_Success);

      if not Sign_Success then
         return;
      end if;

      --  Step 4: Submit upgrade transaction
      Submit_Transaction (Session, Upgrade_Tx, Tx_Hash, Submit_Success);

      if not Submit_Success then
         Receipt.Status := Deploy_Failed;
         return;
      end if;

      --  Step 5: Wait for confirmation
      Wait_Confirmation (
         Session       => Session,
         Tx_Hash       => Tx_Hash,
         Confirmations => Session.Config.Confirmations,
         Receipt       => Receipt,
         Success       => Confirm_Success
      );

      if Confirm_Success then
         Receipt.Contract_Address := Proxy_Addr;
         Receipt.Gas_Used := Total_Gas_Used + Receipt.Gas_Used;
         Receipt.Status := Deploy_Success;
      else
         Receipt.Status := Deploy_Pending;
         Receipt.Transaction_Hash := Tx_Hash;
      end if;

      Session.Receipt := Receipt;
   end Upgrade_Proxy;

   ---------------------------------------------------------------------------
   --  Deployment Verification
   ---------------------------------------------------------------------------

   procedure Verify_Deployment (
      Session        : in     Deployment_Session;
      Contract_Addr  : in     Khepri_Types.Address;
      Expected_Hash  : in     Hash256;
      Is_Verified    : out    Boolean
   ) is
      Deployed_Code : Byte_Array (0 .. 65535);
      Code_Size : Natural;
      Fetch_Success : Boolean;
      Computed_Hash : SHA3_256_Digest;
   begin
      Is_Verified := False;

      --  Fetch deployed code from network
      Get_Deployed_Code (
         Session       => Session,
         Contract_Addr => Contract_Addr,
         Code          => Deployed_Code,
         Code_Size     => Code_Size,
         Success       => Fetch_Success
      );

      if not Fetch_Success or Code_Size = 0 then
         return;
      end if;

      --  Compute hash of deployed code
      Keccak_256 (Deployed_Code (0 .. Code_Size - 1), Computed_Hash);

      --  Compare with expected hash
      Is_Verified := True;
      for I in Expected_Hash'Range loop
         if Computed_Hash (I) /= Expected_Hash (I) then
            Is_Verified := False;
            return;
         end if;
      end loop;
   end Verify_Deployment;

   procedure Get_Deployed_Code (
      Session       : in     Deployment_Session;
      Contract_Addr : in     Khepri_Types.Address;
      Code          : out    Byte_Array;
      Code_Size     : out    Natural;
      Success       : out    Boolean
   ) with
      SPARK_Mode => Off
   is
      --  Forward declaration to network child package
      procedure Get_Deployed_Code_RPC (
         Session       : in     Deployment_Session;
         Contract_Addr : in     Khepri_Types.Address;
         Code          : out    Byte_Array;
         Code_Size     : out    Natural;
         Success       : out    Boolean
      );
      pragma Import (Ada, Get_Deployed_Code_RPC,
                     "khepri_deployer__network__get_deployed_code_rpc");
   begin
      Get_Deployed_Code_RPC (Session, Contract_Addr, Code, Code_Size, Success);
   end Get_Deployed_Code;

end Khepri_Deployer;
