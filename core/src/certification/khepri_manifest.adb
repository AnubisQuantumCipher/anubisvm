pragma SPARK_Mode (On);

package body Khepri_Manifest is

   ---------------------------------------------------------------------------
   --  Manifest Operations
   ---------------------------------------------------------------------------

   procedure Parse_Manifest (
      Data     : in  Byte_Array;
      Manifest : out Contract_Manifest;
      Success  : out Boolean
   ) is
   begin
      --  Initialize with defaults
      Manifest := (
         Header       => (
            Magic           => 0,
            Version         => 0,
            Flags           => 0,
            Header_Size     => 0,
            Metadata_Offset => 0,
            ABI_Offset      => 0,
            Cert_Offset     => 0,
            Deps_Offset     => 0,
            Total_Size      => 0,
            Checksum        => (others => 0)
         ),
         Metadata     => Empty_Metadata,
         ABI          => (
            Functions      => (others => (
               Name        => Empty_String,
               Selector    => (others => 0),
               Mutability  => Mutability_Pure,
               Inputs      => (others => (
                  Name       => Empty_String,
                  Param_Type => Type_U256,
                  Indexed    => False
               )),
               Input_Count => 0,
               Outputs     => (others => (
                  Name       => Empty_String,
                  Param_Type => Type_U256,
                  Indexed    => False
               )),
               Output_Count=> 0,
               Gas_Estimate=> 0,
               WCET_Bound  => 0
            )),
            Function_Count => 0,
            Events         => (others => (
               Name        => Empty_String,
               Signature   => (others => 0),
               Params      => (others => (
                  Name       => Empty_String,
                  Param_Type => Type_U256,
                  Indexed    => False
               )),
               Param_Count => 0,
               Anonymous   => False
            )),
            Event_Count    => 0,
            Errors         => (others => (
               Name        => Empty_String,
               Selector    => (others => 0),
               Params      => (others => (
                  Name       => Empty_String,
                  Param_Type => Type_U256,
                  Indexed    => False
               )),
               Param_Count => 0
            )),
            Error_Count    => 0
         ),
         Certification=> (
            Status           => Uncertified_Status,
            WCET_Bounds      => (others => (
               Function_Selector => (others => 0),
               Cycles           => 0,
               Gas_Bound        => 0,
               Proven           => False
            )),
            WCET_Count       => 0,
            Proof_Hash       => (others => 0),
            CT_Analysis_Hash => (others => 0),
            Audit_Hash       => (others => 0)
         ),
         Dependencies => (
            Deps      => (others => (
               Name              => Empty_String,
               Contract_Address  => (others => 0),
               Version_Required  => Empty_String,
               Min_Cert_Level    => Level_None
            )),
            Dep_Count => 0
         )
      );

      --  Check minimum size
      if Data'Length < 64 then
         Success := False;
         return;
      end if;

      --  Check magic number (first 4 bytes)
      declare
         Magic : Word32 := 0;
      begin
         Magic := Word32 (Data (Data'First)) or
                  Word32 (Data (Data'First + 1)) * 256 or
                  Word32 (Data (Data'First + 2)) * 65536 or
                  Word32 (Data (Data'First + 3)) * 16777216;

         if Magic /= Manifest_Magic then
            Success := False;
            return;
         end if;

         Manifest.Header.Magic := Magic;
      end;

      --  Placeholder: Full parsing would continue here
      Success := True;
   end Parse_Manifest;

   procedure Serialize_Manifest (
      Manifest : in  Contract_Manifest;
      Data     : out Byte_Array;
      Size     : out Natural;
      Success  : out Boolean
   ) is
   begin
      Data := (others => 0);
      Size := 0;

      --  Write magic number
      Data (Data'First)     := Byte (Manifest.Header.Magic and 16#FF#);
      Data (Data'First + 1) := Byte ((Manifest.Header.Magic / 256) and 16#FF#);
      Data (Data'First + 2) := Byte ((Manifest.Header.Magic / 65536) and 16#FF#);
      Data (Data'First + 3) := Byte ((Manifest.Header.Magic / 16777216) and 16#FF#);

      --  Placeholder: Full serialization would continue here
      Size := 64;  --  Minimum header size
      Success := True;
   end Serialize_Manifest;

   function Validate_Manifest (
      Manifest : Contract_Manifest
   ) return Boolean is
   begin
      --  Check magic
      if Manifest.Header.Magic /= Manifest_Magic then
         return False;
      end if;

      --  Check version
      if Manifest.Header.Version > Manifest_Version then
         return False;
      end if;

      return True;
   end Validate_Manifest;

   function Calculate_Checksum (
      Manifest : Contract_Manifest
   ) return Hash256 is
      Result : Hash256 := (others => 0);
   begin
      --  Placeholder: Would hash manifest contents
      Result (0) := Byte (Manifest.Header.Magic and 16#FF#);
      return Result;
   end Calculate_Checksum;

   function Get_Function (
      Manifest : Contract_Manifest;
      Selector : Bytes4
   ) return Function_ABI is
   begin
      for I in 0 .. Manifest.ABI.Function_Count - 1 loop
         if Manifest.ABI.Functions (Function_Index (I)).Selector = Selector then
            return Manifest.ABI.Functions (Function_Index (I));
         end if;
      end loop;

      --  Return empty if not found
      return (
         Name        => Empty_String,
         Selector    => (others => 0),
         Mutability  => Mutability_Pure,
         Inputs      => (others => (
            Name       => Empty_String,
            Param_Type => Type_U256,
            Indexed    => False
         )),
         Input_Count => 0,
         Outputs     => (others => (
            Name       => Empty_String,
            Param_Type => Type_U256,
            Indexed    => False
         )),
         Output_Count=> 0,
         Gas_Estimate=> 0,
         WCET_Bound  => 0
      );
   end Get_Function;

   function Get_Event (
      Manifest  : Contract_Manifest;
      Signature : Hash256
   ) return Event_ABI is
   begin
      for I in 0 .. Manifest.ABI.Event_Count - 1 loop
         if Manifest.ABI.Events (Event_Index (I)).Signature = Signature then
            return Manifest.ABI.Events (Event_Index (I));
         end if;
      end loop;

      return (
         Name        => Empty_String,
         Signature   => (others => 0),
         Params      => (others => (
            Name       => Empty_String,
            Param_Type => Type_U256,
            Indexed    => False
         )),
         Param_Count => 0,
         Anonymous   => False
      );
   end Get_Event;

   function Get_WCET_Bound (
      Manifest : Contract_Manifest;
      Selector : Bytes4
   ) return Gas_Amount is
   begin
      for I in 0 .. Manifest.Certification.WCET_Count - 1 loop
         if Manifest.Certification.WCET_Bounds (WCET_Index (I)).Function_Selector = Selector then
            return Manifest.Certification.WCET_Bounds (WCET_Index (I)).Gas_Bound;
         end if;
      end loop;
      return 0;
   end Get_WCET_Bound;

   ---------------------------------------------------------------------------
   --  Manifest Builder
   ---------------------------------------------------------------------------

   function Create_Builder return Manifest_Builder is
   begin
      return (
         Manifest => (
            Header       => (
               Magic           => Manifest_Magic,
               Version         => Manifest_Version,
               Flags           => 0,
               Header_Size     => 64,
               Metadata_Offset => 64,
               ABI_Offset      => 0,
               Cert_Offset     => 0,
               Deps_Offset     => 0,
               Total_Size      => 0,
               Checksum        => (others => 0)
            ),
            Metadata     => Empty_Metadata,
            ABI          => (
               Functions      => (others => (
                  Name        => Empty_String,
                  Selector    => (others => 0),
                  Mutability  => Mutability_Pure,
                  Inputs      => (others => (
                     Name       => Empty_String,
                     Param_Type => Type_U256,
                     Indexed    => False
                  )),
                  Input_Count => 0,
                  Outputs     => (others => (
                     Name       => Empty_String,
                     Param_Type => Type_U256,
                     Indexed    => False
                  )),
                  Output_Count=> 0,
                  Gas_Estimate=> 0,
                  WCET_Bound  => 0
               )),
               Function_Count => 0,
               Events         => (others => (
                  Name        => Empty_String,
                  Signature   => (others => 0),
                  Params      => (others => (
                     Name       => Empty_String,
                     Param_Type => Type_U256,
                     Indexed    => False
                  )),
                  Param_Count => 0,
                  Anonymous   => False
               )),
               Event_Count    => 0,
               Errors         => (others => (
                  Name        => Empty_String,
                  Selector    => (others => 0),
                  Params      => (others => (
                     Name       => Empty_String,
                     Param_Type => Type_U256,
                     Indexed    => False
                  )),
                  Param_Count => 0
               )),
               Error_Count    => 0
            ),
            Certification=> (
               Status           => Uncertified_Status,
               WCET_Bounds      => (others => (
                  Function_Selector => (others => 0),
                  Cycles           => 0,
                  Gas_Bound        => 0,
                  Proven           => False
               )),
               WCET_Count       => 0,
               Proof_Hash       => (others => 0),
               CT_Analysis_Hash => (others => 0),
               Audit_Hash       => (others => 0)
            ),
            Dependencies => (
               Deps      => (others => (
                  Name              => Empty_String,
                  Contract_Address  => (others => 0),
                  Version_Required  => Empty_String,
                  Min_Cert_Level    => Level_None
               )),
               Dep_Count => 0
            )
         ),
         Is_Valid => True
      );
   end Create_Builder;

   procedure Set_Metadata (
      Builder  : in Out Manifest_Builder;
      Metadata : in     Contract_Metadata
   ) is
   begin
      Builder.Manifest.Metadata := Metadata;
   end Set_Metadata;

   procedure Add_Function (
      Builder : in Out Manifest_Builder;
      Func    : in     Function_ABI;
      Success : out    Boolean
   ) is
   begin
      if Builder.Manifest.ABI.Function_Count >= Max_Functions then
         Success := False;
         return;
      end if;

      Builder.Manifest.ABI.Functions (
         Function_Index (Builder.Manifest.ABI.Function_Count)) := Func;
      Builder.Manifest.ABI.Function_Count :=
         Builder.Manifest.ABI.Function_Count + 1;
      Success := True;
   end Add_Function;

   procedure Add_Event (
      Builder : in Out Manifest_Builder;
      Event   : in     Event_ABI;
      Success : out    Boolean
   ) is
   begin
      if Builder.Manifest.ABI.Event_Count >= Max_Events then
         Success := False;
         return;
      end if;

      Builder.Manifest.ABI.Events (
         Event_Index (Builder.Manifest.ABI.Event_Count)) := Event;
      Builder.Manifest.ABI.Event_Count :=
         Builder.Manifest.ABI.Event_Count + 1;
      Success := True;
   end Add_Event;

   procedure Set_Certification (
      Builder : in Out Manifest_Builder;
      Cert    : in     Certification_Data
   ) is
   begin
      Builder.Manifest.Certification := Cert;
   end Set_Certification;

   procedure Add_Dependency (
      Builder : in Out Manifest_Builder;
      Dep     : in     Dependency_Entry;
      Success : out    Boolean
   ) is
   begin
      if Builder.Manifest.Dependencies.Dep_Count >= Max_Dependencies then
         Success := False;
         return;
      end if;

      Builder.Manifest.Dependencies.Deps (
         Dep_Index (Builder.Manifest.Dependencies.Dep_Count)) := Dep;
      Builder.Manifest.Dependencies.Dep_Count :=
         Builder.Manifest.Dependencies.Dep_Count + 1;
      Success := True;
   end Add_Dependency;

   function Build (Builder : Manifest_Builder) return Contract_Manifest is
   begin
      return Builder.Manifest;
   end Build;

end Khepri_Manifest;
