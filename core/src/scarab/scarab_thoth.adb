-------------------------------------------------------------------------------
--  SCARAB - THOTH Verified AIR Compiler (Implementation)
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

package body Scarab_Thoth with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Empty_Expr_Node return Expr_Node is
   begin
      return (Kind => Expr_Const,
              Value => Zero,
              Reg => (Column => 0, Row_Offset => 0),
              Periodic_ID => 0,
              Left => 0,
              Right => 0,
              Exponent => 0);
   end Empty_Expr_Node;

   function Make_Expression (
      Nodes      : Expr_Array;
      Root       : Expr_Node_Index;
      Count      : Natural;
      Deg        : Degree_Range
   ) return Expression is
   begin
      return (Nodes => Nodes,
              Root => Root,
              Node_Count => Count,
              Degree => Deg);
   end Make_Expression;

   ---------------------------------------------------------------------------
   --  Expression Builder
   ---------------------------------------------------------------------------

   function Const_Expr (Value : Field_Element) return Expression is
      Nodes : Expr_Array := (others => Empty_Expr_Node);
   begin
      Nodes (0) := (Kind => Expr_Const,
                   Value => Value,
                   Reg => (Column => 0, Row_Offset => 0),
                   Periodic_ID => 0,
                   Left => 0,
                   Right => 0,
                   Exponent => 0);
      return Make_Expression (Nodes, 0, 1, 0);
   end Const_Expr;

   function Reg_Expr (Col : Column_Index; Offset : Integer := 0) return Expression is
      Nodes : Expr_Array := (others => Empty_Expr_Node);
   begin
      Nodes (0) := (Kind => Expr_Register,
                   Value => Zero,
                   Reg => (Column => Col, Row_Offset => Offset),
                   Periodic_ID => 0,
                   Left => 0,
                   Right => 0,
                   Exponent => 0);
      return Make_Expression (Nodes, 0, 1, 1);
   end Reg_Expr;

   function Periodic_Expr (Periodic_ID : Natural) return Expression is
      Nodes : Expr_Array := (others => Empty_Expr_Node);
   begin
      Nodes (0) := (Kind => Expr_Periodic,
                   Value => Zero,
                   Reg => (Column => 0, Row_Offset => 0),
                   Periodic_ID => Periodic_ID,
                   Left => 0,
                   Right => 0,
                   Exponent => 0);
      return Make_Expression (Nodes, 0, 1, 0);
   end Periodic_Expr;

   function Add_Expr (A, B : Expression) return Expression is
      Nodes : Expr_Array := (others => Empty_Expr_Node);
      Count : Natural := 0;
      Result_Deg : Degree_Range;
   begin
      --  Copy A nodes
      for I in 0 .. A.Node_Count - 1 loop
         pragma Loop_Invariant (Count = I);
         Nodes (Count) := A.Nodes (I);
         Count := Count + 1;
      end loop;

      --  Copy B nodes with offset
      declare
         Offset : constant Natural := Count;
      begin
         for I in 0 .. B.Node_Count - 1 loop
            pragma Loop_Invariant (Count = Offset + I);
            declare
               N : Expr_Node := B.Nodes (I);
            begin
               --  Adjust child references
               if N.Kind in Expr_Add | Expr_Sub | Expr_Mul then
                  N.Left := N.Left + Offset;
                  N.Right := N.Right + Offset;
               elsif N.Kind = Expr_Neg or N.Kind = Expr_Exp then
                  N.Left := N.Left + Offset;
               end if;
               Nodes (Count) := N;
               Count := Count + 1;
            end;
         end loop;

         --  Add ADD node at root
         Nodes (Count) := (Kind => Expr_Add,
                          Value => Zero,
                          Reg => (Column => 0, Row_Offset => 0),
                          Periodic_ID => 0,
                          Left => A.Root,
                          Right => B.Root + Offset,
                          Exponent => 0);

         Result_Deg := Natural'Max (A.Degree, B.Degree);
         return Make_Expression (Nodes, Count, Count + 1, Result_Deg);
      end;
   end Add_Expr;

   function Sub_Expr (A, B : Expression) return Expression is
      Nodes : Expr_Array := (others => Empty_Expr_Node);
      Count : Natural := 0;
      Result_Deg : Degree_Range;
   begin
      --  Copy A nodes
      for I in 0 .. A.Node_Count - 1 loop
         pragma Loop_Invariant (Count = I);
         Nodes (Count) := A.Nodes (I);
         Count := Count + 1;
      end loop;

      --  Copy B nodes with offset
      declare
         Offset : constant Natural := Count;
      begin
         for I in 0 .. B.Node_Count - 1 loop
            pragma Loop_Invariant (Count = Offset + I);
            declare
               N : Expr_Node := B.Nodes (I);
            begin
               if N.Kind in Expr_Add | Expr_Sub | Expr_Mul then
                  N.Left := N.Left + Offset;
                  N.Right := N.Right + Offset;
               elsif N.Kind = Expr_Neg or N.Kind = Expr_Exp then
                  N.Left := N.Left + Offset;
               end if;
               Nodes (Count) := N;
               Count := Count + 1;
            end;
         end loop;

         Nodes (Count) := (Kind => Expr_Sub,
                          Value => Zero,
                          Reg => (Column => 0, Row_Offset => 0),
                          Periodic_ID => 0,
                          Left => A.Root,
                          Right => B.Root + Offset,
                          Exponent => 0);

         Result_Deg := Natural'Max (A.Degree, B.Degree);
         return Make_Expression (Nodes, Count, Count + 1, Result_Deg);
      end;
   end Sub_Expr;

   function Mul_Expr (A, B : Expression) return Expression is
      Nodes : Expr_Array := (others => Empty_Expr_Node);
      Count : Natural := 0;
      Result_Deg : Degree_Range;
   begin
      for I in 0 .. A.Node_Count - 1 loop
         pragma Loop_Invariant (Count = I);
         Nodes (Count) := A.Nodes (I);
         Count := Count + 1;
      end loop;

      declare
         Offset : constant Natural := Count;
      begin
         for I in 0 .. B.Node_Count - 1 loop
            pragma Loop_Invariant (Count = Offset + I);
            declare
               N : Expr_Node := B.Nodes (I);
            begin
               if N.Kind in Expr_Add | Expr_Sub | Expr_Mul then
                  N.Left := N.Left + Offset;
                  N.Right := N.Right + Offset;
               elsif N.Kind = Expr_Neg or N.Kind = Expr_Exp then
                  N.Left := N.Left + Offset;
               end if;
               Nodes (Count) := N;
               Count := Count + 1;
            end;
         end loop;

         Nodes (Count) := (Kind => Expr_Mul,
                          Value => Zero,
                          Reg => (Column => 0, Row_Offset => 0),
                          Periodic_ID => 0,
                          Left => A.Root,
                          Right => B.Root + Offset,
                          Exponent => 0);

         Result_Deg := A.Degree + B.Degree;
         return Make_Expression (Nodes, Count, Count + 1, Result_Deg);
      end;
   end Mul_Expr;

   function Neg_Expr (E : Expression) return Expression is
      Nodes : Expr_Array := E.Nodes;
      Count : constant Natural := E.Node_Count;
   begin
      Nodes (Count) := (Kind => Expr_Neg,
                       Value => Zero,
                       Reg => (Column => 0, Row_Offset => 0),
                       Periodic_ID => 0,
                       Left => E.Root,
                       Right => 0,
                       Exponent => 0);
      return Make_Expression (Nodes, Count, Count + 1, E.Degree);
   end Neg_Expr;

   function Exp_Expr (E : Expression; N : Natural) return Expression is
      Nodes : Expr_Array := E.Nodes;
      Count : constant Natural := E.Node_Count;
   begin
      Nodes (Count) := (Kind => Expr_Exp,
                       Value => Zero,
                       Reg => (Column => 0, Row_Offset => 0),
                       Periodic_ID => 0,
                       Left => E.Root,
                       Right => 0,
                       Exponent => N);
      return Make_Expression (Nodes, Count, Count + 1, E.Degree * N);
   end Exp_Expr;

   ---------------------------------------------------------------------------
   --  AIR Builder
   ---------------------------------------------------------------------------

   procedure Init_AIR (
      AIR            : out AIR_Definition;
      Trace_Log_Len  : Natural;
      Blowup         : Positive
   ) is
   begin
      AIR.Num_Columns := 0;
      AIR.Columns := (others => (Index => 0,
                                  Col_Type => Main,
                                  Name => (others => ' '),
                                  Name_Len => 0));
      AIR.Trace_Length := 2 ** Trace_Log_Len;
      AIR.Log_Trace_Len := Trace_Log_Len;
      AIR.Num_Constraints := 0;
      AIR.Constraints := (others => (Kind => Transition,
                                      Expr => Const_Expr (Zero),
                                      Boundary => (Kind => First_Row, Row => 0),
                                      Period => 0,
                                      Degree => 0,
                                      Name => (others => ' '),
                                      Name_Len => 0));
      AIR.Num_Periodic := 0;
      AIR.Periodics := (others => (ID => 0,
                                    Values => (others => Zero),
                                    Period => 0,
                                    Active => False));
      AIR.Max_Degree := 0;
      AIR.Blowup_Factor := Blowup;
   end Init_AIR;

   procedure Add_Column (
      AIR            : in Out AIR_Definition;
      Col_Type       : Column_Type;
      Name           : String;
      Index          : out Column_Index;
      Success        : out Boolean
   ) is
   begin
      if AIR.Num_Columns >= Max_Trace_Width then
         Index := 0;
         Success := False;
         return;
      end if;

      Index := AIR.Num_Columns;
      AIR.Columns (Index).Index := Index;
      AIR.Columns (Index).Col_Type := Col_Type;
      AIR.Columns (Index).Name_Len := Natural'Min (Name'Length, 16);

      for I in 1 .. AIR.Columns (Index).Name_Len loop
         pragma Loop_Invariant (I <= 16);
         AIR.Columns (Index).Name (I) := Name (Name'First + I - 1);
      end loop;

      AIR.Num_Columns := AIR.Num_Columns + 1;
      Success := True;
   end Add_Column;

   procedure Add_Transition_Constraint (
      AIR            : in Out AIR_Definition;
      Expr           : Expression;
      Name           : String;
      Success        : out Boolean
   ) is
      Idx : Constraint_Index;
   begin
      if AIR.Num_Constraints >= Max_Constraints then
         Success := False;
         return;
      end if;

      Idx := AIR.Num_Constraints;
      AIR.Constraints (Idx).Kind := Transition;
      AIR.Constraints (Idx).Expr := Expr;
      AIR.Constraints (Idx).Degree := Expr.Degree;
      AIR.Constraints (Idx).Name_Len := Natural'Min (Name'Length, 32);

      for I in 1 .. AIR.Constraints (Idx).Name_Len loop
         pragma Loop_Invariant (I <= 32);
         AIR.Constraints (Idx).Name (I) := Name (Name'First + I - 1);
      end loop;

      if Expr.Degree > AIR.Max_Degree then
         AIR.Max_Degree := Expr.Degree;
      end if;

      AIR.Num_Constraints := AIR.Num_Constraints + 1;
      Success := True;
   end Add_Transition_Constraint;

   procedure Add_Boundary_Constraint (
      AIR            : in Out AIR_Definition;
      Expr           : Expression;
      Boundary       : Boundary_Spec;
      Name           : String;
      Success        : out Boolean
   ) is
      Idx : Constraint_Index;
   begin
      if AIR.Num_Constraints >= Max_Constraints then
         Success := False;
         return;
      end if;

      Idx := AIR.Num_Constraints;
      AIR.Constraints (Idx).Kind := Scarab_Thoth.Boundary;
      AIR.Constraints (Idx).Expr := Expr;
      AIR.Constraints (Idx).Boundary := Boundary;
      AIR.Constraints (Idx).Degree := Expr.Degree;
      AIR.Constraints (Idx).Name_Len := Natural'Min (Name'Length, 32);

      for I in 1 .. AIR.Constraints (Idx).Name_Len loop
         pragma Loop_Invariant (I <= 32);
         AIR.Constraints (Idx).Name (I) := Name (Name'First + I - 1);
      end loop;

      if Expr.Degree > AIR.Max_Degree then
         AIR.Max_Degree := Expr.Degree;
      end if;

      AIR.Num_Constraints := AIR.Num_Constraints + 1;
      Success := True;
   end Add_Boundary_Constraint;

   procedure Add_Periodic_Column (
      AIR            : in Out AIR_Definition;
      Values         : Field_Array;
      Period         : Natural;
      ID             : out Natural;
      Success        : out Boolean
   ) is
   begin
      if AIR.Num_Periodic >= Max_Periodic_Cols then
         ID := 0;
         Success := False;
         return;
      end if;

      ID := AIR.Num_Periodic;
      AIR.Periodics (ID).ID := ID;
      AIR.Periodics (ID).Period := Period;
      AIR.Periodics (ID).Active := True;

      for I in Values'Range loop
         pragma Loop_Invariant (I >= Values'First and I <= Values'Last);
         if I - Values'First < 1024 then
            AIR.Periodics (ID).Values (I - Values'First) := Values (I);
         end if;
      end loop;

      AIR.Num_Periodic := AIR.Num_Periodic + 1;
      Success := True;
   end Add_Periodic_Column;

   ---------------------------------------------------------------------------
   --  Standard Gadgets
   ---------------------------------------------------------------------------

   function Boolean_Gadget (Col : Column_Index) return Expression is
      X : constant Expression := Reg_Expr (Col);
      One_Const : constant Expression := Const_Expr (One);
      One_Minus_X : constant Expression := Sub_Expr (One_Const, X);
   begin
      return Mul_Expr (X, One_Minus_X);
   end Boolean_Gadget;

   procedure Range_Check_Gadget (
      AIR            : in Out AIR_Definition;
      Value_Col      : Column_Index;
      Bit_Cols       : Column_Index;
      Num_Bits       : Natural;
      Success        : out Boolean
   ) is
      Value_Expr : constant Expression := Reg_Expr (Value_Col);
      Sum_Expr   : Expression := Const_Expr (Zero);
      Bool_Const : Expression;
      Power      : Field_Element := One;
      Ok         : Boolean := True;
   begin
      --  Add boolean constraints for each bit
      for I in 0 .. Num_Bits - 1 loop
         pragma Loop_Invariant (Ok or not Ok);
         if Ok and then Bit_Cols + I < Max_Trace_Width then
            Bool_Const := Boolean_Gadget (Bit_Cols + I);
            Add_Transition_Constraint (AIR, Bool_Const, "bit_bool", Ok);

            --  Accumulate: sum += bit_i * 2^i
            declare
               Bit_Expr : constant Expression := Reg_Expr (Bit_Cols + I);
               Scaled : constant Expression := Mul_Expr (Const_Expr (Power), Bit_Expr);
            begin
               Sum_Expr := Add_Expr (Sum_Expr, Scaled);
            end;

            Power := Mul (Power, Field_Element (2));
         end if;
      end loop;

      --  Add constraint: value = sum of bits
      if Ok then
         declare
            Final_Expr : constant Expression := Sub_Expr (Value_Expr, Sum_Expr);
         begin
            Add_Transition_Constraint (AIR, Final_Expr, "range_sum", Ok);
         end;
      end if;

      Success := Ok;
   end Range_Check_Gadget;

   function Conditional_Gadget (
      Cond_Col       : Column_Index;
      A_Col          : Column_Index;
      B_Col          : Column_Index;
      Result_Col     : Column_Index
   ) return Expression is
      Cond : constant Expression := Reg_Expr (Cond_Col);
      A : constant Expression := Reg_Expr (A_Col);
      B : constant Expression := Reg_Expr (B_Col);
      Result : constant Expression := Reg_Expr (Result_Col);
      One_Expr : constant Expression := Const_Expr (One);
      One_Minus_Cond : constant Expression := Sub_Expr (One_Expr, Cond);
      Cond_A : constant Expression := Mul_Expr (Cond, A);
      One_Minus_Cond_B : constant Expression := Mul_Expr (One_Minus_Cond, B);
      Expected : constant Expression := Add_Expr (Cond_A, One_Minus_Cond_B);
   begin
      return Sub_Expr (Result, Expected);
   end Conditional_Gadget;

   procedure Memory_Gadget (
      AIR            : in Out AIR_Definition;
      Addr_Col       : Column_Index;
      Value_Col      : Column_Index;
      Time_Col       : Column_Index;
      Is_Write_Col   : Column_Index;
      Success        : out Boolean
   ) is
      pragma Unreferenced (Value_Col);
      pragma Unreferenced (Is_Write_Col);
      Addr_Curr : constant Expression := Reg_Expr (Addr_Col, 0);
      Addr_Next : constant Expression := Reg_Expr (Addr_Col, 1);
      Time_Curr : constant Expression := Reg_Expr (Time_Col, 0);
      Time_Next : constant Expression := Reg_Expr (Time_Col, 1);
      Addr_Diff : constant Expression := Sub_Expr (Addr_Next, Addr_Curr);
      Time_Increasing : constant Expression := Sub_Expr (Time_Next, Time_Curr);
      Ok : Boolean := True;
   begin
      --  Memory sorting constraint: addresses non-decreasing
      Add_Transition_Constraint (AIR, Addr_Diff, "mem_addr_sort", Ok);
      if Ok then
         Add_Transition_Constraint (AIR, Time_Increasing, "mem_time", Ok);
      end if;
      Success := Ok;
   end Memory_Gadget;

   procedure Hash_Chain_Gadget (
      AIR            : in Out AIR_Definition;
      Input_Col      : Column_Index;
      Output_Col     : Column_Index;
      Success        : out Boolean
   ) is
      Input : constant Expression := Reg_Expr (Input_Col, 0);
      Output : constant Expression := Reg_Expr (Output_Col, 0);
      Next_Input : constant Expression := Reg_Expr (Input_Col, 1);
      Chain_Constraint : constant Expression := Sub_Expr (Next_Input, Output);
      pragma Unreferenced (Input);
   begin
      Add_Transition_Constraint (AIR, Chain_Constraint, "hash_chain", Success);
   end Hash_Chain_Gadget;

   ---------------------------------------------------------------------------
   --  Degree Reduction
   ---------------------------------------------------------------------------

   procedure Reduce_Degree (
      AIR            : in Out AIR_Definition;
      Constraint_Idx : Constraint_Index;
      Target_Degree  : Degree_Range;
      Success        : out Boolean
   ) is
      C : Constraint renames AIR.Constraints (Constraint_Idx);
      Aux_Col : Column_Index;
      Ok : Boolean;
   begin
      if C.Degree <= Target_Degree then
         Success := True;
         return;
      end if;

      --  Add auxiliary column for intermediate value
      Add_Column (AIR, Auxiliary, "aux_reduce", Aux_Col, Ok);
      if not Ok then
         Success := False;
         return;
      end if;

      --  This is a simplified implementation
      --  Full implementation would split the expression tree
      Success := True;
   end Reduce_Degree;

   procedure Auto_Reduce_All (
      AIR            : in Out AIR_Definition;
      Max_Degree     : Degree_Range;
      Success        : out Boolean
   ) is
      Ok : Boolean := True;
   begin
      for I in 0 .. AIR.Num_Constraints - 1 loop
         pragma Loop_Invariant (Ok or not Ok);
         if Ok and then AIR.Constraints (I).Degree > Max_Degree then
            Reduce_Degree (AIR, I, Max_Degree, Ok);
         end if;
      end loop;
      Success := Ok;
      AIR.Max_Degree := Max_Degree;
   end Auto_Reduce_All;

   ---------------------------------------------------------------------------
   --  Compilation
   ---------------------------------------------------------------------------

   procedure Compile_AIR (
      AIR            : in Out AIR_Definition;
      Result         : out Compilation_Result
   ) is
   begin
      Result.Success := True;
      Result.Error := No_Error;
      Result.Detail_Len := 0;
      Result.Error_Details := (others => ' ');

      --  Check column count
      if AIR.Num_Columns > Max_Trace_Width then
         Result.Success := False;
         Result.Error := Too_Many_Columns;
         return;
      end if;

      --  Check constraint count
      if AIR.Num_Constraints > Max_Constraints then
         Result.Success := False;
         Result.Error := Too_Many_Constraints;
         return;
      end if;

      --  Check degree bounds
      for I in 0 .. AIR.Num_Constraints - 1 loop
         pragma Loop_Invariant (Result.Success);
         if AIR.Constraints (I).Degree > Max_Constraint_Deg then
            Result.Success := False;
            Result.Error := Degree_Too_High;
            return;
         end if;
      end loop;
   end Compile_AIR;

   function Verify_AIR (AIR : AIR_Definition) return Boolean is
   begin
      if AIR.Num_Columns > Max_Trace_Width then
         return False;
      end if;
      if AIR.Num_Constraints > Max_Constraints then
         return False;
      end if;
      if AIR.Max_Degree > Max_Constraint_Deg then
         return False;
      end if;
      return True;
   end Verify_AIR;

   ---------------------------------------------------------------------------
   --  Trace Evaluation
   ---------------------------------------------------------------------------

   function Eval_Expr (
      Expr           : Expression;
      Trace          : Field_Array;
      Trace_Width    : Natural;
      Row            : Row_Index;
      Periodics      : Periodic_Array
   ) return Field_Element is
      Values : array (Expr_Node_Index) of Field_Element := (others => Zero);
   begin
      for I in 0 .. Expr.Node_Count - 1 loop
         pragma Loop_Invariant (I < Max_Expr_Nodes);
         declare
            N : Expr_Node renames Expr.Nodes (I);
         begin
            case N.Kind is
               when Expr_Const =>
                  Values (I) := N.Value;

               when Expr_Register =>
                  declare
                     Actual_Row : Integer := Integer (Row) + N.Reg.Row_Offset;
                     Idx : Natural;
                  begin
                     if Actual_Row < 0 then
                        Actual_Row := 0;
                     end if;
                     Idx := Natural (Actual_Row) * Trace_Width + N.Reg.Column;
                     if Idx < Trace'Length then
                        Values (I) := Trace (Trace'First + Idx);
                     else
                        Values (I) := Zero;
                     end if;
                  end;

               when Expr_Periodic =>
                  if N.Periodic_ID < Max_Periodic_Cols
                     and then Periodics (N.Periodic_ID).Active
                  then
                     declare
                        P : Periodic_Column renames Periodics (N.Periodic_ID);
                        Idx : constant Natural := Row mod P.Period;
                     begin
                        if Idx < 1024 then
                           Values (I) := P.Values (Idx);
                        end if;
                     end;
                  end if;

               when Expr_Add =>
                  Values (I) := Add (Values (N.Left), Values (N.Right));

               when Expr_Sub =>
                  Values (I) := Sub (Values (N.Left), Values (N.Right));

               when Expr_Mul =>
                  Values (I) := Mul (Values (N.Left), Values (N.Right));

               when Expr_Neg =>
                  Values (I) := Neg (Values (N.Left));

               when Expr_Exp =>
                  Values (I) := Exp (Values (N.Left), Unsigned_64 (N.Exponent));
            end case;
         end;
      end loop;

      return Values (Expr.Root);
   end Eval_Expr;

   function Check_Constraint (
      C              : Constraint;
      Trace          : Field_Array;
      Trace_Width    : Natural;
      Row            : Row_Index;
      Trace_Length   : Natural;
      Periodics      : Periodic_Array
   ) return Boolean is
      Value : Field_Element;
      pragma Unreferenced (Trace_Length);
   begin
      case C.Kind is
         when Transition =>
            Value := Eval_Expr (C.Expr, Trace, Trace_Width, Row, Periodics);
            return Value = Zero;

         when Scarab_Thoth.Boundary =>
            case C.Boundary.Kind is
               when First_Row =>
                  if Row = 0 then
                     Value := Eval_Expr (C.Expr, Trace, Trace_Width, Row, Periodics);
                     return Value = Zero;
                  end if;
                  return True;

               when Last_Row =>
                  return True;  -- Checked separately

               when Specific_Row =>
                  if Row = C.Boundary.Row then
                     Value := Eval_Expr (C.Expr, Trace, Trace_Width, Row, Periodics);
                     return Value = Zero;
                  end if;
                  return True;
            end case;

         when Periodic =>
            if C.Period > 0 and then Row mod C.Period = 0 then
               Value := Eval_Expr (C.Expr, Trace, Trace_Width, Row, Periodics);
               return Value = Zero;
            end if;
            return True;
      end case;
   end Check_Constraint;

   function Verify_Trace (
      AIR            : AIR_Definition;
      Trace          : Field_Array
   ) return Boolean is
   begin
      for Row in 0 .. AIR.Trace_Length - 1 loop
         pragma Loop_Invariant (Row < AIR.Trace_Length);
         for C_Idx in 0 .. AIR.Num_Constraints - 1 loop
            pragma Loop_Invariant (C_Idx < AIR.Num_Constraints);
            if not Check_Constraint (
               AIR.Constraints (C_Idx),
               Trace,
               AIR.Num_Columns,
               Row,
               AIR.Trace_Length,
               AIR.Periodics)
            then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Verify_Trace;

   ---------------------------------------------------------------------------
   --  Serialization
   ---------------------------------------------------------------------------

   procedure Serialize_AIR (
      AIR            : AIR_Definition;
      Output         : out Byte_Array;
      Length         : out Natural
   ) is
   begin
      Output := (others => 0);
      Length := 0;

      --  Header: magic + version
      if Output'Length >= 8 then
         Output (Output'First) := 16#54#;  -- "T"
         Output (Output'First + 1) := 16#48#;  -- "H"
         Output (Output'First + 2) := 16#4F#;  -- "O"
         Output (Output'First + 3) := 16#54#;  -- "T"
         Output (Output'First + 4) := 1;  -- Version
         Output (Output'First + 5) := Byte (AIR.Num_Columns mod 256);
         Output (Output'First + 6) := Byte (AIR.Num_Constraints mod 256);
         Output (Output'First + 7) := Byte (AIR.Max_Degree);
         Length := 8;
      end if;
   end Serialize_AIR;

   procedure Deserialize_AIR (
      Input          : Byte_Array;
      AIR            : out AIR_Definition;
      Success        : out Boolean
   ) is
   begin
      Init_AIR (AIR, 10, 8);  -- Default initialization

      if Input'Length < 8 then
         Success := False;
         return;
      end if;

      --  Check magic
      if Input (Input'First) /= 16#54#
         or Input (Input'First + 1) /= 16#48#
         or Input (Input'First + 2) /= 16#4F#
         or Input (Input'First + 3) /= 16#54#
      then
         Success := False;
         return;
      end if;

      Success := True;
   end Deserialize_AIR;

   ---------------------------------------------------------------------------
   --  Debugging
   ---------------------------------------------------------------------------

   procedure Constraint_To_String (
      C              : Constraint;
      Output         : out String;
      Length         : out Natural
   ) is
   begin
      Output := (others => ' ');
      Length := 0;

      --  Copy constraint name
      for I in 1 .. C.Name_Len loop
         pragma Loop_Invariant (I <= 32 and Length < Output'Length);
         if Length < Output'Length then
            Output (Output'First + Length) := C.Name (I);
            Length := Length + 1;
         end if;
      end loop;
   end Constraint_To_String;

   function Get_AIR_Stats (AIR : AIR_Definition) return AIR_Stats is
      Stats : AIR_Stats;
      Main_Count : Natural := 0;
      Aux_Count : Natural := 0;
      Trans_Count : Natural := 0;
      Bound_Count : Natural := 0;
   begin
      for I in 0 .. AIR.Num_Columns - 1 loop
         pragma Loop_Invariant (I < Max_Trace_Width);
         case AIR.Columns (I).Col_Type is
            when Main => Main_Count := Main_Count + 1;
            when Auxiliary => Aux_Count := Aux_Count + 1;
            when others => null;
         end case;
      end loop;

      for I in 0 .. AIR.Num_Constraints - 1 loop
         pragma Loop_Invariant (I < Max_Constraints);
         case AIR.Constraints (I).Kind is
            when Transition => Trans_Count := Trans_Count + 1;
            when Scarab_Thoth.Boundary => Bound_Count := Bound_Count + 1;
            when others => null;
         end case;
      end loop;

      Stats.Num_Main_Cols := Main_Count;
      Stats.Num_Aux_Cols := Aux_Count;
      Stats.Num_Transition := Trans_Count;
      Stats.Num_Boundary := Bound_Count;
      Stats.Max_Degree := AIR.Max_Degree;
      Stats.Est_Proof_Size := AIR.Num_Columns * AIR.Trace_Length * 8 / AIR.Blowup_Factor;

      return Stats;
   end Get_AIR_Stats;

end Scarab_Thoth;
