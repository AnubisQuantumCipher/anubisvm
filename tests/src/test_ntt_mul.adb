with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Anubis_MLDSA_Types; use Anubis_MLDSA_Types;
with Anubis_MLDSA_Field; use Anubis_MLDSA_Field;
with Anubis_MLDSA_NTT; use Anubis_MLDSA_NTT;
with Anubis_MLDSA_Config; use Anubis_MLDSA_Config;

procedure Test_NTT_Mul is
   A, B, C : Polynomial;
begin
   Put_Line ("Testing NTT multiplication...");
   
   --  Test 1: 1 * 1 = 1
   A := (0 => 1, others => 0);
   B := (0 => 1, others => 0);
   
   Put_Line ("A[0..3] before NTT: " & Field_Element'Image(A(0)) & "," &
             Field_Element'Image(A(1)) & "," & Field_Element'Image(A(2)));
   
   NTT (A);
   NTT (B);
   
   Put_Line ("A[0..3] after NTT: " & Field_Element'Image(A(0)) & "," &
             Field_Element'Image(A(1)) & "," & Field_Element'Image(A(2)));
   
   NTT_Mul (A, B, C);
   
   Put_Line ("C[0..3] after NTT_Mul: " & Field_Element'Image(C(0)) & "," &
             Field_Element'Image(C(1)) & "," & Field_Element'Image(C(2)));
   
   INTT (C);
   
   Put_Line ("C[0..3] after INTT: " & Field_Element'Image(C(0)) & "," &
             Field_Element'Image(C(1)) & "," & Field_Element'Image(C(2)));
   Put_Line ("Expected: 1, 0, 0");
   
   --  Test 2: X * X = X^2 where X = [0,1,0...]
   Put_Line ("");
   Put_Line ("Test 2: X * X");
   A := (1 => 1, others => 0);
   B := (1 => 1, others => 0);
   Put_Line ("A[0..3] before NTT: " & Field_Element'Image(A(0)) & "," &
             Field_Element'Image(A(1)) & "," & Field_Element'Image(A(2)));

   NTT (A);
   NTT (B);
   Put_Line ("A[0..3] after NTT: " & Field_Element'Image(A(0)) & "," &
             Field_Element'Image(A(1)) & "," & Field_Element'Image(A(2)));
   Put_Line ("A[252..255]: " & Field_Element'Image(A(252)) & "," &
             Field_Element'Image(A(253)) & "," & Field_Element'Image(A(254)) & "," &
             Field_Element'Image(A(255)));

   NTT_Mul (A, B, C);
   Put_Line ("C[0..3] after NTT_Mul: " & Field_Element'Image(C(0)) & "," &
             Field_Element'Image(C(1)) & "," & Field_Element'Image(C(2)));
   Put_Line ("C[252..255]: " & Field_Element'Image(C(252)) & "," &
             Field_Element'Image(C(253)) & "," & Field_Element'Image(C(254)) & "," &
             Field_Element'Image(C(255)));

   Put_Line ("Starting INTT...");
   INTT (C);
   Put_Line ("C[0..3] after INTT: " & Field_Element'Image(C(0)) & "," &
             Field_Element'Image(C(1)) & "," & Field_Element'Image(C(2)) & "," &
             Field_Element'Image(C(3)));
   Put_Line ("Expected: 0, 0, 1, 0 (X^2)");
end Test_NTT_Mul;
