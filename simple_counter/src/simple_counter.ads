--  simple_counter Contract
--  Certification Target: GOLD
pragma SPARK_Mode (On);

with Khepri_Types;  use Khepri_Types;
with Khepri_State;  use Khepri_State;
with Khepri_Crypto; use Khepri_Crypto;
with Khepri_Events; use Khepri_Events;

package simple_counter with
   SPARK_Mode => On
is
   --  Contract metadata
   Contract_Name    : constant String := "simple_counter";
   Contract_Version : constant String := "1.0.0";

   --  TODO: Add your contract interface here

end simple_counter;
