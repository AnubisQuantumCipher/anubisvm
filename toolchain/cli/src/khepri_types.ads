-------------------------------------------------------------------------------
--  KHEPRI Types - Common type definitions for KHEPRI toolchain
--
--  SPDX-License-Identifier: Apache-2.0
-------------------------------------------------------------------------------

pragma SPARK_Mode (On);

with Anubis_Types; use Anubis_Types;

package Khepri_Types with
   SPARK_Mode => On
is
   ---------------------------------------------------------------------------
   --  Contract Types
   ---------------------------------------------------------------------------

   type Contract_ID is new Unsigned_64;

   type Contract_State is (
      State_Pending,
      State_Deployed,
      State_Active,
      State_Paused,
      State_Terminated
   );

   ---------------------------------------------------------------------------
   --  Transaction Types
   ---------------------------------------------------------------------------

   type TX_Type is (
      TX_Deploy,
      TX_Call,
      TX_Send,
      TX_Transfer
   );

   type TX_Status is (
      Status_Pending,
      Status_Included,
      Status_Confirmed,
      Status_Failed,
      Status_Reverted
   );

   ---------------------------------------------------------------------------
   --  Network Types
   ---------------------------------------------------------------------------

   type Chain_ID is new Unsigned_64;

   type Network_ID is (
      Mainnet,
      Testnet,
      Devnet,
      Local
   );

end Khepri_Types;
