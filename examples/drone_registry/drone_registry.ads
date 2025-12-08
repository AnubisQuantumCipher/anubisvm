--  Drone Registry Contract
--
--  Quantum-resistant drone identity and flight logging for embedded deployment.
--  This example demonstrates KHEPRI contracts for IoT/drone applications.
--
--  Features:
--  - Drone registration with ML-DSA-87 public keys
--  - Flight position logging with signatures
--  - Owner management and transfer
--  - Geofence violation detection
--  - Flight history queries
--
--  Target: Embedded ARM/RISC-V devices with minimal memory footprint
--  Certification Target: GOLD
--
--  References:
--  - KHEPRI Blueprint v1.0, Section 10.4: Drone/IoT Contract Example
--  - FAA Remote ID requirements

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_U256; use Aegis_U256;

package Drone_Registry with
   SPARK_Mode => On,
   Abstract_State => (Registry_State with External => Async_Writers),
   Initializes => Registry_State
is

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------

   --  Maximum registered drones
   Max_Drones : constant := 10_000;

   --  Maximum flight log entries per drone
   Max_Flight_Logs : constant := 1_000;

   --  Serial number length
   Serial_Length : constant := 16;

   --  Geofence coordinate precision (degrees * 10^7)
   Coordinate_Scale : constant := 10_000_000;

   ---------------------------------------------------------------------------
   --  Coordinate Types
   ---------------------------------------------------------------------------

   --  GPS coordinates with fixed-point precision
   type GPS_Coordinate is record
      Latitude   : Integer;     --  Degrees * 10^7 (-900000000 to 900000000)
      Longitude  : Integer;     --  Degrees * 10^7 (-1800000000 to 1800000000)
      Altitude   : Integer;     --  Meters above sea level
   end record;

   Null_Coordinate : constant GPS_Coordinate := (
      Latitude  => 0,
      Longitude => 0,
      Altitude  => 0
   );

   --  Geofence zone (circular)
   type Geofence_Zone is record
      Center     : GPS_Coordinate;
      Radius     : Natural;        --  Meters
      Max_Alt    : Integer;        --  Maximum altitude (meters)
      Min_Alt    : Integer;        --  Minimum altitude (meters)
      Active     : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Drone Identity Types
   ---------------------------------------------------------------------------

   --  Drone serial number (128-bit unique identifier)
   type Serial_Number is array (0 .. Serial_Length - 1) of Byte;

   Null_Serial : constant Serial_Number := (others => 0);

   --  ML-DSA-87 public key for drone identity (2592 bytes)
   MLDSA_PK_Size : constant := 2592;
   type MLDSA_Public_Key is array (0 .. MLDSA_PK_Size - 1) of Byte;

   --  ML-DSA-87 signature (4627 bytes max)
   MLDSA_Sig_Size : constant := 4627;
   type MLDSA_Signature is array (0 .. MLDSA_Sig_Size - 1) of Byte;

   --  Drone status
   type Drone_Status is (
      Status_Inactive,      --  Not currently flying
      Status_Active,        --  In flight
      Status_Emergency,     --  Emergency landing
      Status_Maintenance,   --  Under maintenance
      Status_Revoked        --  Registration revoked
   );

   --  Registered drone record
   type Drone_Record is record
      Serial       : Serial_Number;
      Public_Key   : MLDSA_Public_Key;
      Owner        : Contract_Address;
      Operator     : Contract_Address;    --  May differ from owner
      Status       : Drone_Status;
      Registered   : Unsigned_64;         --  Registration timestamp
      Last_Update  : Unsigned_64;         --  Last status update
      Flight_Count : Natural;             --  Total flights
      Valid        : Boolean;
   end record;

   ---------------------------------------------------------------------------
   --  Flight Log Types
   ---------------------------------------------------------------------------

   --  Flight log entry
   type Flight_Log_Entry is record
      Drone_Hash   : Hash256;          --  Hash of drone serial
      Timestamp    : Unsigned_64;      --  Unix timestamp
      Position     : GPS_Coordinate;
      Speed        : Natural;          --  Ground speed (cm/s)
      Heading      : Natural;          --  Degrees * 100 (0-35999)
      Vertical     : Integer;          --  Vertical speed (cm/s, + = up)
      Battery      : Natural;          --  Battery percentage (0-100)
      Valid        : Boolean;
   end record;

   Null_Log_Entry : constant Flight_Log_Entry := (
      Drone_Hash => (others => 0),
      Timestamp  => 0,
      Position   => Null_Coordinate,
      Speed      => 0,
      Heading    => 0,
      Vertical   => 0,
      Battery    => 0,
      Valid      => False
   );

   ---------------------------------------------------------------------------
   --  Error Types
   ---------------------------------------------------------------------------

   type Registry_Error is (
      Error_None,
      Error_Already_Registered,
      Error_Not_Registered,
      Error_Unauthorized,
      Error_Invalid_Signature,
      Error_Invalid_Serial,
      Error_Drone_Revoked,
      Error_Geofence_Violation,
      Error_Registry_Full,
      Error_Log_Full
   );

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   --  Initialize the registry with admin address
   procedure Initialize (Admin : in Contract_Address) with
      Global => (Output => Registry_State);

   ---------------------------------------------------------------------------
   --  Drone Registration
   ---------------------------------------------------------------------------

   --  Register a new drone
   procedure Register_Drone (
      Serial     : in     Serial_Number;
      Public_Key : in     MLDSA_Public_Key;
      Owner      : in     Contract_Address;
      Caller     : in     Contract_Address;
      Success    : out    Boolean;
      Error      : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State);

   --  Update drone operator
   procedure Set_Operator (
      Serial   : in     Serial_Number;
      Operator : in     Contract_Address;
      Caller   : in     Contract_Address;
      Success  : out    Boolean;
      Error    : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State);

   --  Transfer drone ownership
   procedure Transfer_Ownership (
      Serial    : in     Serial_Number;
      New_Owner : in     Contract_Address;
      Caller    : in     Contract_Address;
      Success   : out    Boolean;
      Error     : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State);

   --  Update drone status
   procedure Set_Status (
      Serial  : in     Serial_Number;
      Status  : in     Drone_Status;
      Caller  : in     Contract_Address;
      Success : out    Boolean;
      Error   : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State);

   --  Revoke drone registration
   procedure Revoke_Drone (
      Serial  : in     Serial_Number;
      Caller  : in     Contract_Address;
      Success : out    Boolean;
      Error   : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State);

   ---------------------------------------------------------------------------
   --  Flight Logging
   ---------------------------------------------------------------------------

   --  Log flight position (called by drone)
   procedure Log_Position (
      Serial    : in     Serial_Number;
      Position  : in     GPS_Coordinate;
      Speed     : in     Natural;
      Heading   : in     Natural;
      Vertical  : in     Integer;
      Battery   : in     Natural;
      Signature : in     MLDSA_Signature;
      Timestamp : in     Unsigned_64;
      Success   : out    Boolean;
      Error     : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State),
      Pre    => Heading <= 35999 and Battery <= 100;

   --  Start a new flight session
   procedure Start_Flight (
      Serial    : in     Serial_Number;
      Position  : in     GPS_Coordinate;
      Signature : in     MLDSA_Signature;
      Caller    : in     Contract_Address;
      Success   : out    Boolean;
      Error     : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State);

   --  End flight session
   procedure End_Flight (
      Serial    : in     Serial_Number;
      Position  : in     GPS_Coordinate;
      Signature : in     MLDSA_Signature;
      Caller    : in     Contract_Address;
      Success   : out    Boolean;
      Error     : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State);

   ---------------------------------------------------------------------------
   --  Geofence Management
   ---------------------------------------------------------------------------

   --  Maximum geofence zones
   Max_Geofences : constant := 100;

   --  Add geofence zone
   procedure Add_Geofence (
      Zone    : in     Geofence_Zone;
      Caller  : in     Contract_Address;
      Zone_ID : out    Natural;
      Success : out    Boolean;
      Error   : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State);

   --  Remove geofence zone
   procedure Remove_Geofence (
      Zone_ID : in     Natural;
      Caller  : in     Contract_Address;
      Success : out    Boolean;
      Error   : out    Registry_Error
   ) with
      Global => (In_Out => Registry_State);

   --  Check if position violates any geofence
   function Check_Geofence (Position : GPS_Coordinate) return Boolean with
      Global => Registry_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   --  Check if drone is registered
   function Is_Registered (Serial : Serial_Number) return Boolean with
      Global => Registry_State,
      Volatile_Function;

   --  Get drone record
   procedure Get_Drone (
      Serial  : in     Serial_Number;
      Drone   : out    Drone_Record;
      Found   : out    Boolean
   ) with
      Global => Registry_State;

   --  Get drone owner
   function Get_Owner (Serial : Serial_Number) return Contract_Address with
      Global => Registry_State,
      Volatile_Function;

   --  Get drone status
   function Get_Status (Serial : Serial_Number) return Drone_Status with
      Global => Registry_State,
      Volatile_Function;

   --  Get flight count for drone
   function Get_Flight_Count (Serial : Serial_Number) return Natural with
      Global => Registry_State,
      Volatile_Function;

   --  Get last known position
   procedure Get_Last_Position (
      Serial   : in     Serial_Number;
      Position : out    GPS_Coordinate;
      Time     : out    Unsigned_64;
      Found    : out    Boolean
   ) with
      Global => Registry_State;

   --  Get total registered drones
   function Total_Drones return Natural with
      Global => Registry_State,
      Volatile_Function;

   --  Get active drone count
   function Active_Drones return Natural with
      Global => Registry_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Signature Verification
   ---------------------------------------------------------------------------

   --  Verify position log signature
   function Verify_Position_Signature (
      Serial    : Serial_Number;
      Position  : GPS_Coordinate;
      Timestamp : Unsigned_64;
      Signature : MLDSA_Signature
   ) return Boolean with
      Global => Registry_State,
      Volatile_Function;

   ---------------------------------------------------------------------------
   --  Events
   ---------------------------------------------------------------------------

   type Drone_Registered_Event is record
      Serial    : Serial_Number;
      Owner     : Contract_Address;
      Timestamp : Unsigned_64;
   end record;

   type Position_Logged_Event is record
      Serial    : Serial_Number;
      Position  : GPS_Coordinate;
      Timestamp : Unsigned_64;
   end record;

   type Status_Changed_Event is record
      Serial     : Serial_Number;
      Old_Status : Drone_Status;
      New_Status : Drone_Status;
      Timestamp  : Unsigned_64;
   end record;

   type Geofence_Violation_Event is record
      Serial    : Serial_Number;
      Position  : GPS_Coordinate;
      Zone_ID   : Natural;
      Timestamp : Unsigned_64;
   end record;

   type Ownership_Transferred_Event is record
      Serial    : Serial_Number;
      Old_Owner : Contract_Address;
      New_Owner : Contract_Address;
      Timestamp : Unsigned_64;
   end record;

end Drone_Registry;
