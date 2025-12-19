--  IoT Device Registry Contract
--  Post-Quantum Secure Device Management for Embedded Systems
--  Target: Raspberry Pi Pico W, ESP32, ARM Cortex-M, RISC-V
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_Address_Types; use Anubis_Address_Types;

package IoT_Device_Registry with
   SPARK_Mode => On
is
   Contract_Name    : constant String := "iot_device_registry";
   Contract_Version : constant String := "1.0.0";

   ---------------------------------------------------------------------------
   --  Device Configuration
   ---------------------------------------------------------------------------

   Max_Devices      : constant := 10000;
   Max_Sensors      : constant := 8;
   Max_Data_Points  : constant := 100;  --  Per device rolling buffer

   --  Device types (embedded platforms)
   type Device_Type is (
      Unknown,
      Pico_W,           --  Raspberry Pi Pico W (RP2040 + WiFi)
      Pico_2W,          --  Raspberry Pi Pico 2 W (RP2350 + WiFi)
      ESP32,            --  Espressif ESP32
      ESP32_S3,         --  ESP32-S3 with AI acceleration
      STM32_F4,         --  STMicro Cortex-M4
      STM32_H7,         --  STMicro Cortex-M7
      NRF52840,         --  Nordic BLE + Thread
      RISC_V_Generic,   --  Generic RISC-V MCU
      Custom            --  Custom hardware
   );

   --  Device status
   type Device_Status is (
      Inactive,         --  Registered but not active
      Online,           --  Responding to heartbeats
      Offline,          --  Missed heartbeat threshold
      Maintenance,      --  Under maintenance
      Compromised,      --  Security alert
      Decommissioned    --  No longer in service
   );

   --  Sensor types
   type Sensor_Type is (
      None,
      Temperature,      --  Celsius * 100 (fixed point)
      Humidity,         --  Percent * 100
      Pressure,         --  hPa * 100
      Light,            --  Lux
      Motion,           --  Boolean (0/1)
      Voltage,          --  mV
      Current,          --  mA
      GPS_Lat,          --  Latitude * 1_000_000
      GPS_Lon,          --  Longitude * 1_000_000
      Accelerometer,    --  mg (milli-g)
      Gyroscope,        --  mdps (milli-degrees per second)
      CO2,              --  ppm
      Air_Quality,      --  AQI index
      Sound_Level,      --  dB * 10
      Custom_Sensor     --  User-defined
   );

   ---------------------------------------------------------------------------
   --  Type Definitions
   ---------------------------------------------------------------------------

   --  Device ID (index into registry)
   type Device_ID is range 0 .. Max_Devices - 1;

   --  Hardware ID (unique per device - could be MAC, chip ID, etc.)
   subtype Hardware_ID is Hash256;

   --  Firmware version (semantic versioning encoded)
   type Firmware_Version is record
      Major : Unsigned_8;
      Minor : Unsigned_8;
      Patch : Unsigned_16;
   end record;

   --  Sensor configuration
   type Sensor_Config is record
      Sensor   : Sensor_Type;
      Enabled  : Boolean;
      Interval : Unsigned_32;  --  Reading interval in seconds
   end record;

   type Sensor_Array is array (1 .. Max_Sensors) of Sensor_Config;

   --  Sensor reading with timestamp
   type Sensor_Reading is record
      Sensor_Idx : Unsigned_8;     --  Which sensor (1..Max_Sensors)
      Value      : Integer_64;     --  Sensor value (scaled integer)
      Timestamp  : Unsigned_64;    --  Unix timestamp
   end record;

   type Reading_Index is range 0 .. Max_Data_Points - 1;
   type Reading_Buffer is array (Reading_Index) of Sensor_Reading;

   --  Device record
   type Device is record
      Status          : Device_Status;
      Device_Kind     : Device_Type;
      Owner           : Account_ID;       --  AAS-001 owner address
      Hardware        : Hardware_ID;      --  Unique hardware identifier
      Firmware        : Firmware_Version;
      Sensors         : Sensor_Array;
      Last_Heartbeat  : Unsigned_64;      --  Last seen timestamp
      Heartbeat_Count : Unsigned_64;      --  Total heartbeats
      Data_Count      : Unsigned_64;      --  Total readings submitted
      Registered_At   : Unsigned_64;      --  Registration timestamp
      Location_Hash   : Hash256;          --  Optional location identifier
   end record;

   type Device_Array is array (Device_ID) of Device;

   --  Fleet/group for managing device collections
   type Fleet_ID is range 0 .. 255;

   --  Contract state
   type Registry_State is record
      Initialized      : Boolean;
      Admin            : Account_ID;        --  Registry administrator
      Total_Devices    : Unsigned_64;
      Online_Count     : Unsigned_64;
      Total_Readings   : Unsigned_64;
      Heartbeat_Timeout: Unsigned_64;       --  Seconds before offline
      Devices          : Device_Array;
      Cert_Level       : Certification_Level;
   end record;

   --  Empty device for initialization
   Empty_Device : constant Device := (
      Status          => Inactive,
      Device_Kind     => Unknown,
      Owner           => (others => 0),
      Hardware        => Hash256_Zero,
      Firmware        => (Major => 0, Minor => 0, Patch => 0),
      Sensors         => (others => (Sensor => None, Enabled => False, Interval => 0)),
      Last_Heartbeat  => 0,
      Heartbeat_Count => 0,
      Data_Count      => 0,
      Registered_At   => 0,
      Location_Hash   => Hash256_Zero);

   ---------------------------------------------------------------------------
   --  Registry Management
   ---------------------------------------------------------------------------

   procedure Initialize (
      State   : in out Registry_State;
      Admin   : Account_ID;
      Timeout : Unsigned_64)
      with Global => null,
           Pre    => not State.Initialized and Timeout > 0,
           Post   => State.Initialized and
                     State.Total_Devices = 0 and
                     State.Heartbeat_Timeout = Timeout;

   ---------------------------------------------------------------------------
   --  Device Registration
   ---------------------------------------------------------------------------

   procedure Register_Device (
      State       : in Out Registry_State;
      Owner       : Account_ID;
      Hardware    : Hardware_ID;
      Device_Kind : Device_Type;
      Timestamp   : Unsigned_64;
      Did         : out Device_ID;
      Success     : out Boolean)
      with Global => null,
           Pre    => State.Initialized;

   procedure Update_Firmware (
      State   : in Out Registry_State;
      Did     : Device_ID;
      Caller  : Account_ID;
      Version : Firmware_Version;
      Success : out Boolean)
      with Global => null,
           Pre    => State.Initialized;

   procedure Configure_Sensor (
      State      : in Out Registry_State;
      Did        : Device_ID;
      Caller     : Account_ID;
      Sensor_Idx : Unsigned_8;
      Config     : Sensor_Config;
      Success    : out Boolean)
      with Global => null,
           Pre    => State.Initialized and Sensor_Idx in 1 .. Max_Sensors;

   procedure Transfer_Ownership (
      State     : in Out Registry_State;
      Did       : Device_ID;
      Caller    : Account_ID;
      New_Owner : Account_ID;
      Success   : out Boolean)
      with Global => null,
           Pre    => State.Initialized;

   procedure Decommission_Device (
      State   : in Out Registry_State;
      Did     : Device_ID;
      Caller  : Account_ID;
      Success : out Boolean)
      with Global => null,
           Pre    => State.Initialized;

   ---------------------------------------------------------------------------
   --  Device Operations (called by devices)
   ---------------------------------------------------------------------------

   procedure Heartbeat (
      State        : in Out Registry_State;
      Did          : Device_ID;
      Device_Auth  : Account_ID;  --  Device's signing key
      Timestamp    : Unsigned_64;
      Success      : out Boolean)
      with Global => null,
           Pre    => State.Initialized;

   procedure Submit_Reading (
      State       : in Out Registry_State;
      Did         : Device_ID;
      Device_Auth : Account_ID;
      Reading     : Sensor_Reading;
      Success     : out Boolean)
      with Global => null,
           Pre    => State.Initialized;

   procedure Report_Compromise (
      State       : in Out Registry_State;
      Did         : Device_ID;
      Device_Auth : Account_ID;
      Success     : out Boolean)
      with Global => null,
           Pre    => State.Initialized;

   ---------------------------------------------------------------------------
   --  Admin Operations
   ---------------------------------------------------------------------------

   procedure Set_Device_Status (
      State      : in Out Registry_State;
      Did        : Device_ID;
      Caller     : Account_ID;
      New_Status : Device_Status;
      Success    : out Boolean)
      with Global => null,
           Pre    => State.Initialized;

   procedure Update_Timeout (
      State       : in Out Registry_State;
      Caller      : Account_ID;
      New_Timeout : Unsigned_64;
      Success     : out Boolean)
      with Global => null,
           Pre    => State.Initialized and New_Timeout > 0;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Device_Status (
      State : Registry_State;
      Did   : Device_ID) return Device_Status
      with Global => null;

   function Get_Device_Type (
      State : Registry_State;
      Did   : Device_ID) return Device_Type
      with Global => null;

   function Get_Device_Owner (
      State : Registry_State;
      Did   : Device_ID) return Account_ID
      with Global => null;

   function Get_Last_Heartbeat (
      State : Registry_State;
      Did   : Device_ID) return Unsigned_64
      with Global => null;

   function Get_Firmware (
      State : Registry_State;
      Did   : Device_ID) return Firmware_Version
      with Global => null;

   function Get_Total_Devices (State : Registry_State) return Unsigned_64
      with Global => null;

   function Get_Online_Count (State : Registry_State) return Unsigned_64
      with Global => null;

   function Get_Total_Readings (State : Registry_State) return Unsigned_64
      with Global => null;

   function Is_Device_Online (
      State        : Registry_State;
      Did          : Device_ID;
      Current_Time : Unsigned_64) return Boolean
      with Global => null;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Account_Equal (A, B : Account_ID) return Boolean
      with Global => null,
           Post   => Account_Equal'Result =
                     (for all I in Account_ID_Index => A (I) = B (I));

   function Hardware_Equal (A, B : Hardware_ID) return Boolean
      with Global => null;

   function Device_Type_String (DT : Device_Type) return String
      with Global => null;

end IoT_Device_Registry;
