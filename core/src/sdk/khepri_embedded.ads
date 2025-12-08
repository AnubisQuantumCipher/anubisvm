--  KHEPRI Embedded Contract Profile
--
--  Support for running KHEPRI contracts on embedded/IoT targets.
--  Provides memory profiles, runtime configurations, and validation
--  utilities for resource-constrained devices.
--
--  Supported Targets:
--  - ARM64 bare-metal (drones, IoT gateways)
--  - ARM Cortex-M (microcontrollers)
--  - RISC-V (embedded processors)
--
--  Features:
--  - Memory constraint validation
--  - Hardware crypto abstraction
--  - Static memory allocation modes
--  - Cross-compilation support
--
--  Certification Target: GOLD

pragma SPARK_Mode (On);

package Khepri_Embedded with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Target Architecture Types
   ---------------------------------------------------------------------------

   type Target_Architecture is (
      Arch_x86_64,        --  Standard desktop/server
      Arch_aarch64,       --  ARM64 (Raspberry Pi, drones)
      Arch_arm_cortex_m,  --  ARM Cortex-M (MCUs)
      Arch_riscv64,       --  RISC-V 64-bit
      Arch_riscv32        --  RISC-V 32-bit embedded
   );

   type Target_Endianness is (Little_Endian, Big_Endian);

   type Word_Size is (Bits_32, Bits_64);

   type Target_Info is record
      Architecture : Target_Architecture;
      Endianness   : Target_Endianness;
      Word_Bits    : Word_Size;
      Has_FPU      : Boolean;
      Has_SIMD     : Boolean;
   end record;

   --  Common target configurations
   Target_x86_64 : constant Target_Info := (
      Architecture => Arch_x86_64,
      Endianness   => Little_Endian,
      Word_Bits    => Bits_64,
      Has_FPU      => True,
      Has_SIMD     => True
   );

   Target_ARM64 : constant Target_Info := (
      Architecture => Arch_aarch64,
      Endianness   => Little_Endian,
      Word_Bits    => Bits_64,
      Has_FPU      => True,
      Has_SIMD     => True
   );

   Target_Cortex_M4 : constant Target_Info := (
      Architecture => Arch_arm_cortex_m,
      Endianness   => Little_Endian,
      Word_Bits    => Bits_32,
      Has_FPU      => True,
      Has_SIMD     => False
   );

   Target_RISCV64 : constant Target_Info := (
      Architecture => Arch_riscv64,
      Endianness   => Little_Endian,
      Word_Bits    => Bits_64,
      Has_FPU      => True,
      Has_SIMD     => False
   );

   Target_RISCV32 : constant Target_Info := (
      Architecture => Arch_riscv32,
      Endianness   => Little_Endian,
      Word_Bits    => Bits_32,
      Has_FPU      => False,
      Has_SIMD     => False
   );

   ---------------------------------------------------------------------------
   --  Memory Profile Types
   ---------------------------------------------------------------------------

   --  Memory constraints for embedded targets
   type Memory_Profile is record
      Max_Stack_Size : Natural;     --  Maximum stack bytes
      Max_Data_Size  : Natural;     --  Maximum static data bytes
      Max_Code_Size  : Natural;     --  Maximum code bytes
      Max_Heap_Size  : Natural;     --  Maximum heap (0 for no heap)
   end record;

   --  Predefined profiles for different memory constraints
   Profile_Minimal : constant Memory_Profile := (
      Max_Stack_Size => 8 * 1024,        --  8KB stack
      Max_Data_Size  => 16 * 1024,       --  16KB data
      Max_Code_Size  => 64 * 1024,       --  64KB code
      Max_Heap_Size  => 0                --  No heap
   );

   Profile_Constrained : constant Memory_Profile := (
      Max_Stack_Size => 32 * 1024,       --  32KB stack
      Max_Data_Size  => 64 * 1024,       --  64KB data
      Max_Code_Size  => 256 * 1024,      --  256KB code
      Max_Heap_Size  => 0                --  No heap
   );

   Profile_Standard : constant Memory_Profile := (
      Max_Stack_Size => 64 * 1024,       --  64KB stack
      Max_Data_Size  => 256 * 1024,      --  256KB data
      Max_Code_Size  => 512 * 1024,      --  512KB code
      Max_Heap_Size  => 128 * 1024       --  128KB heap
   );

   Profile_Full : constant Memory_Profile := (
      Max_Stack_Size => 256 * 1024,      --  256KB stack
      Max_Data_Size  => 1024 * 1024,     --  1MB data
      Max_Code_Size  => 4 * 1024 * 1024, --  4MB code
      Max_Heap_Size  => 512 * 1024       --  512KB heap
   );

   Profile_Unlimited : constant Memory_Profile := (
      Max_Stack_Size => Natural'Last,
      Max_Data_Size  => Natural'Last,
      Max_Code_Size  => Natural'Last,
      Max_Heap_Size  => Natural'Last
   );

   ---------------------------------------------------------------------------
   --  Memory Validation
   ---------------------------------------------------------------------------

   --  Check if contract fits within memory profile
   function Fits_Profile (
      Contract_Stack : Natural;
      Contract_Data  : Natural;
      Contract_Code  : Natural;
      Profile        : Memory_Profile
   ) return Boolean is (
      Contract_Stack <= Profile.Max_Stack_Size
      and then Contract_Data <= Profile.Max_Data_Size
      and then Contract_Code <= Profile.Max_Code_Size
   ) with
      Global => null;

   --  Memory usage statistics
   type Memory_Usage is record
      Stack_Used   : Natural;
      Stack_Max    : Natural;
      Data_Used    : Natural;
      Data_Max     : Natural;
      Code_Used    : Natural;
      Code_Max     : Natural;
      Heap_Used    : Natural;
      Heap_Max     : Natural;
   end record;

   --  Calculate usage percentage (returns 0-100)
   function Stack_Usage_Percent (Usage : Memory_Usage) return Natural with
      Global => null,
      Pre    => Usage.Stack_Max > 0,
      Post   => Stack_Usage_Percent'Result <= 100;

   function Data_Usage_Percent (Usage : Memory_Usage) return Natural with
      Global => null,
      Pre    => Usage.Data_Max > 0,
      Post   => Data_Usage_Percent'Result <= 100;

   function Code_Usage_Percent (Usage : Memory_Usage) return Natural with
      Global => null,
      Pre    => Usage.Code_Max > 0,
      Post   => Code_Usage_Percent'Result <= 100;

   --  Check if usage is within limits
   function Is_Within_Limits (Usage : Memory_Usage) return Boolean is (
      Usage.Stack_Used <= Usage.Stack_Max
      and then Usage.Data_Used <= Usage.Data_Max
      and then Usage.Code_Used <= Usage.Code_Max
      and then Usage.Heap_Used <= Usage.Heap_Max
   ) with
      Global => null;

   ---------------------------------------------------------------------------
   --  Runtime Configuration Types
   ---------------------------------------------------------------------------

   --  Hardware crypto acceleration options
   type Crypto_Acceleration is record
      Hardware_SHA3    : Boolean;  --  SHA3/Keccak accelerator
      Hardware_AES     : Boolean;  --  AES accelerator
      Hardware_NTT     : Boolean;  --  Number-theoretic transform (ML-DSA/ML-KEM)
      Hardware_RNG     : Boolean;  --  True random number generator
   end record;

   No_Acceleration : constant Crypto_Acceleration := (
      Hardware_SHA3 => False,
      Hardware_AES  => False,
      Hardware_NTT  => False,
      Hardware_RNG  => False
   );

   Full_Acceleration : constant Crypto_Acceleration := (
      Hardware_SHA3 => True,
      Hardware_AES  => True,
      Hardware_NTT  => True,
      Hardware_RNG  => True
   );

   --  Memory management mode
   type Memory_Mode is (
      Static_Only,        --  No dynamic allocation
      Preallocated,       --  Preallocated pools only
      Limited_Heap,       --  Small heap allowed
      Full_Heap           --  Standard heap management
   );

   --  Communication configuration
   type Communication_Config is record
      Network_Stack    : Boolean;  --  Include TCP/IP stack
      Serial_Console   : Boolean;  --  Serial/UART communication
      I2C_Support      : Boolean;  --  I2C peripheral support
      SPI_Support      : Boolean;  --  SPI peripheral support
      CAN_Support      : Boolean;  --  CAN bus support
   end record;

   Serial_Only_Comm : constant Communication_Config := (
      Network_Stack  => False,
      Serial_Console => True,
      I2C_Support    => False,
      SPI_Support    => False,
      CAN_Support    => False
   );

   Full_Comm : constant Communication_Config := (
      Network_Stack  => True,
      Serial_Console => True,
      I2C_Support    => True,
      SPI_Support    => True,
      CAN_Support    => True
   );

   --  Complete runtime configuration
   type Runtime_Config is record
      Crypto    : Crypto_Acceleration;
      Memory    : Memory_Mode;
      Comm      : Communication_Config;
      Watchdog  : Boolean;           --  Enable watchdog timer
      Debug     : Boolean;           --  Debug symbols/assertions
   end record;

   --  Predefined runtime configurations
   Minimal_Runtime : constant Runtime_Config := (
      Crypto   => No_Acceleration,
      Memory   => Static_Only,
      Comm     => Serial_Only_Comm,
      Watchdog => True,
      Debug    => False
   );

   Standard_Runtime : constant Runtime_Config := (
      Crypto   => No_Acceleration,
      Memory   => Preallocated,
      Comm     => Full_Comm,
      Watchdog => True,
      Debug    => False
   );

   Accelerated_Runtime : constant Runtime_Config := (
      Crypto   => Full_Acceleration,
      Memory   => Preallocated,
      Comm     => Full_Comm,
      Watchdog => True,
      Debug    => False
   );

   Development_Runtime : constant Runtime_Config := (
      Crypto   => No_Acceleration,
      Memory   => Full_Heap,
      Comm     => Full_Comm,
      Watchdog => False,
      Debug    => True
   );

   ---------------------------------------------------------------------------
   --  Device Descriptor Types
   ---------------------------------------------------------------------------

   --  Device capability flags
   type Device_Capabilities is record
      Has_Display     : Boolean;
      Has_Networking  : Boolean;
      Has_Sensors     : Boolean;
      Has_Actuators   : Boolean;
      Has_GPS         : Boolean;
      Has_Battery     : Boolean;
      Is_Mobile       : Boolean;
      Is_Safety_Critical : Boolean;
   end record;

   --  Device classification
   type Device_Class is (
      Class_Server,         --  Full-featured server/desktop
      Class_Gateway,        --  IoT gateway/hub
      Class_Controller,     --  Industrial controller
      Class_Sensor_Node,    --  Sensor/monitoring device
      Class_Actuator,       --  Motor/actuator controller
      Class_Drone,          --  Unmanned aerial vehicle
      Class_Wearable,       --  Wearable device
      Class_Vehicle         --  Automotive/vehicle system
   );

   --  Complete device descriptor
   type Device_Descriptor is record
      Class        : Device_Class;
      Target       : Target_Info;
      Memory       : Memory_Profile;
      Runtime      : Runtime_Config;
      Capabilities : Device_Capabilities;
   end record;

   --  Example device configurations
   IoT_Gateway : constant Device_Descriptor := (
      Class        => Class_Gateway,
      Target       => Target_ARM64,
      Memory       => Profile_Standard,
      Runtime      => Standard_Runtime,
      Capabilities => (
         Has_Display        => False,
         Has_Networking     => True,
         Has_Sensors        => False,
         Has_Actuators      => False,
         Has_GPS            => False,
         Has_Battery        => False,
         Is_Mobile          => False,
         Is_Safety_Critical => False
      )
   );

   Drone_Controller : constant Device_Descriptor := (
      Class        => Class_Drone,
      Target       => Target_Cortex_M4,
      Memory       => Profile_Constrained,
      Runtime      => Minimal_Runtime,
      Capabilities => (
         Has_Display        => False,
         Has_Networking     => True,
         Has_Sensors        => True,
         Has_Actuators      => True,
         Has_GPS            => True,
         Has_Battery        => True,
         Is_Mobile          => True,
         Is_Safety_Critical => True
      )
   );

   Sensor_Node : constant Device_Descriptor := (
      Class        => Class_Sensor_Node,
      Target       => Target_RISCV32,
      Memory       => Profile_Minimal,
      Runtime      => Minimal_Runtime,
      Capabilities => (
         Has_Display        => False,
         Has_Networking     => True,
         Has_Sensors        => True,
         Has_Actuators      => False,
         Has_GPS            => False,
         Has_Battery        => True,
         Is_Mobile          => False,
         Is_Safety_Critical => False
      )
   );

   ---------------------------------------------------------------------------
   --  Contract Deployment Types
   ---------------------------------------------------------------------------

   type Deploy_Error is (
      Deploy_OK,
      Deploy_Memory_Exceeded,
      Deploy_Code_Too_Large,
      Deploy_Unsupported_Feature,
      Deploy_Crypto_Unavailable,
      Deploy_Network_Required,
      Deploy_Hardware_Missing
   );

   type Deploy_Result is record
      Success : Boolean;
      Error   : Deploy_Error;
      Usage   : Memory_Usage;
   end record;

   --  Check if contract can be deployed to device
   function Can_Deploy (
      Contract_Stack : Natural;
      Contract_Data  : Natural;
      Contract_Code  : Natural;
      Device         : Device_Descriptor
   ) return Deploy_Result with
      Global => null;

   ---------------------------------------------------------------------------
   --  Power Management
   ---------------------------------------------------------------------------

   type Power_Mode is (
      Power_Full,        --  Full performance
      Power_Balanced,    --  Balance performance/power
      Power_Low,         --  Low power mode
      Power_Sleep,       --  Deep sleep (minimal activity)
      Power_Shutdown     --  Power off
   );

   type Power_Config is record
      Mode           : Power_Mode;
      Wake_On_Event  : Boolean;      --  Wake on external event
      Wake_On_Timer  : Boolean;      --  Wake on timer
      Timer_Interval : Natural;      --  Wake interval (seconds)
   end record;

   Default_Power : constant Power_Config := (
      Mode           => Power_Full,
      Wake_On_Event  => True,
      Wake_On_Timer  => False,
      Timer_Interval => 0
   );

   Low_Power : constant Power_Config := (
      Mode           => Power_Low,
      Wake_On_Event  => True,
      Wake_On_Timer  => True,
      Timer_Interval => 60
   );

   ---------------------------------------------------------------------------
   --  Over-the-Air (OTA) Update Types
   ---------------------------------------------------------------------------

   type OTA_State is (
      OTA_Idle,
      OTA_Downloading,
      OTA_Verifying,
      OTA_Installing,
      OTA_Complete,
      OTA_Failed
   );

   type OTA_Error is (
      OTA_OK,
      OTA_Network_Error,
      OTA_Signature_Invalid,
      OTA_Hash_Mismatch,
      OTA_Size_Exceeded,
      OTA_Rollback_Failed
   );

   type OTA_Status is record
      State         : OTA_State;
      Error         : OTA_Error;
      Progress      : Natural;      --  Percentage (0-100)
      Downloaded    : Natural;      --  Bytes downloaded
      Total_Size    : Natural;      --  Total bytes
   end record;

   Initial_OTA_Status : constant OTA_Status := (
      State      => OTA_Idle,
      Error      => OTA_OK,
      Progress   => 0,
      Downloaded => 0,
      Total_Size => 0
   );

   ---------------------------------------------------------------------------
   --  Watchdog Timer
   ---------------------------------------------------------------------------

   --  Watchdog configuration
   type Watchdog_Config is record
      Enabled       : Boolean;
      Timeout_Ms    : Natural;      --  Timeout in milliseconds
      Auto_Reset    : Boolean;      --  Reset on timeout vs halt
   end record;

   Default_Watchdog : constant Watchdog_Config := (
      Enabled    => True,
      Timeout_Ms => 5000,           --  5 second timeout
      Auto_Reset => True
   );

   --  Kick (pet) the watchdog to prevent timeout
   procedure Kick_Watchdog with
      Global => null;

end Khepri_Embedded;
