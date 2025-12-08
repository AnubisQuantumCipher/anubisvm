--  KHEPRI Hardware Drivers
--
--  Hardware Abstraction Layer (HAL) for embedded KHEPRI deployments.
--  Provides interfaces for cryptographic accelerators, storage,
--  communication peripherals, and sensors.
--
--  This package defines abstract interfaces that must be implemented
--  for each target platform. Default software implementations are
--  provided for platforms without hardware acceleration.
--
--  Supported Peripherals:
--  - Cryptographic: SHA3, AES, RNG, NTT (for ML-DSA/ML-KEM)
--  - Storage: Flash, EEPROM, SD Card
--  - Communication: UART, SPI, I2C, CAN, Ethernet
--  - Sensors: GPS, IMU, Temperature
--  - Actuators: PWM, GPIO
--
--  Certification Target: GOLD

pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package Khepri_Drivers with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Basic Types
   ---------------------------------------------------------------------------

   type Byte is new Unsigned_8;
   type Byte_Array is array (Natural range <>) of Byte;

   --  Driver status
   type Driver_Status is (
      Status_OK,
      Status_Error,
      Status_Busy,
      Status_Timeout,
      Status_Not_Initialized,
      Status_Not_Supported
   );

   --  Driver result with status
   type Driver_Result is record
      Status       : Driver_Status;
      Bytes_Done   : Natural;
   end record;

   Success_Result : constant Driver_Result := (
      Status     => Status_OK,
      Bytes_Done => 0
   );

   ---------------------------------------------------------------------------
   --  Cryptographic Hardware Interface
   ---------------------------------------------------------------------------

   --  SHA3 hardware accelerator
   type SHA3_Accelerator is record
      Initialized : Boolean;
      Busy        : Boolean;
   end record;

   procedure SHA3_Init (Acc : out SHA3_Accelerator) with
      Global => null,
      Post   => Acc.Initialized and not Acc.Busy;

   procedure SHA3_256_Hash (
      Acc    : in out SHA3_Accelerator;
      Input  : in     Byte_Array;
      Output : out    Byte_Array;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Acc.Initialized and Output'Length = 32;

   procedure SHA3_512_Hash (
      Acc    : in out SHA3_Accelerator;
      Input  : in     Byte_Array;
      Output : out    Byte_Array;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Acc.Initialized and Output'Length = 64;

   --  AES hardware accelerator
   type AES_Accelerator is record
      Initialized : Boolean;
      Key_Loaded  : Boolean;
   end record;

   procedure AES_Init (Acc : out AES_Accelerator) with
      Global => null;

   procedure AES_Load_Key (
      Acc    : in out AES_Accelerator;
      Key    : in     Byte_Array;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Key'Length in 16 | 24 | 32;

   procedure AES_Encrypt_Block (
      Acc      : in Out AES_Accelerator;
      Input    : in     Byte_Array;
      Output   : out    Byte_Array;
      Result   : out    Driver_Result
   ) with
      Global => null,
      Pre    => Acc.Key_Loaded and Input'Length = 16 and Output'Length = 16;

   procedure AES_Decrypt_Block (
      Acc      : in Out AES_Accelerator;
      Input    : in     Byte_Array;
      Output   : out    Byte_Array;
      Result   : out    Driver_Result
   ) with
      Global => null,
      Pre    => Acc.Key_Loaded and Input'Length = 16 and Output'Length = 16;

   --  Hardware Random Number Generator
   type RNG_Accelerator is record
      Initialized : Boolean;
      Entropy_Ready : Boolean;
   end record;

   procedure RNG_Init (Acc : out RNG_Accelerator) with
      Global => null;

   procedure RNG_Get_Bytes (
      Acc    : in Out RNG_Accelerator;
      Output : out    Byte_Array;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Acc.Initialized;

   function RNG_Is_Ready (Acc : RNG_Accelerator) return Boolean is
      (Acc.Initialized and Acc.Entropy_Ready) with
      Global => null;

   --  NTT accelerator (for ML-DSA/ML-KEM)
   type NTT_Accelerator is record
      Initialized : Boolean;
   end record;

   procedure NTT_Init (Acc : out NTT_Accelerator) with
      Global => null;

   procedure NTT_Forward (
      Acc    : in Out NTT_Accelerator;
      Data   : in Out Byte_Array;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Acc.Initialized;

   procedure NTT_Inverse (
      Acc    : in Out NTT_Accelerator;
      Data   : in Out Byte_Array;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Acc.Initialized;

   ---------------------------------------------------------------------------
   --  Storage Interface
   ---------------------------------------------------------------------------

   --  Flash memory driver
   type Flash_Driver is record
      Initialized   : Boolean;
      Total_Size    : Natural;      --  Total flash size in bytes
      Sector_Size   : Natural;      --  Sector size for erase
      Write_Size    : Natural;      --  Minimum write size
   end record;

   procedure Flash_Init (
      Drv    : out Flash_Driver;
      Result : out Driver_Result
   ) with
      Global => null;

   procedure Flash_Read (
      Drv    : in     Flash_Driver;
      Addr   : in     Natural;
      Data   : out    Byte_Array;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized and Addr + Data'Length <= Drv.Total_Size;

   procedure Flash_Write (
      Drv    : in Out Flash_Driver;
      Addr   : in     Natural;
      Data   : in     Byte_Array;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized and Addr + Data'Length <= Drv.Total_Size;

   procedure Flash_Erase_Sector (
      Drv    : in Out Flash_Driver;
      Sector : in     Natural;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized;

   --  EEPROM driver
   type EEPROM_Driver is record
      Initialized : Boolean;
      Size        : Natural;
   end record;

   procedure EEPROM_Init (
      Drv    : out EEPROM_Driver;
      Result : out Driver_Result
   ) with
      Global => null;

   procedure EEPROM_Read (
      Drv    : in     EEPROM_Driver;
      Addr   : in     Natural;
      Data   : out    Byte_Array;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized;

   procedure EEPROM_Write (
      Drv    : in Out EEPROM_Driver;
      Addr   : in     Natural;
      Data   : in     Byte_Array;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized;

   ---------------------------------------------------------------------------
   --  Communication Interface
   ---------------------------------------------------------------------------

   --  UART driver
   type UART_Config is record
      Baud_Rate   : Natural;
      Data_Bits   : Natural;
      Stop_Bits   : Natural;
      Parity      : Natural;  --  0=none, 1=odd, 2=even
   end record;

   Default_UART_Config : constant UART_Config := (
      Baud_Rate => 115200,
      Data_Bits => 8,
      Stop_Bits => 1,
      Parity    => 0
   );

   type UART_Driver is record
      Initialized : Boolean;
      Config      : UART_Config;
      RX_Ready    : Boolean;
      TX_Ready    : Boolean;
   end record;

   procedure UART_Init (
      Drv    : out UART_Driver;
      Config : in  UART_Config;
      Result : out Driver_Result
   ) with
      Global => null;

   procedure UART_Send (
      Drv    : in Out UART_Driver;
      Data   : in     Byte_Array;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized;

   procedure UART_Receive (
      Drv    : in Out UART_Driver;
      Data   : out    Byte_Array;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized;

   function UART_Data_Available (Drv : UART_Driver) return Boolean is
      (Drv.Initialized and Drv.RX_Ready) with
      Global => null;

   --  SPI driver
   type SPI_Mode is (Mode_0, Mode_1, Mode_2, Mode_3);

   type SPI_Config is record
      Clock_Speed : Natural;      --  Hz
      Mode        : SPI_Mode;
      MSB_First   : Boolean;
   end record;

   type SPI_Driver is record
      Initialized : Boolean;
      Config      : SPI_Config;
   end record;

   procedure SPI_Init (
      Drv    : out SPI_Driver;
      Config : in  SPI_Config;
      Result : out Driver_Result
   ) with
      Global => null;

   procedure SPI_Transfer (
      Drv      : in Out SPI_Driver;
      TX_Data  : in     Byte_Array;
      RX_Data  : out    Byte_Array;
      Result   : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized and TX_Data'Length = RX_Data'Length;

   --  I2C driver
   type I2C_Config is record
      Clock_Speed : Natural;      --  Hz (100000 or 400000 typical)
   end record;

   type I2C_Driver is record
      Initialized : Boolean;
      Config      : I2C_Config;
   end record;

   procedure I2C_Init (
      Drv    : out I2C_Driver;
      Config : in  I2C_Config;
      Result : out Driver_Result
   ) with
      Global => null;

   procedure I2C_Write (
      Drv     : in Out I2C_Driver;
      Address : in     Byte;
      Data    : in     Byte_Array;
      Result  : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized;

   procedure I2C_Read (
      Drv     : in Out I2C_Driver;
      Address : in     Byte;
      Data    : out    Byte_Array;
      Result  : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized;

   --  CAN bus driver
   type CAN_Frame is record
      ID        : Unsigned_32;
      Extended  : Boolean;
      RTR       : Boolean;
      DLC       : Natural;
      Data      : Byte_Array (0 .. 7);
   end record;

   type CAN_Driver is record
      Initialized : Boolean;
      Bitrate     : Natural;
   end record;

   procedure CAN_Init (
      Drv     : out CAN_Driver;
      Bitrate : in  Natural;
      Result  : out Driver_Result
   ) with
      Global => null;

   procedure CAN_Send (
      Drv    : in Out CAN_Driver;
      Frame  : in     CAN_Frame;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized;

   procedure CAN_Receive (
      Drv    : in Out CAN_Driver;
      Frame  : out    CAN_Frame;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized;

   ---------------------------------------------------------------------------
   --  Sensor Interface
   ---------------------------------------------------------------------------

   --  GPS data
   type GPS_Fix is (No_Fix, Fix_2D, Fix_3D);

   type GPS_Data is record
      Valid      : Boolean;
      Fix        : GPS_Fix;
      Latitude   : Integer;      --  Degrees * 10^7
      Longitude  : Integer;      --  Degrees * 10^7
      Altitude   : Integer;      --  Meters
      Speed      : Natural;      --  cm/s
      Heading    : Natural;      --  Degrees * 100
      Satellites : Natural;
      Timestamp  : Unsigned_64;
   end record;

   type GPS_Driver is record
      Initialized : Boolean;
      Last_Data   : GPS_Data;
   end record;

   procedure GPS_Init (
      Drv    : out GPS_Driver;
      Result : out Driver_Result
   ) with
      Global => null;

   procedure GPS_Update (
      Drv    : in Out GPS_Driver;
      Data   : out    GPS_Data;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized;

   --  IMU (Inertial Measurement Unit) data
   type IMU_Data is record
      Valid       : Boolean;
      Accel_X     : Integer;      --  mg (milli-g)
      Accel_Y     : Integer;
      Accel_Z     : Integer;
      Gyro_X      : Integer;      --  mdps (milli-degrees per second)
      Gyro_Y      : Integer;
      Gyro_Z      : Integer;
      Mag_X       : Integer;      --  mGauss
      Mag_Y       : Integer;
      Mag_Z       : Integer;
      Temperature : Integer;      --  milli-Celsius
   end record;

   type IMU_Driver is record
      Initialized : Boolean;
   end record;

   procedure IMU_Init (
      Drv    : out IMU_Driver;
      Result : out Driver_Result
   ) with
      Global => null;

   procedure IMU_Read (
      Drv    : in Out IMU_Driver;
      Data   : out    IMU_Data;
      Result : out    Driver_Result
   ) with
      Global => null,
      Pre    => Drv.Initialized;

   ---------------------------------------------------------------------------
   --  GPIO Interface
   ---------------------------------------------------------------------------

   type GPIO_Mode is (Mode_Input, Mode_Output, Mode_Analog, Mode_Alternate);
   type GPIO_Pull is (Pull_None, Pull_Up, Pull_Down);
   type GPIO_Speed is (Speed_Low, Speed_Medium, Speed_High, Speed_Very_High);

   type GPIO_Config is record
      Mode  : GPIO_Mode;
      Pull  : GPIO_Pull;
      Speed : GPIO_Speed;
   end record;

   type GPIO_Pin is record
      Port        : Natural;
      Pin         : Natural;
      Initialized : Boolean;
      Config      : GPIO_Config;
   end record;

   procedure GPIO_Init (
      Pin    : out GPIO_Pin;
      Port   : in  Natural;
      Number : in  Natural;
      Config : in  GPIO_Config;
      Result : out Driver_Result
   ) with
      Global => null;

   procedure GPIO_Write (
      Pin   : in GPIO_Pin;
      Value : in Boolean
   ) with
      Global => null,
      Pre    => Pin.Initialized and Pin.Config.Mode = Mode_Output;

   function GPIO_Read (Pin : GPIO_Pin) return Boolean with
      Global => null,
      Pre    => Pin.Initialized and Pin.Config.Mode = Mode_Input;

   procedure GPIO_Toggle (Pin : in GPIO_Pin) with
      Global => null,
      Pre    => Pin.Initialized and Pin.Config.Mode = Mode_Output;

   ---------------------------------------------------------------------------
   --  PWM Interface
   ---------------------------------------------------------------------------

   type PWM_Channel is record
      Initialized : Boolean;
      Timer       : Natural;
      Channel     : Natural;
      Frequency   : Natural;      --  Hz
      Duty_Cycle  : Natural;      --  0-10000 (0.00% - 100.00%)
   end record;

   procedure PWM_Init (
      Ch        : out PWM_Channel;
      Timer     : in  Natural;
      Channel   : in  Natural;
      Frequency : in  Natural;
      Result    : out Driver_Result
   ) with
      Global => null;

   procedure PWM_Set_Duty (
      Ch         : in Out PWM_Channel;
      Duty_Cycle : in     Natural
   ) with
      Global => null,
      Pre    => Ch.Initialized and Duty_Cycle <= 10000;

   procedure PWM_Start (Ch : in Out PWM_Channel) with
      Global => null,
      Pre    => Ch.Initialized;

   procedure PWM_Stop (Ch : in Out PWM_Channel) with
      Global => null,
      Pre    => Ch.Initialized;

   ---------------------------------------------------------------------------
   --  Watchdog Interface
   ---------------------------------------------------------------------------

   type Watchdog_Driver is record
      Initialized : Boolean;
      Timeout_Ms  : Natural;
   end record;

   procedure Watchdog_Init (
      Drv        : out Watchdog_Driver;
      Timeout_Ms : in  Natural;
      Result     : out Driver_Result
   ) with
      Global => null;

   procedure Watchdog_Kick (Drv : in Watchdog_Driver) with
      Global => null,
      Pre    => Drv.Initialized;

   procedure Watchdog_Start (Drv : in Out Watchdog_Driver) with
      Global => null,
      Pre    => Drv.Initialized;

   ---------------------------------------------------------------------------
   --  System Time Interface
   ---------------------------------------------------------------------------

   --  Get system time in milliseconds since boot
   function Get_Tick_Ms return Unsigned_64 with
      Global => null;

   --  Get system time in microseconds
   function Get_Tick_Us return Unsigned_64 with
      Global => null;

   --  Delay for specified milliseconds
   procedure Delay_Ms (Ms : Natural) with
      Global => null;

   --  Delay for specified microseconds
   procedure Delay_Us (Us : Natural) with
      Global => null;

end Khepri_Drivers;
