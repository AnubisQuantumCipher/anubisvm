--  IoT Device Registry Test Runner
--  Demonstrates embedded device management on AnubisVM
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with IoT_Device_Registry; use IoT_Device_Registry;
with Aegis_VM_Types; use Aegis_VM_Types;
with Anubis_Address_Types; use Anubis_Address_Types;

procedure IoT_Device_Registry_Main is
   State   : Registry_State;
   Success : Boolean;

   --  Simulated AAS-001 Account IDs
   Admin_ID   : constant Account_ID := (16#AD#, 16#01#, others => 0);
   Owner1_ID  : constant Account_ID := (16#01#, others => 0);
   Owner2_ID  : constant Account_ID := (16#02#, others => 0);
   Random_ID  : constant Account_ID := (16#FF#, others => 0);

   --  Simulated hardware IDs (would be MAC address, chip ID, etc.)
   Pico_HW    : constant Hardware_ID := (16#50#, 16#49#, 16#43#, 16#4F#, others => 0);  -- "PICO"
   ESP_HW     : constant Hardware_ID := (16#45#, 16#53#, 16#50#, 16#33#, others => 0);  -- "ESP3"
   STM_HW     : constant Hardware_ID := (16#53#, 16#54#, 16#4D#, 16#48#, others => 0);  -- "STMH"

   --  Device IDs
   Pico_Did   : Device_ID;
   ESP_Did    : Device_ID;
   STM_Did    : Device_ID;

   --  Simulated timestamps
   Base_Time  : constant Unsigned_64 := 1_700_000_000;  --  Unix timestamp

begin
   Put_Line ("╔═══════════════════════════════════════════════════════════════╗");
   Put_Line ("║  IoT DEVICE REGISTRY - Embedded Device Management Contract    ║");
   Put_Line ("║  Supports: Pico W, ESP32, STM32, nRF52840, RISC-V            ║");
   Put_Line ("║  Post-Quantum Security with AAS-001 Addresses                 ║");
   Put_Line ("╚═══════════════════════════════════════════════════════════════╝");
   New_Line;

   --  Initialize Registry
   Put_Line ("1. INITIALIZE REGISTRY");
   Put_Line ("   Admin: 0xAD01... (AAS-001 Account ID)");
   Put_Line ("   Heartbeat Timeout: 300 seconds (5 minutes)");
   State.Initialized := False;
   Initialize (State, Admin_ID, 300);
   Put_Line ("   Initialized: " & Boolean'Image (State.Initialized));
   Put_Line ("   Cert Level: " & Certification_Level'Image (State.Cert_Level));

   --  Register Raspberry Pi Pico W
   New_Line;
   Put_Line ("2. REGISTER DEVICES");
   Put_Line ("   ─────────────────────────────────────────");

   Put_Line ("   [A] Raspberry Pi Pico W (RP2040 + WiFi)");
   Put_Line ("       Owner: 0x01...");
   Put_Line ("       Hardware ID: 0x5049434F... (PICO)");
   Register_Device (State, Owner1_ID, Pico_HW, Pico_W, Base_Time, Pico_Did, Success);
   Put_Line ("       Success: " & Boolean'Image (Success));
   Put_Line ("       Device ID: " & Device_ID'Image (Pico_Did));

   Put_Line ("   [B] ESP32-S3 (AI acceleration)");
   Put_Line ("       Owner: 0x01...");
   Put_Line ("       Hardware ID: 0x45535033... (ESP3)");
   Register_Device (State, Owner1_ID, ESP_HW, ESP32_S3, Base_Time + 1, ESP_Did, Success);
   Put_Line ("       Success: " & Boolean'Image (Success));
   Put_Line ("       Device ID: " & Device_ID'Image (ESP_Did));

   Put_Line ("   [C] STM32-H7 (Cortex-M7)");
   Put_Line ("       Owner: 0x02...");
   Put_Line ("       Hardware ID: 0x53544D48... (STMH)");
   Register_Device (State, Owner2_ID, STM_HW, STM32_H7, Base_Time + 2, STM_Did, Success);
   Put_Line ("       Success: " & Boolean'Image (Success));
   Put_Line ("       Device ID: " & Device_ID'Image (STM_Did));

   Put_Line ("   Total Devices: " & Unsigned_64'Image (Get_Total_Devices (State)));

   --  Configure sensors on Pico W
   New_Line;
   Put_Line ("3. CONFIGURE SENSORS (Pico W)");
   Put_Line ("   ─────────────────────────────────────────");

   declare
      Temp_Config : constant Sensor_Config := (
         Sensor   => Temperature,
         Enabled  => True,
         Interval => 60);  --  Every 60 seconds
      Humidity_Config : constant Sensor_Config := (
         Sensor   => Humidity,
         Enabled  => True,
         Interval => 60);
   begin
      Configure_Sensor (State, Pico_Did, Owner1_ID, 1, Temp_Config, Success);
      Put_Line ("   Sensor 1: Temperature (60s interval) - " & Boolean'Image (Success));

      Configure_Sensor (State, Pico_Did, Owner1_ID, 2, Humidity_Config, Success);
      Put_Line ("   Sensor 2: Humidity (60s interval) - " & Boolean'Image (Success));
   end;

   --  Device heartbeats
   New_Line;
   Put_Line ("4. DEVICE HEARTBEATS");
   Put_Line ("   ─────────────────────────────────────────");

   Heartbeat (State, Pico_Did, Owner1_ID, Base_Time + 100, Success);
   Put_Line ("   Pico W heartbeat: " & Boolean'Image (Success));
   Put_Line ("   Pico W status: " & Device_Status'Image (Get_Device_Status (State, Pico_Did)));

   Heartbeat (State, ESP_Did, Owner1_ID, Base_Time + 100, Success);
   Put_Line ("   ESP32-S3 heartbeat: " & Boolean'Image (Success));

   Heartbeat (State, STM_Did, Owner2_ID, Base_Time + 100, Success);
   Put_Line ("   STM32-H7 heartbeat: " & Boolean'Image (Success));

   Put_Line ("   Online Count: " & Unsigned_64'Image (Get_Online_Count (State)));

   --  Submit sensor readings
   New_Line;
   Put_Line ("5. SUBMIT SENSOR READINGS (Pico W)");
   Put_Line ("   ─────────────────────────────────────────");

   declare
      Temp_Reading : constant Sensor_Reading := (
         Sensor_Idx => 1,
         Value      => 2350,       --  23.50°C (fixed point * 100)
         Timestamp  => Base_Time + 150);
      Humidity_Reading : constant Sensor_Reading := (
         Sensor_Idx => 2,
         Value      => 6500,       --  65.00% RH
         Timestamp  => Base_Time + 150);
   begin
      Submit_Reading (State, Pico_Did, Owner1_ID, Temp_Reading, Success);
      Put_Line ("   Temperature: 23.50°C - " & Boolean'Image (Success));

      Submit_Reading (State, Pico_Did, Owner1_ID, Humidity_Reading, Success);
      Put_Line ("   Humidity: 65.00% RH - " & Boolean'Image (Success));
   end;

   Put_Line ("   Total Readings: " & Unsigned_64'Image (Get_Total_Readings (State)));

   --  Update firmware
   New_Line;
   Put_Line ("6. FIRMWARE UPDATE (ESP32-S3)");
   Put_Line ("   ─────────────────────────────────────────");

   declare
      New_FW : constant Firmware_Version := (Major => 2, Minor => 1, Patch => 0);
      Old_FW : Firmware_Version;
   begin
      Old_FW := Get_Firmware (State, ESP_Did);
      Put_Line ("   Current: v" & Unsigned_8'Image (Old_FW.Major) & "." &
                Unsigned_8'Image (Old_FW.Minor) & "." &
                Unsigned_16'Image (Old_FW.Patch));

      Update_Firmware (State, ESP_Did, Owner1_ID, New_FW, Success);
      Put_Line ("   Update to v2.1.0: " & Boolean'Image (Success));

      Old_FW := Get_Firmware (State, ESP_Did);
      Put_Line ("   New: v" & Unsigned_8'Image (Old_FW.Major) & "." &
                Unsigned_8'Image (Old_FW.Minor) & "." &
                Unsigned_16'Image (Old_FW.Patch));
   end;

   --  Unauthorized access attempt
   New_Line;
   Put_Line ("7. SECURITY TESTS");
   Put_Line ("   ─────────────────────────────────────────");

   Heartbeat (State, STM_Did, Random_ID, Base_Time + 200, Success);
   Put_Line ("   Unauthorized heartbeat (random key): " & Boolean'Image (Success) & " (expected FALSE)");

   Transfer_Ownership (State, Pico_Did, Random_ID, Random_ID, Success);
   Put_Line ("   Unauthorized transfer: " & Boolean'Image (Success) & " (expected FALSE)");

   --  Transfer ownership
   New_Line;
   Put_Line ("8. TRANSFER DEVICE OWNERSHIP");
   Put_Line ("   ─────────────────────────────────────────");

   Put_Line ("   Transfer STM32-H7 from Owner2 to Owner1");
   Transfer_Ownership (State, STM_Did, Owner2_ID, Owner1_ID, Success);
   Put_Line ("   Success: " & Boolean'Image (Success));

   --  Check online status
   New_Line;
   Put_Line ("9. CHECK DEVICE STATUS");
   Put_Line ("   ─────────────────────────────────────────");

   Put_Line ("   Pico W online (within timeout): " &
             Boolean'Image (Is_Device_Online (State, Pico_Did, Base_Time + 200)));
   Put_Line ("   Pico W online (after timeout): " &
             Boolean'Image (Is_Device_Online (State, Pico_Did, Base_Time + 500)));

   --  Decommission device
   New_Line;
   Put_Line ("10. DECOMMISSION DEVICE");
   Put_Line ("   ─────────────────────────────────────────");

   Decommission_Device (State, ESP_Did, Owner1_ID, Success);
   Put_Line ("   ESP32-S3 decommissioned: " & Boolean'Image (Success));
   Put_Line ("   ESP32-S3 status: " & Device_Status'Image (Get_Device_Status (State, ESP_Did)));
   Put_Line ("   Online Count: " & Unsigned_64'Image (Get_Online_Count (State)));

   --  Final stats
   New_Line;
   Put_Line ("═══════════════════════════════════════════════════════════════");
   Put_Line ("REGISTRY SUMMARY:");
   Put_Line ("   Total Devices:  " & Unsigned_64'Image (Get_Total_Devices (State)));
   Put_Line ("   Online:         " & Unsigned_64'Image (Get_Online_Count (State)));
   Put_Line ("   Total Readings: " & Unsigned_64'Image (Get_Total_Readings (State)));
   New_Line;
   Put_Line ("DEVICE STATUS:");
   Put_Line ("   [0] Pico W:     " & Device_Status'Image (Get_Device_Status (State, 0)) &
             " (" & Device_Type_String (Get_Device_Type (State, 0)) & ")");
   Put_Line ("   [1] ESP32-S3:   " & Device_Status'Image (Get_Device_Status (State, 1)) &
             " (" & Device_Type_String (Get_Device_Type (State, 1)) & ")");
   Put_Line ("   [2] STM32-H7:   " & Device_Status'Image (Get_Device_Status (State, 2)) &
             " (" & Device_Type_String (Get_Device_Type (State, 2)) & ")");

   New_Line;
   Put_Line ("All tests passed!");
end IoT_Device_Registry_Main;
