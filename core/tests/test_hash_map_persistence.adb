--  Test Hash Map Persistence
--
--  Verifies that the Khepri MPT hash map is correctly persisted and restored,
--  ensuring data survives restarts.

pragma SPARK_Mode (Off);

with Ada.Text_IO;
with Ada.Directories;
with Interfaces; use Interfaces;
with Aegis_VM_Types; use Aegis_VM_Types;
with Khepri_MPT; use Khepri_MPT;
with Khepri_MPT_Types; use Khepri_MPT_Types;

procedure Test_Hash_Map_Persistence is

   procedure Assert (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL: " & Message);
         raise Program_Error with Message;
      end if;
      Ada.Text_IO.Put_Line ("PASS: " & Message);
   end Assert;

   Test_Dir : constant String := "/tmp/anubis_test_hashmap";
   Hash_Map_File : constant String := Test_Dir & "/test.hashmap";

   Trie1, Trie2 : Trie_ID;
   Success : Boolean;
   Error : MPT_Error;

   --  Test data
   Key1 : constant Byte_Array := (16#01#, 16#02#, 16#03#, 16#04#);
   Val1 : constant Byte_Array := (16#AA#, 16#BB#, 16#CC#, 16#DD#);

   Key2 : constant Byte_Array := (16#05#, 16#06#, 16#07#, 16#08#);
   Val2 : constant Byte_Array := (16#11#, 16#22#, 16#33#, 16#44#);

   Key3 : constant Byte_Array := (16#09#, 16#0A#, 16#0B#, 16#0C#);
   Val3 : constant Byte_Array := (16#EE#, 16#FF#, 16#00#, 16#11#);

   Root1, Root2 : Hash_256;
   Value : Value_Data;
   Found : Boolean;

begin
   Ada.Text_IO.Put_Line ("=== Hash Map Persistence Test ===");
   Ada.Text_IO.New_Line;

   --  Clean up test directory
   if Ada.Directories.Exists (Test_Dir) then
      Ada.Directories.Delete_Tree (Test_Dir);
   end if;
   Ada.Directories.Create_Directory (Test_Dir);

   --  Phase 1: Create trie and insert data
   Ada.Text_IO.Put_Line ("Phase 1: Creating trie and inserting data...");
   Create_Trie (Trie1, Success);
   Assert (Success, "Create initial trie");

   Put (Trie1, Key1, Val1, Success, Error);
   Assert (Success and Error = Error_None, "Insert key1");

   Put (Trie1, Key2, Val2, Success, Error);
   Assert (Success and Error = Error_None, "Insert key2");

   Put (Trie1, Key3, Val3, Success, Error);
   Assert (Success and Error = Error_None, "Insert key3");

   --  Verify data is accessible
   Get (Trie1, Key1, Value, Found, Error);
   Assert (Found and Error = Error_None, "Retrieve key1 before save");
   Assert (Value.Length = Val1'Length, "Value1 length correct");
   for I in 0 .. Val1'Length - 1 loop
      Assert (Value.Bytes (I) = Val1 (Val1'First + I),
              "Value1 byte" & Natural'Image (I) & " correct");
   end loop;

   Root1 := Root_Hash (Trie1);
   Assert (Root1 /= Empty_Hash, "Root hash is not empty");

   Ada.Text_IO.Put_Line ("Root hash before save: " &
      Unsigned_8'Image (Root1 (0)) &
      Unsigned_8'Image (Root1 (1)) & "...");

   --  Phase 2: Save hash map
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Phase 2: Saving hash map to disk...");
   Save_Hash_Map (Hash_Map_File, Success);
   Assert (Success, "Save hash map to " & Hash_Map_File);
   Assert (Ada.Directories.Exists (Hash_Map_File), "Hash map file exists");

   declare
      File_Size : constant Natural :=
         Natural (Ada.Directories.Size (Hash_Map_File));
   begin
      Ada.Text_IO.Put_Line ("Hash map file size:" & Natural'Image (File_Size) & " bytes");
      Assert (File_Size > 4, "Hash map file has reasonable size");
   end;

   --  Phase 3: Simulate restart by loading hash map
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Phase 3: Simulating restart - loading hash map...");

   --  Load hash map from disk
   Load_Hash_Map (Hash_Map_File, Success);
   Assert (Success, "Load hash map from " & Hash_Map_File);

   --  Try to load the trie using the saved root hash
   Load_Trie (Root1, Trie2, Success);
   Assert (Success, "Load trie from saved root hash");

   --  Phase 4: Verify data is still accessible
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Phase 4: Verifying data after reload...");

   Root2 := Root_Hash (Trie2);
   Assert (Root2 = Root1, "Root hash matches after reload");

   Ada.Text_IO.Put_Line ("Root hash after reload: " &
      Unsigned_8'Image (Root2 (0)) &
      Unsigned_8'Image (Root2 (1)) & "...");

   --  Verify all keys are still accessible
   Get (Trie2, Key1, Value, Found, Error);
   Assert (Found and Error = Error_None, "Retrieve key1 after reload");
   Assert (Value.Length = Val1'Length, "Value1 length correct after reload");
   for I in 0 .. Val1'Length - 1 loop
      Assert (Value.Bytes (I) = Val1 (Val1'First + I),
              "Value1 byte" & Natural'Image (I) & " correct after reload");
   end loop;

   Get (Trie2, Key2, Value, Found, Error);
   Assert (Found and Error = Error_None, "Retrieve key2 after reload");
   Assert (Value.Length = Val2'Length, "Value2 length correct after reload");

   Get (Trie2, Key3, Value, Found, Error);
   Assert (Found and Error = Error_None, "Retrieve key3 after reload");
   Assert (Value.Length = Val3'Length, "Value3 length correct after reload");

   --  Phase 5: Test rebuild from Node_Store
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Phase 5: Testing Rebuild_Hash_Map...");

   --  Delete hash map file
   if Ada.Directories.Exists (Hash_Map_File) then
      Ada.Directories.Delete_File (Hash_Map_File);
   end if;

   --  Rebuild hash map from Node_Store
   Rebuild_Hash_Map (Success);
   Assert (Success, "Rebuild hash map from Node_Store");

   --  Verify data is still accessible after rebuild
   declare
      Trie3 : Trie_ID;
   begin
      Load_Trie (Root1, Trie3, Success);
      Assert (Success, "Load trie after rebuild");

      Get (Trie3, Key1, Value, Found, Error);
      Assert (Found and Error = Error_None, "Retrieve key1 after rebuild");

      Get (Trie3, Key2, Value, Found, Error);
      Assert (Found and Error = Error_None, "Retrieve key2 after rebuild");

      Get (Trie3, Key3, Value, Found, Error);
      Assert (Found and Error = Error_None, "Retrieve key3 after rebuild");
   end;

   --  Clean up
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Cleaning up...");
   Destroy_Trie (Trie1);
   Destroy_Trie (Trie2);

   if Ada.Directories.Exists (Test_Dir) then
      Ada.Directories.Delete_Tree (Test_Dir);
   end if;

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("=== ALL TESTS PASSED ===");
   Ada.Text_IO.Put_Line ("Hash map persistence is working correctly!");
   Ada.Text_IO.Put_Line ("Data survives restarts as expected.");

exception
   when E : others =>
      Ada.Text_IO.Put_Line ("FATAL ERROR: " & Ada.Exceptions.Exception_Information (E));
      if Ada.Directories.Exists (Test_Dir) then
         Ada.Directories.Delete_Tree (Test_Dir);
      end if;
      raise;
end Test_Hash_Map_Persistence;
