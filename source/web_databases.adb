--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Directories;

package body Web_Databases is


   type R_Respository is
      record
         null;
      end record;


   function Dir_Exists (Host : in T_Host_Name) return Boolean
   is
      use Ada.Directories;
   begin
      return Exists (Host);
   end Dir_Exists;


   procedure Register (Host : in T_Host_Name)
   is
      use Ada.Text_IO;
   begin
      if not Dir_Exists (Host) then
         raise Constraint_Error
           with "Web directory '" & Host & "'does not exist.";
      end if;
      Put_Line ("Register: Stub.");
   end Register;


   function Create_Respository
     (Host : in T_Host_Name) return T_Respository
   is
      use Ada.Directories;
   begin
      if not Ada.Directories.Exists (Host) then
         raise Constraint_Error
           with "Web directory '" & Host & "' does not exist.";
      end if;
      return new R_Respository;
   end Create_Respository;


   procedure Put (Respository : in T_Respository;
                  Host        : in String;
                  URI         : in String)
   is
      pragma Unreferenced (Respository);
      use Ada.Text_IO;
   begin
      Put ("Web_Databases.Put: ");
      Put ("Host: ");
      Put (Host);
      Put ("    ");
      Put ("(URI: ");
      Put (URI);
      New_Line;
   end Put;

end Web_Databases;
