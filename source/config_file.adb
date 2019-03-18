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
--  with Ada.Exceptions;

with DK8543.Strings.Comments;
--  with DK8543.Errors;

with Options;
with GAWS_Log;
with Virtual_Hosts;

package body Config_File is


   function Get_Host
     (From_File : in out Ada.Text_IO.File_Type)
     return String;


   function Get_Host
     (From_File : in out Ada.Text_IO.File_Type)
     return String
   is
      use DK8543.Strings.Comments;

      Line_Raw  : constant String := Ada.Text_IO.Get_Line (From_File);
   begin
      return Trim_Comments (Line_Raw);
   end Get_Host;



   procedure Register_Hosts (Success : out Boolean)
   is
      use Ada.Text_IO;
      Hosts_File  : constant String  := Options.Host_List_File.all;
      Exists      : constant Boolean := Ada.Directories.Exists (Hosts_File);
      File        : File_Type;
      All_Success : Boolean := True;
      Line_Number : Natural := 0;
   begin
      Success := False;

      GAWS_Log.Put_Line ("Registering hosts");
      GAWS_Log.Put_Horizontal_Line;

      if not Exists then
         GAWS_Log.Put_Line ("ERROR: Hosts file '" & Hosts_File & "' not found.");
         return;
      end if;

      Open (File, In_File, Hosts_File);
      loop
         declare
            Host_Name      : constant String := Get_Host (File);
            Append_Success : Boolean;
         begin
            Line_Number := Line_Number + 1;

            if Host_Name /= "" then
               Virtual_Hosts.Append_Respository (Host_Name, Append_Success);

               if Append_Success then
                  GAWS_Log.Put_Line ("Registered '" & Host_Name & "'.");
               else
                  All_Success := False;
                  GAWS_Log.Put_Line ("ERROR: Registering '" & Host_Name & "' failed.");
               end if;

            end if;

--         exception
--
--              when Occ : Constraint_Error =>
--                 raise;
--             DK8543.Errors.Error
--                 (Hosts_File, Line_Number,
--                  Ada.Exceptions.Exception_Message (Occ));
         end;
      end loop;

   exception

      when End_Error =>
         Close (File);
         GAWS_Log.Put_Horizontal_Line;
         Success := All_Success;

   end Register_Hosts;


end Config_File;
