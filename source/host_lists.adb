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
with Ada.Exceptions;

with DK8543.Text.Comments;
with DK8543.Errors;

with Setup;
with Respositories;
--  with Web_Server;

package body Host_Lists is


   function Get_Host
     (From_File : in out Ada.Text_IO.File_Type) return String;


   function Get_Host (From_File : in out Ada.Text_IO.File_Type)
                     return String
   is
      use DK8543.Text.Comments;
      Line_Raw  : constant String := Ada.Text_IO.Get_Line (From_File);
   begin
      return Trim_Comments (Line_Raw);
   end Get_Host;



   procedure Register_Hosts (Hosts_File : in String)
   is
      use Ada.Text_IO;
--      use Ada.Strings.Unbounded;
      Exists     : constant Boolean := Ada.Directories.Exists (Hosts_File);
      File       : File_Type;
      Line_Number : Natural := 0;
   begin
      if not Exists then
         raise Constraint_Error
           with "Hosts file " & Hosts_File & " does not exist.";
      end if;

      Open (File, In_File, Hosts_File);
      loop
         declare
            Host_Name : constant String := Get_Host (File);
         begin
            Line_Number := Line_Number + 1;
            if Host_Name /= "" then
               Respositories.Append_Respository (Host_Name);
               Put (Setup.Get_Program_Name);
               Put (": Respository for '");
               Put (Host_Name);
               Put ("' created.");
               New_Line;
            end if;

         exception

            when Occ : Constraint_Error =>
               DK8543.Errors.Error
                 (Hosts_File, Line_Number,
                  Ada.Exceptions.Exception_Message (Occ));
         end;
      end loop;

   exception

      when End_Error =>
         Close (File);

   end Register_Hosts;


end Host_Lists;
