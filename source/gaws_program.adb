--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Directories;

with Command_Line;
with Setup;
with Options;
with Exceptions;
with Program;
with Host_Lists;

procedure GAWS_Program is

   procedure Show_Help;
   --  Put help text to terminal.

   procedure Show_Help
   is
      use Ada.Text_IO, Setup;
      Version : constant String :=
        Get_Program_Name & " (" & Get_Program_Version & ")";
      Build : constant String :=
        "Build (" & Get_Build_ISO8601_UTC & ")";
   begin
      Put_Line (Version);
      Put_Line (Build);
      New_Line;
      Put_Line ("The author disclaims copyright to this source code.  In place of");
      Put_Line ("a legal notice, here is a blessing:");
      New_Line;
      Put_Line ("   May you do good and not evil.");
      Put_Line ("   May you find forgiveness for yourself and forgive others.");
      Put_Line ("   May you share freely, not taking more than you give.");
      New_Line;
   end Show_Help;

   use Ada.Command_Line;

   Success : Boolean;
begin

   Command_Line.Parse (Success);

   if not Success then
      Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   if  Options.Show_Version then
      Show_Help;
      return;
   end if;

   declare
      use Host_Lists;
      use Ada.Text_IO;
      Exists : constant Boolean := Ada.Directories.Exists (Options.Host_List_File.all);
   begin
      Put ("Hosts file exists: ");
      Put (Boolean'Image (Exists));
      New_Line;
      if Exists then
         declare
            Count : constant Natural := Hosts_Count;
         begin
            Put ("Hosts count: ");
            Put (Natural'Image (Count));
            New_Line;
            for Index in 1 .. Count loop
               Put (Positive'Image (Index));
               Put (": ");
               Put (Get_Host (Index));
               New_Line;
            end loop;
         end;
      end if;
   end;
   Program.Run;

exception

   when Occurrence : others =>
      Exceptions.Put (Occurrence);

end GAWS_Program;
