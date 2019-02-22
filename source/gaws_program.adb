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

with Command_Line;
with Setup;
with Options;
with Exceptions;
with Program;
with Host_Lists;

procedure GAWS_Program is

   procedure Put_Version;
   --  Put help text to terminal.

   procedure Put_Hints;
   --  Put hinst text to terminal.

   procedure Put_Version
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
   end Put_Version;

   procedure Put_Hints
   is
      use Ada.Text_IO, Setup;
   begin
      Put (Setup.Get_Program_Name);
      Put (": Try http://gaws.org:8088");
      New_Line;

      Put (Setup.Get_Program_Name);
      Put (": Stop server with Control-C");
      New_Line;
   end Put_Hints;

      use Ada.Command_Line;

   Success : Boolean;
begin

   Command_Line.Parse (Success);

   if not Options.Be_Quiet then
      Put_Version;
   end if;

   if not Success then
      Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   if  Options.Show_Version then
      Put_Version;
      return;
   end if;

   if not Options.Be_Quiet then
      Put_Hints;
   end if;

   Host_Lists.Register_Hosts (Hosts_File => Options.Host_List_File.all);

   Program.Run;

exception

   when Occurrence : others =>
      Exceptions.Put (Occurrence);

end GAWS_Program;
