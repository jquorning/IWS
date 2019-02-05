--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

with GNAT.Command_Line;

with Options;
with Setup;

package body Command_Line is

   use Options;

   procedure Getopt_Callback
     (Switch  : String;  Param : String;  Section : String);
   --  To be installed and called by Getopt on command line switch encounter.


   Long_Switch_Show_Help     : constant String := "--help";
   Long_Switch_Show_Version  : constant String := "--version";
   Long_Switch_Web_Directory : constant String := "--directory";
   Long_Switch_TCP_IP_Port   : constant String := "--port";
   Long_Switch_Host_Name     : constant String := "--hostname";
   Long_Switch_Database_File : constant String := "--database";

   use GNAT.Command_Line;
   Config     : Command_Line_Configuration;


   procedure Getopt_Callback (Switch, Param, Section : String) is
   begin
      Ada.Text_IO.Put_Line ("Switch : " & Switch);
      Ada.Text_IO.Put_Line ("Param  : " & Param);
      Ada.Text_IO.Put_Line ("Section: " & Section);
   end Getopt_Callback;


   procedure Process_Command_Line (Result : out Process_Result) is
   begin
      Define_Switch (Config, Option_Show_Help'Access, "-h",
                     Help => "Show this help text.",
                     Long_Switch => Long_Switch_Show_Help);
      Define_Switch (Config, Option_Show_Version'Access, "-x",
                     Help => "Show program name and version.");
      Define_Switch (Config, Option_Show_Version'Access, "-v",
                     Help        => "Show program name and version.",
                     Long_Switch => Long_Switch_Show_Version);
      Define_Switch (Config, Option_Web_Directory'Access, "-d=",
                     Help        => "Static web directory.",
                     Long_Switch => Long_Switch_Web_Directory);
      Define_Switch (Config, Option_TCP_IP_Port'Access, "-p=",
                     Help        => "Web server listen port.",
                     Long_Switch => Long_Switch_TCP_IP_Port);
      Define_Switch (Config, Option_Host_Name'Access, "-h=",
                     Help        => "Host name to serve.",
                     Long_Switch => Long_Switch_Host_Name);
      Define_Switch (Config, Option_Database_File'Access, "-b=",
                     Help        => "SQLite3 Database file.",
                     Long_Switch => Long_Switch_Database_File);

      --  Do the whole parsing business.
      --  Actually just the -D= option and file name.
      Getopt (Config, Getopt_Callback'Access);

      if Option_Show_Help then
         Result := Bailout;

      elsif Option_Show_Version then
         declare
            use Ada.Text_IO, Setup;
            Version : constant String :=
              Program_Name & " (" &
              Get_Program_Version & ") Build (" &
              Get_Build_ISO8601 & ")";
         begin
            Put_Line (Version);
            New_Line;
            Put_Line ("The author disclaims copyright to this source code.  In place of");
            Put_Line ("a legal notice, here is a blessing:");
            New_Line;
            Put_Line ("   May you do good and not evil.");
            Put_Line ("   May you find forgiveness for yourself and forgive others.");
            Put_Line ("   May you share freely, not taking more than you give.");
            New_Line;
         end;
         Result := Bailout;

      end if;

   exception

      when Invalid_Switch =>
         Ada.Text_IO.Put_Line ("INVALID_SWITCH");

      when Invalid_Parameter =>
         Ada.Text_IO.Put_Line ("INVALID_PARAMETER");
         Result := Failure;

      when Exit_From_Command_Line =>
         Ada.Text_IO.Put_Line ("EXIt_FROM_COMMAND_LINE");
         Result := Bailout;

   end Process_Command_Line;


end Command_Line;
