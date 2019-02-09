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

package body Command_Line is

   procedure Getopt_Callback
     (Switch  : String;  Param : String;  Section : String);
   --  To be installed and called by Getopt on command line switch encounter.


   Long_Switch_Show_Help     : constant String := "--help";
   Long_Switch_Show_Version  : constant String := "--version";
   Long_Switch_Web_Directory : constant String := "--directory";
   Long_Switch_TCP_IP_Port   : constant String := "--port";
   Long_Switch_Host_Name     : constant String := "--hostname";
   Long_Switch_Database_File : constant String := "--database";

--   use GNAT.Command_Line;
   Config : GNAT.Command_Line.Command_Line_Configuration;


   procedure Getopt_Callback (Switch, Param, Section : String) is
   begin
      Ada.Text_IO.Put_Line ("Switch : " & Switch);
      Ada.Text_IO.Put_Line ("Param  : " & Param);
      Ada.Text_IO.Put_Line ("Section: " & Section);
   end Getopt_Callback;


   procedure Process_Command_Line (Result : out Process_Result)
   is
      use GNAT.Command_Line;
      use Options;
   begin
      Define_Switch (Config, Show_Help'Access,
                     Help => "Show this help text.",
                     Long_Switch => Long_Switch_Show_Help);
      Define_Switch (Config, Show_Version'Access,
                     Help        => "Show program name and version.",
                     Long_Switch => Long_Switch_Show_Version);
      Define_Switch (Config, Web_Directory'Access, "-d=",
                     Help        => "Static web directory.",
                     Long_Switch => Long_Switch_Web_Directory);
      Define_Switch (Config, TCP_IP_Port'Access, "-p=",
                     Help        => "Web server listen port.",
                     Long_Switch => Long_Switch_TCP_IP_Port);
      Define_Switch (Config, Host_Name'Access, "-h=",
                     Help        => "Host name to serve.",
                     Long_Switch => Long_Switch_Host_Name);
      Define_Switch (Config, Database_File'Access, "-b=",
                     Help        => "SQLite3 Database file.",
                     Long_Switch => Long_Switch_Database_File);

      --  Do the whole parsing business.
      --  Actually just the -D= option and file name.
      --  Getopt (Config, Getopt_Callback'Access);
      Getopt (Config);

      if Options.Show_Help then
         Result := Bailout;

      elsif Options.Show_Version then
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
