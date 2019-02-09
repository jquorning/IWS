--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with GNAT.Command_Line;

with Options;

package body Command_Line is

   Long_Switch_Show_Help    : constant String := "--help";
   Long_Switch_Show_Version : constant String := "--version";
   Long_Switch_Web_Dir_Base : constant String := "--web-base=";
   Long_Switch_TCP_IP_Port  : constant String := "--port=";
   Long_Switch_Host_List    : constant String := "--host-list=";

   Config : GNAT.Command_Line.Command_Line_Configuration;

   procedure Parse (Success : out Boolean)
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
      Define_Switch (Config, Web_Dir_Base'Access, "-w=", Argument => "DIR",
                     Help        => "Base of web directory strcture.",
                     Long_Switch => Long_Switch_Web_Dir_Base);
      Define_Switch (Config, TCP_IP_Port'Access, "-p=", Argument => "PORT",
                     Help        => "Web server listen port.",
                     Long_Switch => Long_Switch_TCP_IP_Port);
      Define_Switch (Config, Host_List_File'Access, "-h=", Argument => "FILE",
                     Help        => "List of hosts to serve.",
                     Long_Switch => Long_Switch_Host_List);

      --  Do the whole parsing business.
      Getopt (Config);

      Success := True;

   exception

      when
        Invalid_Switch         |
        Invalid_Parameter      |
        Exit_From_Command_Line =>

         Success := False;
         raise;

   end Parse;


end Command_Line;
