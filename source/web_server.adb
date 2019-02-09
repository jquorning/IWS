--
--  This is a very simple Web Page Server using the AWS.Services.Page_Server.
--

with Ada.Text_IO;

with AWS.Config;
with AWS.Server.Log;
with AWS.Services.Page_Server;

with Web_Callbacks;
with Options;

package body Web_Server is

   Server : AWS.Server.HTTP;

   procedure Startup is

      Config : constant AWS.Config.Object := AWS.Config.Get_Current;

   begin
      if AWS.Config.Directory_Browser_Page (Config) /= "" then
         AWS.Services.Page_Server.Directory_Browsing (True);
      end if;

      if AWS.Config.Log_Filename_Prefix (Config) /= "" then
         AWS.Server.Log.Start (Server);
      end if;

      if AWS.Config.Error_Log_Filename_Prefix (Config) /= "" then
         AWS.Server.Log.Start_Error (Server);
      end if;

--      AWS.Services.Web_Block.Registry.Register
--        ("/", "../web/main.thtml", null);

--           Templates.Insert
--             (Translations,
--              Templates.Assoc ("TIL_DATO",
--                               My_Dates.To_ISO8601_Date (Til_Dato)));
      Web_Callbacks.Initialize;

      AWS.Server.Start
        (Server,
         Name     => "web_block",
         Callback => Web_Callbacks.Main'Access,
         Port     => Integer'Value (Options.TCP_IP_Port.all));
   end Startup;


   procedure Shutdown is
   begin
      Ada.Text_IO.Put_Line ("AWS server shutdown in progress.");
      AWS.Server.Shutdown (Server);
   end Shutdown;


end Web_Server;

