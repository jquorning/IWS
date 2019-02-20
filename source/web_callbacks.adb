--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

with AWS.MIME;
--  with AWS.Templates;
--  with AWS.Parameters;

with GNAT.Traceback.Symbolic;

with DK8543.AWS.Status;
--  with DK8543.AWS.MIME;

with Web_Server;
with Web_Databases;

package body Web_Callbacks is

--   Translations : AWS.Templates.Translate_Set;


--   procedure Serve_Main_Page (Request : in AWS.Status.Data);
   --  Build main web page "/"


   procedure Initialize is
   begin
      null;
   end Initialize;


--     procedure Serve_Main_Page (Request : in AWS.Status.Data) is
--        List : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
--        CMD  : constant String := AWS.Parameters.Get (List, "cmd");
--        pragma Unreferenced (CMD);
--     begin
--        null;
--     end Serve_Main_Page;

   ----------
   -- Main --
   ----------




   function Main (Request : in AWS.Status.Data)
                 return AWS.Response.Data
   is
      use AWS;
      use DK8543.AWS.Status;
--      use DK8543.AWS.MIME;
--      URI       : constant String := Status.URI (Request);
      Host      : constant String := Host_Part (Status.Host (Request));
--      File_Name : constant String := URI (URI'First + 1 .. URI'Last);
--      Extension : constant String := Extension_Part (File_Name);
--      MIME      : constant String := To_MIME (Extension);
   begin
      declare
         use Ada.Text_IO;
      begin
         Put ("Web_Callbacks.Main: ");
         Put ("Serviceing host: ");
         Put (Host);
--           Put ("    ");
--           Put ("MIME: ");
--           Put (MIME);
--           Put ("    ");
--           Put ("URI: ");
--           Put (URI);
         New_Line;
      end;

      --  Asyncronous
      --  Web_Databases.Service_Request (Web_Server.Example, Request);

      --  Syncronous
      return Web_Databases.Serve_Page (Web_Server.Example, Request);

--        if
--          URI = "/stylesheets/print.css" or
--          URI = "/stylesheets/main.css" or
--          URI = "/stylesheets/boilerplate.css"
--        then
--           return AWS.Response.Build
--             (AWS.MIME.Text_CSS,
--              Message_Body => Templates.Parse (Web_Base & File_Name));

--        elsif URI = "/favicon.ico" then
--           return AWS.Response.Build
--             (AWS.MIME.Text_HTML, Message_Body
--                => Templates.Parse (Web_Base & "image/favicon.ico"));

--        elsif URI = "/" then
--           Serve_Main_Page (Request);
--           Web_Databases.Put (Web_Server.Example, Host, URI);
--           return AWS.Response.Build
--             (AWS.MIME.Text_HTML,
--              Message_Body => AWS.Templates.Parse (Web_Base & "static/main.html",
--                                                   Translations));

--        elsif URI = "/test" then
--           return AWS.Response.Build
--             (AWS.MIME.Text_HTML,
--              Message_Body => "<html><head><title>Test</title></head>" &
--                "<body><h1>Test</html>");

--        else
--           Ada.Text_IO.Put_Line ("URI is " & URI);
--           Ada.Text_IO.Put_Line ("Filename is " & File_Name);
--           return AWS.Response.Build
--             (AWS.MIME.Text_HTML,
--              Message_Body => Templates.Parse (Web_Base & "fejl.html"));
--        end if;
      return AWS.Response.Build
        (AWS.MIME.Text_HTML,
         Message_Body => AWS.Response.Default_Authenticate_Message);

   exception

      when others =>
         declare --  Call_Stack
            Trace  : GNAT.Traceback.Tracebacks_Array (1 .. 100);
            Length : Natural;
         begin
            GNAT.Traceback.Call_Chain (Trace, Length);
            Ada.Text_IO.Put_Line
              (GNAT.Traceback.Symbolic.Symbolic_Traceback
                 (Trace (1 .. Length)));
         end;
         raise;

   end Main;


end Web_Callbacks;
