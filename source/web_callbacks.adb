--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Strings.Fixed;

with AWS.MIME;
with AWS.Templates;
with AWS.Parameters;

with GNAT.Traceback.Symbolic;

package body Web_Callbacks is

   Web_Base : constant String := "../web/";
   Translations : AWS.Templates.Translate_Set;


   procedure Serve_Main_Page (Request : in AWS.Status.Data);
   --  Build main web page "/"


   procedure Initialize is
   begin
      null;
   end Initialize;


   procedure Serve_Main_Page (Request : in AWS.Status.Data) is
      List : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      CMD  : constant String := AWS.Parameters.Get (List, "cmd");
      pragma Unreferenced (CMD);
   begin
      null;
   end Serve_Main_Page;

   ----------
   -- Main --
   ----------

   function Host_Part (Host : in String) return String;
   --  Get host part of Host with possibly port like "www.example.com:8088".

   function Host_Part (Host : in String) return String
   is
      use Ada.Strings.Fixed;
      Separator          : constant String := ":";
      Separator_Position : constant Natural := Index (Host, Separator);
   begin
      if Separator_Position = 0 then
         return Host;
      else
         return Host (Host'First .. Separator_Position - 1);
      end if;
   end Host_Part;

   function Main (Request : in AWS.Status.Data)
                 return AWS.Response.Data
   is
      use AWS;

      URI      : constant String := Status.URI (Request);
      Host     : constant String := Host_Part (Status.Host (Request));
      Filename : constant String := URI (URI'First + 1 .. URI'Last);
   begin
      declare
         use Ada.Text_IO;
      begin
         Put ("Host: ");
         Put (Host);
         Put ("    ");
         Put ("URI: ");
         Put (URI);
         New_Line;
      end;

      if
        URI = "/stylesheets/print.css" or
        URI = "/stylesheets/main.css" or
        URI = "/stylesheets/boilerplate.css" or
        URI = "/css/rg.css"
      then
         return AWS.Response.Build
           (MIME.Text_CSS,
            Message_Body => Templates.Parse (Web_Base & Filename));

      elsif URI = "/favicon.ico" then
         Ada.Text_IO.Put_Line ("Serving ikon " & URI);
         return AWS.Response.Build
           (MIME.Text_HTML, Message_Body
              => Templates.Parse (Web_Base & "favicon.ico"));

      elsif URI = "/" then
         Serve_Main_Page (Request);
         return AWS.Response.Build
           (MIME.Text_HTML,
            Message_Body => AWS.Templates.Parse (Web_Base & "main.thtml",
                                                 Translations));

      elsif URI = "/test" then
         return AWS.Response.Build
           (MIME.Text_HTML,
            Message_Body => "<html><head><title>Test</title></head>" &
              "<body><h1>Test</html>");

      else
         Ada.Text_IO.Put_Line ("URI is " & URI);
         Ada.Text_IO.Put_Line ("Filename is " & Filename);
         return AWS.Response.Build
           (MIME.Text_HTML,
            Message_Body => Templates.Parse (Web_Base & "fejl.html"));
      end if;

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
