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

   subtype Host_String is String;
   function Host_Part (Host : in Host_String) return Host_String;
   --  Get host part of Host with possibly port.
   --  Example: "example.com:8088" returns "example.com".

   subtype File_Name_String is String;
   subtype File_Extension_String is String;
   function Extension_Part
     (File_Name : in File_Name_String) return File_Extension_String;
   --  Get extension part of File_Name with possible file extension.
   --  Example: "main.css" returns "css".

   subtype T_MIME is String;
   function To_MIME (Extension : in File_Extension_String) return T_MIME;
   --  Get MIME type from file extension.
   --  Example: "css" returns "text/css".
   --  Example: ""    returns "text/html".


   function Host_Part (Host : in Host_String) return Host_String
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


   function Extension_Part
     (File_Name : in File_Name_String) return File_Name_String
   is
      use Ada.Strings;
      Separator          : constant String  := ".";
      Separator_Position : constant Natural := Fixed.Index (File_Name, Separator,
                                                            Going => Backward);
   begin
      if Separator_Position = 0 then
         return "";
      else
         return File_Name (Separator_Position + Separator'Length .. File_Name'Last);
      end if;
   end Extension_Part;


   function To_MIME (Extension : in File_Extension_String) return T_MIME is
      use AWS.MIME;
      LC : constant String := Extension;
   begin
      if    LC = ""     then  return Text_HTML;
      elsif LC = "html" then  return Text_HTML;
      elsif LC = "css"  then  return Text_CSS;
      else
         raise Constraint_Error;
      end if;
   end To_MIME;


   function Main (Request : in AWS.Status.Data)
                 return AWS.Response.Data
   is
      use AWS;

      URI       : constant String := Status.URI (Request);
      Host      : constant String := Host_Part (Status.Host (Request));
      File_Name : constant String := URI (URI'First + 1 .. URI'Last);
      Extension : constant String := Extension_Part (File_Name);
      MIME      : constant String := To_MIME (Extension);
   begin
      declare
         use Ada.Text_IO;
      begin
         Put ("Host: ");
         Put (Host);
         Put ("    ");
         Put ("MIME: ");
         Put (MIME);
         Put ("    ");
         Put ("URI: ");
         Put (URI);
         New_Line;
      end;

      if
        URI = "/stylesheets/print.css" or
        URI = "/stylesheets/main.css" or
        URI = "/stylesheets/boilerplate.css"
      then
         return AWS.Response.Build
           (AWS.MIME.Text_CSS,
            Message_Body => Templates.Parse (Web_Base & File_Name));

      elsif URI = "/favicon.ico" then
         return AWS.Response.Build
           (AWS.MIME.Text_HTML, Message_Body
              => Templates.Parse (Web_Base & "favicon.ico"));

      elsif URI = "/" then
         Serve_Main_Page (Request);
         return AWS.Response.Build
           (AWS.MIME.Text_HTML,
            Message_Body => AWS.Templates.Parse (Web_Base & "main.thtml",
                                                 Translations));

      elsif URI = "/test" then
         return AWS.Response.Build
           (AWS.MIME.Text_HTML,
            Message_Body => "<html><head><title>Test</title></head>" &
              "<body><h1>Test</html>");

      else
         Ada.Text_IO.Put_Line ("URI is " & URI);
         Ada.Text_IO.Put_Line ("Filename is " & File_Name);
         return AWS.Response.Build
           (AWS.MIME.Text_HTML,
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
