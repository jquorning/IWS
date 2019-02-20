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

with AWS.MIME;
with AWS.Templates;

with DK8543.AWS.Status;
with DK8543.AWS.MIME;

package body Respositories is


   type R_Respository is
      record
         null;
      end record;


   function Create_Respository
     (Host : in T_Host_Name) return T_Respository
   is
      use Ada.Directories;
   begin
      if not Ada.Directories.Exists (Host) then
         raise Constraint_Error
           with "Web directory '" & Host & "' does not exist.";
      end if;
      return new R_Respository;
   end Create_Respository;


   function Serve_Page (Respository : in T_Respository;
                        Request     : in AWS.Status.Data)
                       return AWS.Response.Data
   is
      pragma Unreferenced (Respository);
      use AWS;
      use DK8543.AWS.Status;
      use DK8543.AWS.MIME;
      Data      : AWS.Response.Data;
      Web_Base  : constant String := "../respository/example.com/";
      URI       : constant String := Status.URI (Request);
      Host      : constant String := Host_Part (Status.Host (Request));
      File_Name : constant String := URI (URI'First + 1 .. URI'Last);
      Extension : constant String := Extension_Part (File_Name);
      MIME      : constant String := To_MIME (Extension);
   begin
      declare
         use Ada.Text_IO;
      begin
         Put ("Web_Databases.Service_Request: ");
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
         Data := AWS.Response.Build
           (AWS.MIME.Text_CSS,
            Message_Body => Templates.Parse (Web_Base & File_Name));

      elsif URI = "/favicon.ico" then
         Data := AWS.Response.Build
           (AWS.MIME.Text_HTML, Message_Body
              => Templates.Parse (Web_Base & "image/favicon.ico"));

      elsif URI = "/" then
         Data := AWS.Response.Build
           (AWS.MIME.Text_HTML,
            Message_Body => AWS.Templates.Parse (Web_Base & "static/main.html"));

      elsif URI = "/test" then
         Data := AWS.Response.Build
           (AWS.MIME.Text_HTML,
            Message_Body => "<html><head><title>Test</title></head>" &
              "<body><h1>Test</html>");

      else
         Ada.Text_IO.Put_Line ("URI is " & URI);
         Ada.Text_IO.Put_Line ("Filename is " & File_Name);
         Data := AWS.Response.Build
           (AWS.MIME.Text_HTML,
            Message_Body => Templates.Parse (Web_Base & "fejl.html"));
      end if;

      return Data;
   end Serve_Page;


end Respositories;
