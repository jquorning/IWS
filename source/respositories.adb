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
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;

with AWS.MIME;
with AWS.Templates;

with DK8543.AWS.Status;
with DK8543.AWS.MIME;

package body Respositories is

   use Ada.Containers;
   function Hash (Host : T_Host_Name) return Hash_Type;

   function Equivalent_Keys (Left, Right : T_Host_Name)
                            return Boolean;


   function Hash (Host : T_Host_Name) return Hash_Type
   is
   begin
      return Ada.Strings.Unbounded.Hash (Host);
   end Hash;


   function Equivalent_Keys (Left, Right : T_Host_Name)
                            return Boolean
   is
      use Ada.Strings.Unbounded;
   begin
      return Left = Right;
   end Equivalent_Keys;


   package Respository_Maps is
      new Ada.Containers.Hashed_Maps
     (Key_Type        => T_Host_Name,
      Element_Type    => T_Respository,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);

   Map : Respository_Maps.Map;

   type R_Respository is
      record
         null;
      end record;


   function Create_Respository
     (Host : in S_Host_Name) return T_Respository
   is
      use Ada.Directories;
      use Ada.Strings.Unbounded;
   begin
      if not Ada.Directories.Exists (Host) then
         raise Constraint_Error
           with "Web directory '" & Host & "' does not exist.";
      end if;
      declare
         Respository : constant T_Respository := new R_Respository;
      begin
         Respository_Maps.Insert (Map, To_Unbounded_String (Host), Respository);
         return Respository;
      end;
   end Create_Respository;


   function Delegate (Request : in AWS.Status.Data)
                     return AWS.Response.Data
   is
      use Ada.Strings.Unbounded;
      use DK8543.AWS.Status;
      Host_Name   : constant String        := Host_Part (AWS.Status.Host (Request));
      Respository : constant T_Respository := Map.Element (To_Unbounded_String (Host_Name));
   begin
      return Serve_Page (Respository, Request);
   end Delegate;


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
