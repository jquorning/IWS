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

with GAWS_Log;

package body Virtual_Hosts is


   Respository_Base : constant String := "../respository/";


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

   type C_Respository is
     new AWS.Services.Dispatchers.Virtual_Host.Handler with
     null record;


   overriding function Dispatch
     (Dispatcher : Virtual_Host_Dispatcher;
      Request    : AWS.Status.Data)
     return AWS.Response.Data
   is
      pragma Unreferenced (Dispatcher);
   begin
      return Serve_Page (Request);
   end Dispatch;


   procedure Append_Respository (Host_Name : in     S_Host_Name;
                                 Success   :    out Boolean)
   is
      use Ada.Strings.Unbounded;
   begin
      Success := False;

      if not Ada.Directories.Exists (Host_Name) then
         return;
      end if;

      declare
         use Respository_Maps;

         Host_Unbound : constant Unbounded_String := To_Unbounded_String (Host_Name);
         Respository  : C_Respository;
      begin
         if Find (Map, Host_Unbound) = No_Element then
            --  Insert (Map, Host_Unbound, Respository);

            Register
              (Dispatcher,
               Host_Name,
               Respository);

            Success := True;
         else
            Success := False;
         end if;
      end;
   end Append_Respository;


   function Serve_Page (Request     : in AWS.Status.Data)
                       return AWS.Response.Data
   is
      use AWS;
      use AWS.MIME;
      use DK8543.AWS.Status;
      use DK8543.AWS.MIME;

      Data      : AWS.Response.Data;
      URI       : constant String := Status.URI (Request);
      Host      : constant String := Host_Part (Status.Host (Request));
      File_Name : constant String := URI (URI'First + 1 .. URI'Last);
      Extension : constant String := Extension_Part (File_Name);
      MIME      : constant String := To_MIME (Extension);
      Host_Base : constant String := Respository_Base & Host & "/";
   begin
      GAWS_Log.Put_Line (Host & "  " & URI & "  " & MIME);

      if URI = "/" then
         Data := AWS.Response.Build
           (AWS.MIME.Text_HTML,
            Message_Body => AWS.Templates.Parse (Host_Base & "static/main.html"));

      elsif URI = "/favicon.ico" then
         Data := AWS.Response.Build
           (AWS.MIME.Text_HTML, Message_Body
              => Templates.Parse (Host_Base & "image/favicon.ico"));

      elsif URI = "/test" then
         Data := AWS.Response.Build
           (AWS.MIME.Text_HTML,
            Message_Body => "<html><head><title>Test</title></head>" &
              "<body><h1>Test</html>");

      elsif MIME = Text_HTML then
         Data := AWS.Response.Build
           (AWS.MIME.Text_HTML,
            Message_Body => AWS.Templates.Parse (Host_Base & URI));

      elsif MIME = Text_CSS then
         Data := AWS.Response.Build
           (AWS.MIME.Text_CSS,
            Message_Body => Templates.Parse (Host_Base & URI));

      else
         Ada.Text_IO.Put_Line ("URI is " & URI);
         Ada.Text_IO.Put_Line ("Filename is " & File_Name);
         Data := AWS.Response.Build
           (AWS.MIME.Text_HTML,
            Message_Body => Templates.Parse (Respository_Base & "common/fejl.html"));
      end if;

      return Data;
   end Serve_Page;


end Virtual_Hosts;
