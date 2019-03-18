--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Unbounded;

with AWS.Status;
with AWS.Response;
with AWS.Services.Dispatchers.Virtual_Host;

package Virtual_Hosts is

   Unknown_Host : exception;
   --  Host is not known

   subtype S_Host_Name is String;
   subtype T_Host_Name is Ada.Strings.Unbounded.Unbounded_String;

   type T_Respository is private;

   procedure Append_Respository (Host_Name : in     S_Host_Name;
                                 Success   :    out Boolean);
   --  Append serving host with Host_Name to list of host.

   function Serve_Page (Request     : in AWS.Status.Data)
                       return AWS.Response.Data;
   --  Syncronous old style serve page.


   type Virtual_Host_Dispatcher is
     new AWS.Services.Dispatchers.Virtual_Host.Handler with
     null record;

   overriding function Dispatch
     (Dispatcher : Virtual_Host_Dispatcher;
      Request    : AWS.Status.Data)
     return AWS.Response.Data;
   --  Returns an error message (code 404) if there is no match for the request


   Dispatcher : Virtual_Host_Dispatcher;

private

   type C_Respository;
   type T_Respository is access all C_Respository'Class;

end Virtual_Hosts;
