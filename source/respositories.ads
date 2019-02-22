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

package Respositories is

   Unknown_Host : exception;
   --  Host is not known

   subtype S_Host_Name is String;
   subtype T_Host_Name is Ada.Strings.Unbounded.Unbounded_String;

   type T_Respository is private;

   procedure Append_Respository (Host_Name : in S_Host_Name);
   --  Append serving host with Host_Name to list of host.

   function Delegate (Request : in AWS.Status.Data)
                     return AWS.Response.Data;
   --  Deletate Request out to the respository and get responce back.

   function Serve_Page (Respository : in T_Respository;
                        Request     : in AWS.Status.Data)
                       return AWS.Response.Data;
   --  Syncronous old style serve page.

private

   type R_Respository;
   type T_Respository is access all R_Respository;

end Respositories;
