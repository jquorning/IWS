--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with AWS.Status;
with AWS.Response;

package Web_Databases is

   subtype T_Host_Name is String;

   type T_Respository is private;

   function Create_Respository
     (Host : in T_Host_Name) return T_Respository;
   --  Create respository for host.

   function Serve_Page (Respository : in T_Respository;
                        Request     : in AWS.Status.Data)
                       return AWS.Response.Data;
   --  Syncronous old style serve page.

private

   type R_Respository;
   type T_Respository is access all R_Respository;

end Web_Databases;
