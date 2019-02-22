--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with DK8543.AWS.Status;

with Respositories;

package body Web_Callbacks is

--   Translations : AWS.Templates.Translate_Set;


   procedure Initialize is
   begin
      null;
   end Initialize;

   ----------
   -- Main --
   ----------

   function Main (Request : in AWS.Status.Data)
                 return AWS.Response.Data
   is
      use AWS;
      use DK8543.AWS.Status;
      Host : constant String := Host_Part (Status.Host (Request));
      pragma Unreferenced (Host);
   begin

      --  Asyncronous
      --  Web_Databases.Service_Request (Web_Server.Example, Request);

      --  Syncronous delegation
      return Respositories.Delegate (Request);

   end Main;


end Web_Callbacks;
