--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

with GNAT.Traceback.Symbolic;

with DK8543.AWS.Status;

with Web_Server;
with Web_Databases;

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

      --  Syncronous
      return Web_Databases.Serve_Page (Web_Server.Example, Request);

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
