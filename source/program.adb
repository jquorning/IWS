--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Web_Server;

package body Program is

   procedure Run
   is
   begin
      Web_Server.Startup;
      delay 20.000;
      Web_Server.Shutdown;
   end Run;

end Program;
