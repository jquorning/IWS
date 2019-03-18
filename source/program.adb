--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with GAWS_Log;
with Config_File;
with Web_Server;

package body Program is

   procedure Run
   is
      Register_Success : Boolean;
   begin
      GAWS_Log.Start;

      --  Register hosts
      Config_File.Register_Hosts (Register_Success);
      GAWS_Log.Put_Register_Hosts (Register_Success);

      Web_Server.Startup;
      Web_Server.Work_Until_Stopped;
      Web_Server.Shutdown;
      GAWS_Log.Stop;
   end Run;

end Program;
