--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Web_Databases;

package Web_Server is

   procedure Startup;
   procedure Work_Until_Stopped;
   procedure Shutdown;

   Example : Web_Databases.T_Respository;

end Web_Server;

