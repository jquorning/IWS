--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with AWS.Log;

package GAWS_Log is

   Log : AWS.Log.Object;

   procedure Start;
   procedure Stop;
   procedure Put_Line (Item : in String);
   procedure Flush;

end GAWS_Log;
