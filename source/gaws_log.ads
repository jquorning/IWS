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
   procedure Flush;

   procedure Put_Horizontal_Line (Length : in Natural   := 60;
                                  Marker : in Character := '-');
   procedure Put_Line (Item : in String);
   procedure Put_Register_Hosts (Success : in Boolean);

end GAWS_Log;
