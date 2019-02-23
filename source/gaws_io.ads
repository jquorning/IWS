--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package GAWS_IO is


   procedure Put_Version (To_Log : in Boolean := False);
   --  Put help text to terminal.

   procedure Put_Hints;
   --  Put hinst text to terminal.

   procedure Put_Blessing;
   --  Put blessing to terminal.


end GAWS_IO;
