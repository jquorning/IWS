--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

package body Exceptions is

   procedure Put
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      use Ada.Exceptions;
      use Ada.Text_IO;
      Occ : Exception_Occurrence renames Occurrence;
   begin
      Put_Line (Exception_Name (Occ));
      New_Line;
      Put_Line (Exception_Message (Occ));
      New_Line;
   end Put;

end Exceptions;
