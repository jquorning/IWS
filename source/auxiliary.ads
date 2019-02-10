--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package Auxiliary is

   function Trim_Comments (From_String : String) return String;
   --  Trim away comments from From_String. Comments are "#" and
   --  "--". Both at starting line and end of line comments.

end Auxiliary;
