--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Fixed;

package body Auxiliary is

   function Trim_Comments (From_String : String) return String
   is
      use Ada.Strings;
      Pos_Hash : constant Natural := Fixed.Index (From_String, "#");
      Pos_Ada  : constant Natural := Fixed.Index (From_String, "--");
      Position :          Natural := From_String'Last + 1;
   begin

      if Pos_Hash /= 0 then
         Position := Pos_Hash;
      end if;

      if Pos_Ada /= 0 then
         Position := Natural'Min (Position, Pos_Ada);
      end if;

      return
        Fixed.Trim
        (From_String (From_String'First .. Position - 1), Both);

   end Trim_Comments;

end Auxiliary;
