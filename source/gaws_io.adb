--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

with Setup;
with GAWS_Log;

package body GAWS_IO is


   procedure Put_Version (To_Log : in Boolean := False)
   is
      use Ada.Text_IO, Setup;
      Program       : constant String := Setup.Get_Program_Name;
      Version       : constant String := Setup.Get_Program_Version;
      Timestamp     : constant String := Setup.Get_Build_ISO8601_UTC;
      Uname         : constant String := Setup.Get_Uname;
      Version_Image : constant String := Program & " (" & Version & ")";
      Build_Image   : constant String := "Build (" & Timestamp & ")";
      Uname_Image   : constant String := "Uname (" & Uname & ")";
   begin
      case To_Log is

         when False =>
            Put_Line (Version_Image);
            Put_Line (Build_Image);
            New_Line;

         when True =>
            GAWS_Log.Put_Line (Version_Image & " " & Build_Image);
            GAWS_Log.Put_Line (Uname_Image);

      end case;
   end Put_Version;


   procedure Put_Blessing
   is
      use Ada.Text_IO;
   begin
      Put_Line ("The author disclaims copyright to this source code.  In place of");
      Put_Line ("a legal notice, here is a blessing:");
      New_Line;
      Put_Line ("   May you do good and not evil.");
      Put_Line ("   May you find forgiveness for yourself and forgive others.");
      Put_Line ("   May you share freely, not taking more than you give.");
      New_Line;
   end Put_Blessing;


   procedure Put_Hints
   is
      use Ada.Text_IO, Setup;
   begin
      Put (Setup.Get_Program_Name);
      Put (": Try http://gaws.org:8088");
      New_Line;

      Put (Setup.Get_Program_Name);
      Put (": Stop server with Control-C");
      New_Line;
   end Put_Hints;


end GAWS_IO;
