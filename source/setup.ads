--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package Setup is

   Program_Name         : constant String := "GAWS";

   function Get_Program_Version return String;
   function Get_Build_ISO8601 return String;
   function Get_Uname_M return String;
   function Get_Uname_N return String;
   function Get_Uname_P return String;
   function Get_Uname_R return String;
   function Get_Uname_S return String;

end Setup;
