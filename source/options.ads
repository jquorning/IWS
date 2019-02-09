--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with GNAT.Strings;

package Options is

   Show_Version  : aliased Boolean;
   Show_Help     : aliased Boolean;

   use GNAT.Strings;
   Web_Directory : aliased String_Access := new String'("");
   Host_Name     : aliased String_Access := new String'("");
   Database_File : aliased String_Access := new String'("");
   TCP_IP_Port   : aliased String_Access := new String'("80");

end Options;
