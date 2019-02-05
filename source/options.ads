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

   use GNAT.Strings;

   Option_Show_Version  : aliased Boolean;
   Option_Show_Help     : aliased Boolean;

   Option_Web_Directory : aliased String_Access := new String'("");
   Option_Host_Name     : aliased String_Access := new String'("");
   Option_Database_File : aliased String_Access := new String'("");

   Option_TCP_IP_Port   : aliased Integer;

end Options;
