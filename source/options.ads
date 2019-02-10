--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with GNAT.Strings;

with Setup;

package Options is

   Show_Version  : aliased Boolean;
   Show_Help     : aliased Boolean;

   Default_Host_List_File : constant String := Setup.Get_Program_Name & ".hosts";
   Default_TCP_IP_Port    : constant String := "8080";

   use GNAT.Strings;
   Web_Dir_Base   : aliased String_Access := new String'("");
   Host_List_File : aliased String_Access := new String'(Default_Host_List_File);
   TCP_IP_Port    : aliased String_Access := new String'(Default_TCP_IP_Port);

end Options;
