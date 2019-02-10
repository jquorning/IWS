--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package Host_Lists is

   function Hosts_Count return Natural;
   --  Return hosts count. Call Host_List_File_Exists before this
   --  function to avoid Constraint_Error.

   function Get_Host (Index : in Positive) return String;

end Host_Lists;
