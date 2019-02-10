--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Directories;

with Auxiliary;

package body Options is


   procedure Read (Host_Count :    out Natural;
                   Index      : in     Positive := Positive'Last;
                   Host_Name  :    out String);


   function Host_List_File_Exists return Boolean
   is
      use Ada.Directories;
   begin
      return Exists (Host_List_File.all);
   end Host_List_File_Exists;


   procedure Read (Host_Count :    out Natural;
                   Index      : in     Positive := Positive'Last;
                   Host_Name  :    out String)
   is
      use Ada.Text_IO;
      use Auxiliary;
      File       : File_Type;
      Host_Index : Natural := 0;
      Length     : Natural;
   begin
      Open (File, In_File, Host_List_File.all);
      loop
         declare
            Line_Raw : constant String := Get_Line (File);
            Line     : constant String := Trim_Comments (Line_Raw);
         begin
            if Line /= "" then
               Host_Index := Host_Index + 1;
               if Host_Index = Index then
                  Length := Natural'Min (Line'Length, Host_Name'Length);
                  Host_Name := (others => ' ');
                  Host_Name (Host_Name'First .. Host_Name'First + Length - 1) :=
                    Line (Line'First .. Line'First + Length - 1);
               end if;
            end if;
         exception
            when End_Error =>
               exit;
         end;

      end loop;

   exception

      when End_Error =>
         Close (File);
         Host_Count := Host_Index;

   end Read;


   function Host_List_Hosts_Count return Natural
   is
      Host_Count : Natural;
      Host_Name  : String (1 .. 1000);
   begin

      if not Host_List_File_Exists then
         raise Constraint_Error
           with "Hosts lists file does not exist.";
      end if;

      Read (Host_Count, Host_Name => Host_Name);
      return Host_Count;
   end Host_List_Hosts_Count;


   function Get_Host (Index : in Positive) return String
   is
      use Ada.Strings;
      Host_Count : Natural;
      Host_Name  : String (1 .. 1000) := (others => ' ');
   begin
      Read (Host_Count, Index, Host_Name);
      return Fixed.Trim (Host_Name, Both);
   end Get_Host;

end Options;
