--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Command_Line;

with Command_Line;
with Setup;
with Options;

procedure GAWS_Program is

   procedure Show_Help;
   --  Show help text to terminal.

   procedure Show_Help
   is
      use Ada.Text_IO, Setup;
      Version : constant String :=
        Program_Name & " (" &
        Get_Program_Version & ") Build (" &
        Get_Build_ISO8601 & ")";
   begin
      Put_Line (Version);
      New_Line;
      Put_Line ("The author disclaims copyright to this source code.  In place of");
      Put_Line ("a legal notice, here is a blessing:");
      New_Line;
      Put_Line ("   May you do good and not evil.");
      Put_Line ("   May you find forgiveness for yourself and forgive others.");
      Put_Line ("   May you share freely, not taking more than you give.");
      New_Line;
   end Show_Help;

   use Ada.Command_Line;
   use Command_Line;

   Status : Process_Result;
begin
   Process_Command_Line (Status);

   case Status is

      when Command_Line.Success  =>
         Set_Exit_Status (Ada.Command_Line.Success);

      when Command_Line.Failure  =>
         Set_Exit_Status (Ada.Command_Line.Failure);
         return;

      when Command_Line.Bailout  =>
         if Options.Show_Version then
            Show_Help;
         end if;
         Set_Exit_Status (Ada.Command_Line.Success);
         return;

   end case;

   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);

end GAWS_Program;
