--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Command_Line;

with Command_Line;
with Options;
with Exceptions;
with Program;
with GAWS_IO;

procedure GAWS_Program is

   Parse_Success : Boolean;

begin

   Command_Line.Parse (Parse_Success);

   if not Options.Be_Quiet then
      GAWS_IO.Put_Version;
      GAWS_IO.Put_Blessing;
   end if;

   if not Parse_Success then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   if  Options.Show_Version then
      GAWS_IO.Put_Version;
      return;
   end if;

   if not Options.Be_Quiet then
      GAWS_IO.Put_Hints;
   end if;

   Program.Run;

exception

   when Occurrence : others =>
      Exceptions.Put (Occurrence);

end GAWS_Program;
