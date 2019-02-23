--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with GAWS_IO;

package body GAWS_Log is

   use AWS.Log;


   procedure Start
   is
   begin
      Start (Log             => Log,
             Split           => Each_Run,
             File_Directory  => ".",
             Filename_Prefix => "GAWSLOG",
             Auto_Flush      => True);

      GAWS_IO.Put_Version (To_Log => True);
   end Start;


   procedure Stop
   is
   begin
      Stop (Log);
   end Stop;


   procedure Put_Line (Item : in String)
   is
   begin
      Write (Log, Item);
   end Put_Line;


   procedure Flush
   is
   begin
      Flush (Log);
   end Flush;


end GAWS_Log;
