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

      GAWS_Log.Put_Horizontal_Line;
      GAWS_IO.Put_Version (To_Log => True);
   end Start;


   procedure Stop
   is
   begin
      Stop (Log);
   end Stop;


   procedure Flush
   is
   begin
      Flush (Log);
   end Flush;


   procedure Put_Horizontal_Line (Length : in Natural   := 60;
                                  Marker : in Character := '-')
   is
      Line : constant String (1 .. Length) := (others => Marker);
   begin
      Put_Line (Line);
   end Put_Horizontal_Line;


   procedure Put_Line (Item : in String)
   is
   begin
      Write (Log, Item);
   end Put_Line;


   procedure Put_Register_Hosts (Success : in Boolean)
   is
   begin
      case Success is
         when True  =>  Put_Line ("Registered all host successfully");
         when False =>  Put_Line ("ERROR: One or more hosts failed to register");
      end case;
   end Put_Register_Hosts;


end GAWS_Log;
