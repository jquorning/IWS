--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Unbounded;

package Web_Databases is

   subtype T_Host_Name is String;

--   function Dir_Exists (Host : in T_Host_Name) return Boolean;
   --

--   procedure Register (Host : in T_Host_Name);
   --  May rise Constraint_Error when Host does not exist.

   type T_Respository is private;

   function Create_Respository
     (Host : in T_Host_Name) return T_Respository;
   --  Create respository for host.

   use Ada.Strings.Unbounded;
   type T_Request is
      record
         Host : Unbounded_String;
         URL  : Unbounded_String;
      end record;

   procedure Put (Respository : in T_Respository;
                  Host        : in String;
                  URI         : in String);
   --  Put Request on work qeueue of Respository.

private

   type R_Respository;
   type T_Respository is access all R_Respository;

end Web_Databases;
