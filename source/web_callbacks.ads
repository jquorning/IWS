--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with AWS.Response;
with AWS.Status;

package Web_Callbacks is

   procedure Initialize;

   function Main (Request : in AWS.Status.Data)
                 return AWS.Response.Data;

end Web_Callbacks;
