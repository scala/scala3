val x = Some(10)

def f =
   if x.exists
         (x => x == 10) then
      println("Yes")
   else
      println("No")