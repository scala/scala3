import scala.util.NotGiven
val x: LeakFoo[1] = ??? : LeakFoo[2]  // error
val notok = summon[NotGiven[LeakFoo[1] =:= LeakFoo[2]]] // ok
