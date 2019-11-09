package scala.tasty
package reflect
/* FIXME Using class tags to type tests abstract type members in unsound

   ```
   trait R {
     type Nat
     type Succ <: Nat
     type Idx
     given ClassTag[Nat]
     given ClassTag[Succ]
     given ClassTag[Idx]
     def n: Nat
   }
   object RI extens R {
     type Nat = Int
     type Succ = Int
     type Idx = Int
     given ClassTag[Nat] = classOf[Integer]
     given ClassTag[Succ] = new ClassTag[Integer] {
       def runtimeClass = classOf[Integer]
       def unapply(x: Any): Option[Succ] = x match
         case n: Int if n > 0 => Some(n)
         case _ => None
     }
     given ClassTag[Idx] = classOf[Integer]
     def n: Nat = 4
   }
   val r1: R = RI
   val r2: R = RI
   r1.n match {
     case n: r2.Nat => // Should not match or should have an unchecked waring
     case n: r1.Idx => // Should not match or should have an unchecked waring
     case n: r1.Succ => // Should match only if r1.n is an r1.Succ under the constraints set in r1
   }
   ```
*/
/** Place holder until we implement a ClassTag like abstraction that is sound for all type tests */
type IsInstanceOf[T] = scala.reflect.ClassTag[T]
