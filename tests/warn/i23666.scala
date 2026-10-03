
type Tuple = scala.Tuple

infix type =:= [A, B] = A => B

object `=:=` :
   given [A] => A =:= A = a => a

extension [T <: Tuple] (tuple: T)

   def reverse[A, B](using ev: T =:= (A, B)): (B, A) = // warn
      val ab = ev(tuple)
      (ab._2, ab._1)

   def toList: List[Nothing] = Nil // warn
