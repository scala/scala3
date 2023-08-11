trait A[T <: Tuple] { val x: Int }
given empty: A[EmptyTuple] with { val x = 1 }
given inductive[Tup <: NonEmptyTuple](using A[Tuple.Tail[Tup]]): A[Tup] with { val x = summon[A[Tuple.Tail[Tup]]].x + 1 }

object Test:
  def main(args: Array[String]): Unit =
    println(summon[A[(String, String, String)]].x) //this line is fine
    println(summon[A[(String, String, String, String)]].x) //this line gives error
end Test
