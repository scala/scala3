trait Show[A] {
  def show(value: A): String
}

object Show {
  given identity: Show[String] = a => a

  given int: Show[Int] = _.toString()

  given list[A](using A: Show[A]): Show[List[A]] = _.map(A.show).toString()
}
