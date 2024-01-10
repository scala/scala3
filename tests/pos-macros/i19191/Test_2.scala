def Test = {
  val vars: Vars[Int] = ???

  val works = vars.foreach { v => () }
  val fails = for (v <- vars) ()
}
