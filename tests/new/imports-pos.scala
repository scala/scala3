package test;

import java.lang.System as S

object test {
  import S.out.{print as p, println as print}

  val foo = 1;

  p("hello"); print("world"); S.out.println("!");
  S.out.flush();
}
object test1 {
  import test._;
  foo
}
