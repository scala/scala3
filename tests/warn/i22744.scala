
//> using options -Wunused:privates -Werror

object test {
  private trait Foo[A] { val value: A }

  private object Foo { // no warn prefix of implicit value
    given int: Foo[Int] = new Foo[Int] { val value = 1 }
  }

  val i = summon[Foo[Int]].value
}

object supplement {
  private trait Foo[A] { val value: A }

  private object Foo { // no warn prefix of implicit value
    given int: Foo[Int] = new Foo[Int] { val value = 1 }
  }

  private def fooValue[A](using f: Foo[A]): A = f.value

  val i = fooValue[Int]
}

package p:
  private trait Foo[A] { val value: A }

  private object Foo { // no warn prefix of implicit value
    given int: Foo[Int] = new Foo[Int] { val value = 1 }
  }

  private def fooValue[A](using f: Foo[A]): A = f.value

  val i = fooValue[Int]
