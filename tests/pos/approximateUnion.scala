object approximateUnion {

  trait C[+T]
  trait D
  trait E
  trait X[-T]

  {
    trait A extends C[A] with D
    trait B extends C[B] with D

    val coin = true
    val x = if (coin) new A{} else new B{}
    val y = Some(if (coin) new A{} else new B{} )

    val xtest: C[A | B] & D = x
    val ytest: Some[C[A | B] & D] = y
  }

  {
    trait A extends C[X[A]] with D
    trait B extends C[X[B]] with D with E

    val coin = true
    val x = if (coin) new A{} else new B{}
    val y = Some(if (coin) new A{} else new B{})

    val xtest: C[X[A & B]] & D = x
    val ytest: Some[C[X[A & B]] & D] = y
  }
}

object approximateUnion2 {

  trait C[T]
  trait D
  trait E
  trait X[-T]

  {
    trait A extends C[A] with D
    trait B extends C[B] with D

    val coin = true
    val x = if (coin) new A{} else new B{}
    val y = Some(if (coin) new A{} else new B{})

    val xtest: C[_ >: A & B <: A | B] & D = x
    val ytest: Some[C[_ >: A & B <: A | B] & D] = y
  }

  {
    trait A extends C[X[A]] with D
    trait B extends C[X[B]] with D with E

    val coin = true
    val x = if (coin) new A{} else new B{}
    val y = Some(if (coin) new A{} else new B{})

    val xtest: C[_ >: X[A | B] <: X[A & B]] & D = x
    val ytest: Some[C[_ >: X[A | B] <: X[A & B]]] = y
  }
}

object approximateUnion3 {

  trait C[-T]
  trait D
  trait E
  trait X[-T]

  {
    trait A extends C[A] with D
    trait B extends C[B] with D

    val coin = true
    val x = if (coin) new A{} else new B{}
    val y = Some(if (coin) new A{} else new B{})

    val xtest: C[A & B] & D = x
    val ytest: Some[C[A & B] & D] = y
  }

  {
    trait A extends C[X[A]] with D
    trait B extends C[X[B]] with D with E

    val coin = true
    val x = if (coin) new A{} else new B{}
    val y = Some(if (coin) new A{} else new B{})

    val xtest: C[X[A | B]] & D = x
    val ytest2: Some[C[X[A | B]] & D] = y
  }
}

