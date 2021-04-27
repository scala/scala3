object Foo

val x: Foo.type = Foo
val y: Foo.type | Nothing = x
val z: Foo.type = y
val a: 1 | Nothing = 1
val b: 1 = a

val intSuper = summon[(Int | Nothing) <:< Int]
val intSub = summon[Int <:< (Int | Nothing)]
val intEq = summon[Int =:= (Int | Nothing)]

val fooSuper = summon[(Foo.type | Nothing) <:< Foo.type]
val fooSub = summon[Foo.type <:< (Foo.type | Nothing)]
val fooEq = summon[Foo.type =:= (Foo.type | Nothing)]

val oneSuper = summon[(1 | Nothing) <:< 1]
val oneSub = summon[1 <:< (1 | Nothing)]
val oneEq = summon[1 =:= (1 | Nothing)]

