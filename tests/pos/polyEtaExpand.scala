object polyEtaExpand:
        
    def id[T](x: T) = x

    val valId1 /*Any => Any*/ = id
    // TODO: should be v, but valValId2 needs to keep compiling even after that
    // which requires polymorphic functions to have their parameters infered
    //val valId1 /*[T] => T => T*/ = id
    //val valValId1: [T] => T => T = valId1
    val valValId2: Any => Any = valId1
    
    val valId2: Int => Int    = id
    val valId3: [U] => U => U = id
    val valId4: [U <: Int] => U => U = id

    // v doesn't work, as id: [T] => T => T instead of [T] => T => F[T] (even tho F[_] could be Id[_])
    //val mapped: (Int, String, Char) = (1, "two", '3').map(id) 

    def monoPair[T](x: T)(y: T): (T, T) = (x, y)
    val valMonoPair: [T] => T => T => (T,T) = monoPair

    def protoPair[T](x: T): [U] => (U) => (T, U) = [U] => (y: U) => (x,y)
    val valProtoPair1: [T] => (T) => [U] => U => (T, U) = protoPair

    val x: [T] => T => Option[T] = Option.apply

    // TODO: uncomment when https://github.com/lampepfl/dotty/issues/14351 is solved
    //val p: (Option[Int], Option[String]) = (1,"foo").map(Option.apply)


    // Variance

    def simpleVariance1[T](x: T): Nothing = ???
    val valSimpleVariance1: [T] => T => T = simpleVariance1

    def simpleVariance2[T](x: Any): T = ???
    val valSimpleVariance2: [T] => T => T = simpleVariance2

    type Z
    type X <: Z
    type Y >: X <: Z

    val complicatedVarNormalExpand: [T >: X <: Y ] => X => Z = [T >: X <: Y ] => id[T]
    val complicatedVar: [T >: X <: Y ] => X => Z = id
