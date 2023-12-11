type SecondOfTwo[X <: Tuple2[Any, Any]] = Tuple.Head[Tuple.Tail[X]]
val a = implicitly[SecondOfTwo[Tuple2[Int, String]] =:= String]

type LastOfThree[X <: Tuple3[Any, Any, Any]] = Tuple.Tail[Tuple.Tail[X]]
val b = implicitly[LastOfThree[Tuple3[Int, String, Boolean]] =:= Tuple1[Boolean]]
