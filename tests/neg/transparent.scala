object Invalid {
  transparent def f(x: Int) = x + 1
  f(1): String  // error
  f(1): {0}  // error

  // val y: Int = ???
  // type YPlusOne = {while}  // TODO: errror: Non-sensical singleton-type expression: ...
}

object Foo {
  transparent def foo(b: Boolean): { if (b) 1 else 2 } =
    if (b) 1 else 2

  foo(true):  { if(true) 2 else 1 }  // error
  foo(false): { if(false) 2 else 1 }  // error

  var b: Boolean = true
  foo(b): { 1 }  // error
}

// object CyclicTransparenType {
//   transparent def trans(j: Int): Int = {
//     println(opaque(j))
//     2 * j
//   }

//   def opaque(i: Int) = {trans(2): { 2 * 2 }} + i
// }

// object SimpleEqs {
//   val x = 1
//   val y: {x} = x
//   implicitly[{x + 1} =:= {y}]  // errror
//   implicitly[{x + 1} =:= {y + 2}]  // errror
//   implicitly[{x + 1} =:= {1 + y}]  // errror: TypeComparer doesn't know about commutativity

//   val b = true
//   implicitly[{b} =:= {b}]
//   implicitly[{!b} =:= {!b}]
//   implicitly[{!b} =:= {b}]  // errror
// }


// object Stability {
//   def f1(x: Int): Int = x
//   def f2(x: Int): {x} = x

//   val x = 1
//   implicitly[{f1(x)} =:= {x}]  // errror: f1 is not considered stable  // errror: f1's result type is not precise enough
//   implicitly[{f1(x)} =:= {f1(x)}]  // errror: f1 is not considered stable  // errror: f1 is not considered stable
//   implicitly[{f2(x)} =:= {x}]
//   implicitly[{f2(x)} =:= {f2(x)}]
//   implicitly[{f1(x)} =:= {f2(x)}]  // errror: f1 is not considered stable  // errror: f1's result type is not precise enough
// }
