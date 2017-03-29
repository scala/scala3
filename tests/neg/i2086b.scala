
object Test { // error: Test cannot be instantiated since it has a member type Bound with possibly conflicting bounds [Left, Right] => Left <: ... <: [Left, Right] => Right
  type Bound[Left, Right] >: Left <: Right
  type Foo = Bound[Any, Nothing]
}
