trait Foo[T <: Bar[T]#Elem] // error: illegal cyclic reference
trait Bar[T] {
  type Elem = T
}
trait Foo2[T <: Bar2[T]#Elem] // error: illegal cyclic reference
trait Bar2[T] {
  type Elem = T
}
