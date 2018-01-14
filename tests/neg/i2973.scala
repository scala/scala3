class Foo[U]
class Bar[-T] extends Foo[T] // error: contravariant type T occurs in invariant position
