class Foo[A]
class Bar[A] extends Foo  // was error, now expanded to Foo[Nothing]
