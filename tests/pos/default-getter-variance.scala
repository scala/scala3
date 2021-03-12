class Foo[+A] {
  def count(f: A => Boolean = _ => true): Unit = {}
  // The preceding line is valid, even though the generated default getter
  // has type `A => Boolean` which wouldn't normally pass variance checks
  // because it's equivalent to the following overloads which are valid:
  def count2(f: A => Boolean): Unit = {}
  def count2(): Unit = count(_ => true)
}

class Bar1[+A] extends Foo[A] {
  override def count(f: A => Boolean): Unit = {}
  // This reasoning extends to overrides:
  override def count2(f: A => Boolean): Unit = {}
}

class Bar2[+A] extends Foo[A] {
  override def count(f: A => Boolean = _ => true): Unit = {}
  // ... including overrides which also override the default getter:
  override def count2(f: A => Boolean): Unit = {}
  override def count2(): Unit = count(_ => true)
}

// This can be contrasted with the need for variance checks in
// `protected[this] methods (cf tests/neg/t7093.scala),
// default getters do not have the same problem since they cannot
// appear in arbitrary contexts.


// Crucially, this argument does not apply to situations in which the default
// getter result type is not a subtype of the parameter type, for example (from
// tests/neg/variance.scala):
//
//   class Foo[+A: ClassTag](x: A) {
//     private[this] val elems: Array[A] = Array(x)
//     def f[B](x: Array[B] = elems): Array[B] = x
//   }
//
// If we tried to rewrite this with an overload, it would fail
// compilation:
//
//  def f[B](): Array[B] = f(elems) // error: Found: Array[A], Expected: Array[B]
//
// So we only disable variance checking for default getters whose
// result type is the method parameter type, this is checked by
// `tests/neg/variance.scala`
