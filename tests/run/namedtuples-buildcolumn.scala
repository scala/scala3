import NamedTuple.*
import compiletime.{constValue, erasedValue}

// The generic buildColumn function
transparent inline def buildColumn[N <: Tuple, V <: Tuple, T](
  r: NamedTuple[N, V], name: String, f: NamedTuple[N, V] => T)
  (using Tuple.Disjoint[N, Tuple1[name.type]] =:= true)
    : Concat[NamedTuple[N, V], NamedTuple[Tuple1[name.type], Tuple1[T]]]
    = r ++ Tuple1(f(r)).withNames[Tuple1[name.type]]

extension [N <: Tuple, V <: Tuple](r: NamedTuple[N, V])
  inline def mkString: String =
    r.toSeqMap
      .map((n, v) => s"$n = $v")
      .mkString("(", ", ", ")")

// Concrete Pet example
type Pet = (kind: String, age: Int, weight: Double)

val pet = (kind = "Dog", age = 7, weight = 6.5)

@main def Test =
  val pet2 = buildColumn(pet, "growth", pet => pet.weight / pet.age)
  // Test the inferred type:
  val _: (kind: String, age: Int, weight: Double, growth: Double) = pet2
  // Show the results
  println(pet.mkString)
  println(pet2.mkString)


