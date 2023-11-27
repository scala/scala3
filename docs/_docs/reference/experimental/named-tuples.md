---
layout: doc-page
title: "Named Tuples"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/named-tuples.html
---

The elements of a tuple can now be named. Example:
```scala
type Person = (name: String, age: Int)
val Bob: Person = (name = "Bob", age = 33)

Bob match
  case (name, age) =>
    println(s"$name is $age years old")

val persons: List[Person] = ...
val minors = persons.filter: p =>
  p.age < 18
```
Named bindings in tuples are similar to function parameters and arguments. We use `name: Type` for element types and `name = value` for element values. It is illegal to mix named and unnamed elements in a tuple, or to use the same same
name for two different elements.

Fields of named tuples can be selected by their name, as in the line `p.age < 18` above.

### Conformance

The order of names in a named tuple matters. For instance, the type `Person` above and the type `(age: Int, name: String)` would be different, incompatible types.

Values of named tuple types can also be be defined using regular tuples. For instance:
```scala
val x: Person = ("Laura", 25)

def register(person: Person) = ...
register(person = ("Silvain", 16))
register(("Silvain", 16))
```
This follows since a regular tuple `(T_1, ..., T_n)` is treated as a subtype of a named tuple `(N_1 = T_1, ..., N_n = T_n)` with the same element types. On the other hand, named tuples do not conform to unnamed tuples, so the following is an error:
```scala
val x: (String, Int) = Bob           // error: type mismatch
```
One can convert a named tuple to an unnamed tuple with the `dropNames` method, so the following works:
```scala
val x: (String, Int) = Bob.dropNames // ok
```
Note that conformance rules for named tuples are analogous to the rules for named parameters. One can assign parameters by position to a named parameter list.
```scala
  def f(param: Int) = ...
  f(param = 1)   // OK
  f(2)           // Also OK
```
But one cannot use a name to pass an argument to an unnamed parameter:
```scala
    val f: Int => T
    f(2)         // OK
    f(param = 2) // Not OK
```
The rules for tuples are analogous. Unnamed tuples conform to named tuple types, but the opposite does not hold.


### Pattern Matching

When pattern matching on a named tuple, the pattern may be named or unnamed.
If the pattern is named it needs to mention only a subset of the tuple names, and these names can come in any order. So the following are all OK:
```scala
Bob match
  case (name, age) => ...

Bob match
  case (name = x, age = y) => ...

Bob match
  case (age = x) => ...

Bob match
  case (age = x, name = y) => ...
```

### Expansion

Named tuples are in essence just a convenient syntax for regular tuples with  element types. In the internal representation, an element like `age: Int` of a named tuple is given the type `NamedTuple.Element["age", Int]`. This type
is defined in object `NamedTuple` as follows:
```scala
  opaque type Element[name <: String & Singleton, A] >: A = A
```
`Element` is an opaque type alias of its second, value parameter. The first parameter is a string constant type which determines the name of the element.
Since the type is just an alias of its value part, names are erased at runtime,
and named tuples and regular tuples have the same representation.

The `Element` type publicly known to be a supertype (but not a subtype) of its value paramater, which means that regular tuples can be assigned to named tuples but not _vice versa_.

A minimal implementation of the `NamedTuple` object looks like this:
```scala
package scala
object NamedTuple:

  opaque type Element[name <: String & Singleton, A] >: A = A

  object Element:
    def apply[S <: String & Singleton, A](name: S, x: A): Element[name.type, A] = x
    def extract[S <: String & Singleton]: ValueExtractor[S] = ValueExtractor[S]()
    extension [S <: String & Singleton, A](named: Element[S, A]) def value: A = named

    class ValueExtractor[S <: String & Singleton]:
      def unapply[A](x: Element[S, A]): Some[A] = Some(x)
  end Element

  type DropNames[T <: Tuple] = T match
    case Element[_, x] *: xs => x *: DropNames[xs]
    case _ => T

  extension [T <: Tuple](x: T) def dropNames: DropNames[T] =
    x.asInstanceOf // named and unnamed tuples have the same runtime representation
end NamedTuple
```
A named tuple element value like `name = value` is expanded by the compiler to `NamedTuple.Element.apply("name", value)`. A selection on a named tuple like `x.age` is expanded as follows: First, find at compile-time the index of the field of the tuple `x` that is of type `Element["age", T]`, for some type `T`. Say this index is `n`. Then, expand the selection `x.age` to `x(n).value`. The `value` method
is an extension method on named tuple `Element`s that strips the name from the type and returns just the value part.

A pattern match with a named tuple pattern like `age = x` and a selector value `s` translates to the `unapply` call `NamedTuple.Element.extract("age").unapply(s)`.

This translation of named tuples to opaque element types is fixed by the specification and therefore known to the programmer. This means that:

 - All tuple operations also work with named tuples "out of the box".
 - Macro libraries can rely on this expansion.

### Restrictions

The following restrictions apply to named tuple elements:

 1. Either all elements of a tuple are named or none are named. It is illegal to mix named and unnamed elements in a tuple. For instance, the following is in error:
    ```scala
    val illFormed1 = ("Bob", age = 33)  // error
    ```
 2. Each element name in a named tuple must be unique. For instance, the following is in error:
    ```scala
    val illFormed2 = (name = "", age = 0, name = true)  // error
    ```
 3. Named tuples can be matched with either named or regular patterns. But regular tuples and other selector types can only be matched with regular tuple patterns. For instance, the following is in error:
    ```scala
    (tuple: Tuple) match
        case (age = x) => // error
    ```

### Syntax

The syntax of Scala is extended as follows to support named tuples:
```
SimpleType        ::=  ...
                    |  ‘(’ NameAndType {‘,’ NameAndType} ‘)’
NameAndType       ::=  id ':' Type

SimpleExpr        ::=  ...
                    |  '(' NamedExprInParens {‘,’ NamedExprInParens} ')'
NamedExprInParens ::=  id '=' ExprInParens

SimplePattern     ::=  ...
                    |  '(' NamedPattern {‘,’ NamedPattern} ')'
NamedPattern      ::=  id '=' Pattern
```
