---
layout: doc-page
title: "Translation of Enums and ADTs"
---

The compiler expands enum classes and cases to code that only uses
Scala's other language features. As such, enums in Scala are
convenient _syntactic sugar_, but they are not essential to understand
Scala's core.

We now explain the expansion of enums is explained in detail. First,
some terminology and notational conventions:

 - We use `E` as a name of an enum class, and `C` as a name of an enum case that appears in the companion object of `E`.
 - We use `<...>` for syntactic constructs that in some circumstances might be empty. For instance `<body>` represents either the body of a case between `{...}` or nothing at all.

 - Enum cases fall into three categories:

   - _Class cases_ are those cases that are parameterized, either with a type parameter section `[...]` or with one or more (possibly empty) parameter sections `(...)`.
   - _Simple cases_ are cases of a non-generic enum class that have neither parameters nor an extends clause or body. That is, they consist of a name only.
   - _Value cases_ are all cases that do not have a parameter section but that do have a (possibly generated) extends clause and/or a body.

  Simple cases and value cases are called collectively _singleton cases_.

The desugaring rules imply that class cases are mapped to case classes, and singleton cases are mapped to `val` definitions.

There are eight desugaring rules. Rules (1) and (2) desugar enums and
enum classes. Rules (3) and (4) define extends clauses for cases that
are missing them. Rules (5 - 7) define how such expanded cases map
into case classes, case objects or vals. Finally, rule (8) expands
comma separated simple cases into a sequence of cases.

1.  An `enum` definition

         enum E ... { <cases> }

    expands to an enum class and a companion object

        enum class E ...
        object E { <cases> }

2. An enum class definition

       enum class E ... extends <parents> ...

    expands to a `sealed` `abstract` class that extends the `scala.Enum` trait:

       sealed abstract class E ... extends <parents> with scala.Enum ...

3. If `E` is an enum class without type parameters, then a case in its companion object without an extends clause

       case C <params> <body>

    expands to

       case C <params> <body> extends E

4. If `E` is an enum class with type parameters `Ts`, then a case in its
   companion object without an extends clause

       case C <params> <body>

   expands according to two alternatives, depending whether `C` has type
   parameters or not. If `C` has type parameters, they must have the same
   names and appear in the same order as the enum type parameters `Ts`
   (variances may be different, however). In this case

       case C [Ts] <params> <body>

   expands to

       case C[Ts] <params> extends E[Ts] <body>

   For the case where `C` does not have type parameters, assume `E`'s type
   parameters are

       V1 T1 > L1 <: U1 ,   ... ,    Vn Tn >: Ln <: Un      (n > 0)

   where each of the variances `Vi` is either `'+'` or `'-'`. Then the case
   expands to

       case C <params> extends E[B1, ..., Bn] <body>

   where `Bi` is `Li` if `Vi = '+'` and `Ui` if `Vi = '-'`. It is an error if
   `Bi` refers to some other type   parameter `Tj (j = 0,..,n-1)`. It is also
   an error if `E` has type parameters that are non-variant.

5. A class case

       case C <params> ...

   expands analogous to a case class:

       final case class C <params> ...

   However, unlike for a regular case class, the return type of the associated
   `apply` method is a fully parameterized type instance of the enum class `E`
   itself instead of `C`.  Also the enum case defines an `enumTag` method of
   the form

       def enumTag = n

   where `n` is the ordinal number of the case in the companion object,
   starting from 0.

6. A value case

       case C extends <parents> <body>

   expands to a value definition

       val C = new <parents> { <body>; def enumTag = n; $values.register(this) }

   where `n` is the ordinal number of the case in the companion object,
   starting from 0.  The statement `$values.register(this)` registers the value
   as one of the `enumValues` of the enumeration (see below). `$values` is a
   compiler-defined private value in the companion object.

7. A simple case

       case C

   of an enum class `E` that does not take type parameters expands to

       val C = $new(n, "C")

   Here, `$new` is a private method that creates an instance of of `E` (see
   below).

8. A simple case consisting of a comma-separated list of enum names

       case C_1, ..., C_n

   expands to

       case C_1; ...; case C_n

   Any modifiers or annotations on the original case extend to all expanded
   cases.

## Equality

An `enum` type contains a `scala.Eq` instance that restricts values of the `enum` type to
be compared only to other values of the same enum type. Furtermore, generic
`enum` types are comparable only if their type arguments are. For instance the
`Option` enum type will get the following definition in its companion object:

    implicit def eqOption[T, U](implicit ev1: Eq[T, U]): Eq[Option[T], Option[U]] = Eq

## Translation of Enumerations

Non-generic enum classes `E` that define one or more singleton cases
are called _enumerations_. Companion objects of enumerations define
the following additional members.

   - A method `enumValue` of type `scala.collection.immutable.Map[Int, E]`.
     `enumValue(n)` returns the singleton case value with ordinal number `n`.
   - A method `enumValueNamed` of type `scala.collection.immutable.Map[String, E]`.
     `enumValueNamed(s)` returns the singleton case value whose `toString`
     representation is `s`.
   - A method `enumValues` which returns an `Iterable[E]` of all singleton case
     values in `E`, in the order of their definitions.

Companion objects that contain at least one simple case define in addition:

   - A private method `$new` which defines a new simple case value with given
     ordinal number and name. This method can be thought as being defined as
     follows.

         def $new(tag: Int, name: String): ET = new E {
           def enumTag = tag
           def toString = name
           $values.register(this)   // register enum value so that `valueOf` and `values` can return it.
         }
