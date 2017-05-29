---
layout: doc-page
title: "Programmatic Structural Types"
---

Previously, Scala supported structural types by means of
reflection. This is problematic on other platforms, because Scala's
reflection is JVM-based. Consequently, Scala.js and Scala.native don't
support structural types fully. The reflction based implementation is
also needlessly restrictive, since it rules out other implementation
schemes. This makes structural types unsuitable for e.g. modelling
rows in a database, for which they would otherwise seem to be an ideal
match.

Dotty allows to implement structural types programmatically, using
"Selectables". `Selectable` is a trait defined as follows:

    trait Selectable extends Any {
      def selectDynamic(name: String): Any
      def selectDynamicMethod(name: String, paramClasses: ClassTag[_]*): Any =
        new UnsupportedOperationException("selectDynamicMethod")
    }

The most important method of a `Selectable` is `selectDynamic`: It
takes a field name and returns the value associated with that name in
the selectable.

Assume now `r` is a value with structural type `S`.` In general `S` is
of the form `C { Rs }`, i.e. it consists of a class reference `C` and
refinement declarations `Rs`. We call a field selection `r.f`
_structural_ if `f` is a name defined by a declaration in `Rs` whereas
`C` defines no member of name `f`. Assuming the selection has type
`T`, it is mapped to something equivalent to the following code:

    (r: Selectable).selectDynamic("f").asInstanceOf[T]

That is, we make sure `r` conforms to type `Selectable`, potentially
by adding an implicit conversion. We then invoke the `get` operation
of that instance, passing the the name `"f"` as a parameter. We
finally cast the resulting value back to the statically known type
`T`.

`Selectable` also defines another access method called
`selectDynamicMethod`. This operation is used to select methods
instead of fields. It gets passed the class tags of the selected
method's formal parameter types as additional arguments. These can
then be used to disambiguate one of several overloaded variants.

Package `scala.reflect` contains an implicit conversion which can map
any value to a selectable that emulates reflection-based selection, in
a way similar to what was done until now:

    package scala.reflect

    object Selectable {
      implicit def reflectiveSelectable(receiver: Any): scala.Selectable =
        receiver match {
          case receiver: scala.Selectable => receiver
          case _ => new scala.reflect.Selectable(receiver)
        }
    }

When imported, `reflectiveSelectable` provides a way to access fields
of any structural type using Java reflection. This is similar to the
current implementation of structural types. The main difference is
that to get reflection-based structural access one now has to add an
import:

    import scala.relect.Selectable.reflectiveSelectable

On the other hand, the previously required language feature import of
`reflectiveCalls` is now redundant and is therefore dropped.

As you can see from its implementation above, `reflectSelectable`
checks first whether its argument is already a run-time instance of
`Selectable`, in which case it is returned directly. This means that
reflection-based accesses only take place as a last resort, if no
other `Selectable` is defined.

Other selectable instances can be defined in libraries. For instance,
here is a simple class of records that support dynamic selection:

    case class Record(elems: (String, Any)*) extends Selectable {
      def selectDynamic(name: String): Any = elems.find(_._1 == name).get._2
    }

`Record` consists of a list of pairs of element names and values. Its
`selectDynamic` operation finds the pair with given name and returns
its value.

For illustration, let's define a record value and cast it to a
structural type `Person`:

    type Person = Record { val name: String; val age: Int }
    val person = Record(("name" -> "Emma", "age" -> 42)).asInstanceOf[Person]

Then `person.name` will have static type `String`, and will produce `"Emma"` as result.

The safety of this scheme relies on the correctness of the cast. If
the cast lies about the structure of the record, the corresponding
`selectDynamic` operation would fail.  In practice, the cast would
likely be part if a database access layer which would ensure its
correctness.

## Notes:

1. The scheme does not handle polymorphic methods in structural
refinements. Such polymorphic methods are currently flagged as
errors. It's not clear whether the use case is common enough to
warrant the additional complexity of supporting it.

2. There are clearly some connections with `scala.Dynamic` here, since
both select members programmatically. But there are also some
differences.

 - Fully dynamic selection is not typesafe, but structural selection
   is, as long as the correspondence of the structural type with the
   underlying value is as stated.

 - `Dynamic` is just a marker trait, which gives more leeway where and
   how to define reflective access operations. By contrast
   `Selectable` is a trait which declares the access operations.

 - One access operation, `selectDynamic` is shared between both
   approaches, but the other access operations are
   different. `Selectable` defines a `selectDynamicMethod`, which
   takes class tags indicating the method's formal parameter types as
   additional argument. `Dynamic` comes with `applyDynamic` and
   `updateDynamic` methods, which take actual argument values.
