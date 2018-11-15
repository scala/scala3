---
layout: doc-page
title: Changed: Lazy Vals - More Details
---

Dotty implements [Version 6](https://docs.scala-lang.org/sips/improved-lazy-val-initialization.html#version-6---no-synchronization-on-this-and-concurrent-initialization-of-fields)
of the [SIP-20] improved lazy vals initialization proposal.

## Motivation

The newly proposed lazy val initialization mechanism aims to eliminate the acquisition of resources
during the execution of the lazy val initializer block, thus reducing the possibility of a deadlock.
The concrete deadlock scenarios that the new lazy val initialization scheme eliminates are
summarized in the [SIP-20] document.

## Implementation

Given a lazy field of the form:

```scala
class Foo {
  @volatile lazy val bar = <RHS>
}
```

The Dotty compiler will generate code equivalent to:

```scala
class Foo {
  import dotty.runtime.LazyVals
  var value_0: Int = _
  var bitmap = 0
  val bitmap_offset = LazyVals.getOffset(classOf[LazyCell], "bitmap")

  def bar(): Int = {
    var result: Int = 0
    var retry: Boolean = true
    var flag: Long = 0L
    while (retry) {
      flag = LazyVals.get(this, bitmap_offset)
      LazyVals.STATE(flag, <field-id>) match {
        case <state-0> =>
          if (LazyVals.CAS(this, bitmap_offset, flag, <state-1>)) {
            try result = <RHS>
            catch {
              case ex =>
                LazyVals.setFlag(this, bitmap_offset, <state-0>, <field-id>)
                throw ex
            }
            value_0 = result
            LazyVals.setFlag(this, bitmap_offset, <state-3>, <field-id>)
            retry = false
          }
        case <state-1> | <state-2> =>
          LazyVals.wait4Notification(this, bitmap_offset, flag, <field-id>)
        case <state-3> =>
          retry = false
          result = value_0
        }
      }
    result
  }
}
```

The state of the lazy val `<state-i>` is represented with 4 values: 0, 1, 2 and 3. The state 0
represents a non-initialized lazy val. The state 1 represents a lazy val that is currently being
initialized by some thread. The state 2 denotes that there are concurrent readers of the lazy val.
The state 3 represents a lazy val that has been initialized. `<field-id>` is the id of the lazy
val. This id grows with the number of volatile lazy vals defined in the class.

## Note on recursive lazy vals

Ideally recursive lazy vals should be flagged as an error. The current behavior for `@volatile`
recursive lazy vals is undefined (initialization may result in a deadlock). Non `@volatile` lazy
vals behave as in Scala 2.

## Reference

* [SIP-20]

[SIP-20]: https://docs.scala-lang.org/sips/improved-lazy-val-initialization.html
