# Inline Traits

## Motivation
Inline traits is a new attempt to solve the specialization problem for the JVM in a more convenient way than the `@specialized` annotation
from Scala 2 (the main problem of this being code bloat as we generated all possible specializations at declaration time of the specialized
class). Inline traits work alongside  `Specialized` traits, the latter being detailed in an accompanying document. 

The problem is as follows: due to the JVM's (lack of) support for generics, generic type parameters are erased by the compiler:
```scala
class A[T](val x: T)

class C:
    val w1 = A[Int](1)
    val w2 = A[Int](2)
    val w3 = A[Int](w1.x + w2.x)
```
Is converted to:
```scala
class A:
    val x: Object
class C() extends Object() {
    def w1(): A = new A(Int.box(1))
    def w2(): A = new A(Int.box(2))
    def w3(): A = new A(Int.box(Int.unbox(this.w1().x()).+(Int.unbox(this.w2().x()))))
}
```
Thus type `T` is converted to `Object`, and so every use of `A[Int]()` must first build an `Object` containing the `Int` we want to pass (*boxing*),
in order to be able to call `A(x: Object)`. There is no `A(x: int)`. When referencing `x`  on an `A` we get an `Object` back, which we have to *unbox*
(extract from the wrapping `Object`).

This object creation and deletion is very slow. We desire a way to avoid this by generating specialized instances of classes which
use primitive types instead of `Object`. These can be used in situations where the (un)boxing overhead is likely to be high.

## Solution
An inline trait is defined just like a normal trait, but with an `inline` modifier.

Inline traits may be extended by objects, classes or other inline traits, *but not by ordinary traits*.

The following is an example of the use of an `inline trait`.

```scala
inline trait A[T](val x: T):
    def foo: T = x

class B extends A[Int](1)
```
Let the term *inline trait* refer to traits such as `A` above, and let *inline receiver* refer to class-likes 
we inline into, such as `B` above.

When an inline trait is inherited by an object, class or another inline trait, all its contents are inlined, and adapted
to the context of the inline receiver. In particular this means that:
- references to type parameters of the inline trait  are specialized to the type arguments provided during extension
- `this` calls are updated to refer to the inline receiver.

Inline traits are themselves translated to pure interfaces. However their bodies are of course retained in Tasty files; this
enables us to inline them into inline receivers that exist in different compilation units (for example when an inline recevier
in user code extends an inline trait from a library).

The example above generates:
```scala
// Inline trait converted to pure interface
inline trait A[T](x: T):
    val x: T
    def foo: T

// Extending class now contains inlined body
// with references to T specialized to Int.
// this.x refers to B.x
class B extends A[Int](1):
    override val x: Int = 1
    override def foo: Int = this.x
```

With multiple inline traits:

```scala
inline trait A[T](val x: T):
    def foo: T = x

inline trait B extends A[Int]
class C extends B, A[Int](1)
```

```scala
inline trait A[T](x: T):
    val x: T
    def foo: T

inline trait B extends A[Int]:
    override def x: Int
    override def foo: Int

class C extends B, A[Int](1):
    override def x = 1
    override def foo = x
```
While it is not immediately obvious why permitting inline traits to be inlined into other inline traits is useful (we could simply
inline everything into the first class/object in the hierarchy, it becomes advantageous when we bring in the `Specialized` annotation; see the accompanying document).


Furthermore:
- References to members of inline traits accessed on inline receivers point to the inlined version, to ensure we avoid unnecessary boxing: [1]
```scala
inline trait A[T](val x: T):
    def foo#1: T

class B extends A[Int](1)
    def foo#2: T = x

def fun(x: B) = 
    x.foo // points to foo#2 
```
- Inline traits may define private members, and these are handled specially: [2]
    - Private fields in the inline trait are inlined as private fields with a mangled name in the inline receiver. This ensures they do not collide with privates inherited from other inline traits.
    - The private fields are then no longer accessible in the inline trait, as it is transformed into a pure interface. We can't however easily delete them; therefore they are name-mangled and converted to protected to allow them to exist without a definition.   

```scala
inline trait A(b: Boolean):
    private val x: Int = 1
    def foo(): Int = if b then x + 1 else 0

class B extends A(true)
```
Is converted to:

```scala
inline trait A(b: Boolean):
    protected val x$inline_trait_erased_private#4481: Int
    def foo(): Int

class B extends A(true):
    private val A$$b: Boolean = true
    private val A$$x: Int = 1
    override def foo(): Int = if this.A$$b then this.A$$x.+(1) else 0
```
- An inline receiver may mix in multiple inline traits with colliding member names. In this case the latest extended trait prevails. In the following example calling `foo` on an instance of `C` will return "Bonjour". This is in contrast to ordinary traits which require the `override` modifier in this case. <!-- TODO: Is this ok? We do it because otherwise they would have to write override on all members in Specialized traits because we mix those in multiple times. I suppose we could add this ourselves? Maybe that's better. -->
```scala
inline trait A:
    def foo = "Hello World"

inline trait B:
    def foo = "Bonjour"

class C extends A, B
```
However, an inline receiver may not define a member whose name collides with the name of an inlined public member from a parent inline trait, unless the override modifier is used. This reflects the behaviour of ordinary traits.
```scala
inline trait A:
    def foo = "Hello World"

inline trait B:
    def foo = "Bonjour"

class C extends A, B:
    def foo = "Bonjour2" // Must be override.
```
- Inline receivers may not access the parameters of their parents (these are private):
```scala
inline trait A(x: Int)

class C extends A(10):
  val y = x // error: Not Found Error
```

- Inlined members of inline traits are typed with the type of the right hand side resulting from inlining. This is particularly important for typeclass instances:
```scala
inline trait A[T: Numeric]:
    private val v: Numeric[T] = summon[Numeric[T]]

class B extends A[Int]
```
Is converted to:
```scala
inline trait A[T: Numeric]:
    private val v: Numeric[T] = summon[Numeric[T]]

class B extends A[Int]:
    private given val A$$evidence$1: Numeric.IntIsIntegral.type =  Numeric.IntIsIntegral
    private val A$$v: Numeric.IntIsIntegral = this.A$$evidence$1
``` 
This means that references to `v.fromInt()`, `v.add()` etc are optimised and avoid boxing. However, this type acquisition is only applied to non-var members, as it could
lead to unsoundness if applied to `var`s:

```scala
inline trait Counter extends Iterator:
  private var current: Int = 0
  def next(): Int = current += 1
```
Narrowing `current` to the type of the initializer here would give it type `0`. This makes the increment operation in `next()` illegal.

## Benefits of inline traits
We can now do the following with no boxing and unboxing:
```scala
inline trait A[T](val x: T)
class IntA(x: Int) extends A[Int](x)

class C:
    val w1 = IntA(1)
    val w2 = IntA(2)
    val w3 = IntA(w1.x + w2.x)
```

Inline traits avoid all of the pain points of Scala 2 specialization. They can do more than primitive specialization since they also specialize on value parameters and reference types. This helps avoid megamorphic dispatch. Inline traits also profit from all the optimizations available for inline methods, including inline matches, summonFrom, embedded splices. Indeed the analogy of inline traits and inline methods is strong: inline calls correspond to supercalls in extending classes and objects, inline parameters are inline parameters of the trait, inline traits can have nested inline matches, etc.

Inline trait expansions are only generated on demand when a class or object extends an inline trait. This avoids the up-front cost and code explosion due to creating specialized copies which might never be needed.

## Shortcoming of inline traits
Compared to full specialization, inline traits have one shortcoming, namely that interfaces are not specialized. For example: 

```scala
inline trait Foo[T](x: T):
    def foo = x

class Bar extends Foo[Int](42)

def f(b: Foo[Int]) = 37 + b.foo
 
@main def main =
    val x = Bar()
    f(x)
```

In this code the call to `b.foo` will refer to the version of `foo` typed `foo: T` which becomes `foo: Object` during erasure, becasue we accessed `foo` on
an object of declared type `Foo` (even though `b`'s actual runtime type is `Bar`). This will in turn call a bridge method which means the `foo: Int` method will be called, but unnecessary boxing and unboxing will be added:

```scala
inline trait Foo
    def foo#1(): Object

class Bar extends Foo[Int](42):
    override def foo#2(): Int = 42
    override def foo#3(): Object = Int.box(this.foo())

def f(b: Foo): Int = 37 + Int.unbox(b.foo#1()) // virtual call to foo#1 resolves to bridge method foo#3, which in turn calls actual method foo#2, with boxing. 
```

This problem is addressed via `Specialized` traits; see the accompanying document on those.

## Interaction with other language features

| Language feature         | Is currently supported inside inline traits? |
|--------------------------|----------------------------------------------|
| Methods                  | ✅                                           |
| `val` / `var` Properties | ✅                                           |
| Private properies |              ❌                              | <!-- TODO: Surely this is less than ideal - see inline-trait-body-private-name-collision.scala -->
| `type`s                  | ✅                                           |
| Inner classes/traits            | ❌                                    |
| Opaque types             | ❌                                           |
| Self types               | ❌                                           |
| Inheritance (of inline traits) | Only allowed by classes and inline traits | 

## Processing of inline traits in the compiler
Inline traits in user code are inlined in the phase `specializeInlineTraits`. The phase `replaceInlinedTraitSymbols`
is responsible for updating references to members of inline receivers to point to the inlined members, instead of the
generic members in the parent inline trait (see [1] above). Finally the phase `pruneInlineTraits` is responsible for 
converting inline traits into pure interfaces by removing their right hand sides. It also handles the mangling in [2]. 

Specialized traits rely on the semantics of inline traits, as they desugar to inline traits. However, the phase
`desugarSpecializedTraits` inlines these inline traits itself (sharing code with the `specializeInlineTraits` phase).
This is necessary because otherwise there would be a circular dependency between the two phases (see the Specialized traits
document for more information). This means that we need to run `specializeInlineTraits` *first* (because we don't want to inline
twice for inline traits resulting from specialization). 

## Internal Note regarding versions of inline traits
This behaviour is the same as that in Timothée's thesis except for the following points:
 - We now allow inline traits to be inlined directly into other inline traits as well as objects and classes.
 - We now do replacement of member accesses to point to the inlined versions throughout the whole code, not just in the bodies of inner classes
 - He allows inline traits to contain inner classes in principle, however in practice they don't work which is why we ban them.
 - We specialize types of member accesses on e.g. Numeric
 - He in principle allows traits to extend inline traits although it doesn't work that well; we think we probably want to forbid this.
