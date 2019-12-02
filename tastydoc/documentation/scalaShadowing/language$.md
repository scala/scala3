scalaShadowing
# object language

<pre><code class="language-scala" >final object language extends Serializable</pre></code>
The `scala.language` object controls the language features available to the programmer, as proposed in the
[**SIP-18 document**](https://docs.google.com/document/d/1nlkvpoIRkx7at1qJEZafJwthZ3GeIklTFhqmXMvTX9Q/edit).
Each of these features has to be explicitly imported into the current scope to become available:
```scala
import language.postfixOps // or language._
List(1, 2, 3) reverse
```
The language features are:
* [`dynamics`](../scalaShadowing/language$.md#dynamics)            enables defining calls rewriting using the `Dynamic` trait
* [`postfixOps`](../scalaShadowing/language$.md#postfixOps)          enables postfix operators
* [`reflectiveCalls`](../scalaShadowing/language$.md#reflectiveCalls)     enables using structural types
* [`implicitConversions`](../scalaShadowing/language$.md#implicitConversions) enables defining implicit methods and members
* [`higherKinds`](../scalaShadowing/language$.md#higherKinds)         enables writing higher-kinded types
* [`existentials`](../scalaShadowing/language$.md#existentials)        enables writing existential types
* `experimental`        contains newer features that have not yet been tested in production

and, for dotty:
* `Scala2`]               backwards compatibility mode for Scala2
* `noAutoTupling`       disable auto-tupling
* `strictEquality`      enable strick equality

***production*** Language Features

***experimental*** Experimental Language Features

***experimental*** 10
Dotty-specific features come at the end.
Note: Due to the more restricted language import mechanism in dotty (only
imports count, implicits are disregarded) we don't need the constructions
of the inherited language features. A simple object for each feature is
sufficient.

## Concrete Type Members:
### Scala2
<pre><code class="language-scala" >final object <a href="./language$/Scala2$.md">Scala2</a></pre></code>
Where imported, a backwards compatibility mode for Scala2 is enabled

### experimental
<pre><code class="language-scala" >final object <a href="./language$/experimental$.md">experimental</a></pre></code>
The experimental object contains features that have been recently added but have not
been thoroughly tested in production yet.
Experimental features **may undergo API changes** in future releases, so production
code should not rely on them.
Programmers are encouraged to try out experimental features and
[report any bugs or API inconsistencies](http://issues.scala-lang.org)
they encounter so they can be improved in future releases.

***Group*** experimental

### noAutoTupling
<pre><code class="language-scala" >final object <a href="./language$/noAutoTupling$.md">noAutoTupling</a></pre></code>
Where imported, auto-tupling is disabled

### strictEquality
<pre><code class="language-scala" >final object <a href="./language$/strictEquality$.md">strictEquality</a></pre></code>
Where imported loose equality using eqAny is disabled

## Concrete Value Members:
### dynamics
<pre><code class="language-scala" >@volatile implicit val dynamics: dynamics</pre></code>
Where enabled, direct or indirect subclasses of trait scala.Dynamic can
be defined. Unless dynamics is enabled, a definition of a class, trait,
or object that has Dynamic as a base trait is rejected. Dynamic member
selection of existing subclasses of trait Dynamic are unaffected;
they can be used anywhere.
**Why introduce the feature?** To enable flexible DSLs and convenient interfacing
with dynamic languages.
**Why control it?** Dynamic member selection can undermine static checkability
of programs. Furthermore, dynamic member selection often relies on reflection,
which is not available on all platforms.

***Group*** production


### existentials
<pre><code class="language-scala" >@volatile implicit val existentials: existentials</pre></code>
Only where enabled, existential types that cannot be expressed as wildcard
types can be written and are allowed in inferred types of values or return
types of methods. Existential types with wildcard type syntax such as `List[_]`,
or `Map[String, _]` are not affected.
**Why keep the feature?** Existential types are needed to make sense of Java’s wildcard
types and raw types and the erased types of run-time values.
**Why control it?** Having complex existential types in a code base usually makes
application code very brittle, with a tendency to produce type errors with
obscure error messages. Therefore, going overboard with existential types
is generally perceived not to be a good idea. Also, complicated existential types
might be no longer supported in a future simplification of the language.

***Group*** production


### higherKinds
<pre><code class="language-scala" >@volatile implicit val higherKinds: higherKinds</pre></code>
Only where this flag is enabled, higher-kinded types can be written.
**Why keep the feature?** Higher-kinded types enable the definition of very general
abstractions such as functor, monad, or arrow. A significant set of advanced
libraries relies on them. Higher-kinded types are also at the core of the
scala-virtualized effort to produce high-performance parallel DSLs through staging.
**Why control it?** Higher kinded types in Scala lead to a Turing-complete
type system, where compiler termination is no longer guaranteed. They tend
to be useful mostly for type-level computation and for highly generic design
patterns. The level of abstraction implied by these design patterns is often
a barrier to understanding for newcomers to a Scala codebase. Some syntactic
aspects of higher-kinded types are hard to understand for the uninitiated and
type inference is less effective for them than for normal types. Because we are
not completely happy with them yet, it is possible that some aspects of
higher-kinded types will change in future versions of Scala. So an explicit
enabling also serves as a warning that code involving higher-kinded types
might have to be slightly revised in the future.

***Group*** production


### implicitConversions
<pre><code class="language-scala" >@volatile implicit val implicitConversions: implicitConversions</pre></code>
Only where enabled, definitions of legacy implicit conversions and certain uses
of implicit conversions are allowed.
A legacy implicit conversion is an implicit value of unary function type `A => B`,
or an implicit method that has in its first parameter section a single,
non-implicit parameter. Examples:
```scala
implicit def stringToInt(s: String): Int = s.length
implicit val conv = (s: String) => s.length
implicit def listToX(xs: List[T])(implicit f: T => X): X = ...
```
Implicit values of other types are not affected, and neither are implicit
classes. In particular, delegates of the scala.Conversion class can be
defined without having to import the language feature.
The language import is also required to enable _uses_ of implicit conversions
unless the conversion in question is co-defined with the type to which it maps.
Co-defined means: defined in the companion object of the class of the result type.
Examples:
```scala
class A
class B
object B {
  delegate a2b for Conversion[A, B] { ... }
}
object C {
  delegate b2a for Conversion[B, A] { ... }
}
import delegate B._
import delegate C._
val x: A = new B     // language import required
val x: B = new A     // no import necessary since a2b is co-defined with B
```
**Why keep the feature?** Implicit conversions are central to many aspects
of Scala’s core libraries.
**Why control it?** Implicit conversions are known to cause many pitfalls
if over-used. This holds in particular for implicit conversions defined after
the fact between unrelated types.

***Group*** production


### postfixOps
<pre><code class="language-scala" >@volatile implicit val postfixOps: postfixOps</pre></code>
Only where enabled, postfix operator notation `(expr op)` will be allowed.
**Why keep the feature?** Several DSLs written in Scala need the notation.
**Why control it?** Postfix operators interact poorly with semicolon inference.
 Most programmers avoid them for this reason.

***Group*** production


### reflectiveCalls
<pre><code class="language-scala" >@volatile implicit val reflectiveCalls: reflectiveCalls</pre></code>
Only where enabled, accesses to members of structural types that need
reflection are supported. Reminder: A structural type is a type of the form
`Parents { Decls }` where `Decls` contains declarations of new members that do
not override any member in `Parents`. To access one of these members, a
reflective call is needed.
**Why keep the feature?** Structural types provide great flexibility because
they avoid the need to define inheritance hierarchies a priori. Besides,
their definition falls out quite naturally from Scala’s concept of type refinement.
**Why control it?** Reflection is not available on all platforms. Popular tools
such as ProGuard have problems dealing with it. Even where reflection is available,
reflective dispatch can lead to surprising performance degradations.

***Group*** production


