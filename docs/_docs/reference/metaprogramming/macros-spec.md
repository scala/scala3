---
layout: doc-page
title: "Macros Spec"
nightlyOf: https://docs.scala-lang.org/scala3/reference/metaprogramming/macros-spec.html
---

## Formalization

* Multi-stage programming with generative and analytical macros[^2]
* Multi-Stage Macro Calculus, Chapter 4 of Scalable Metaprogramming in Scala 3[^1].
  Contains and extends the calculus of _Multi-stage programming with generative and analytical macros_ with type polymorphism.

## Syntax

The quotation syntax using `'` and `$` was chosen to mimic the string interpolation syntax of Scala.
Like a string double-quotation, a single-quote block can contain splices.
However, unlike strings, splices can contain quotes using the same rules.

```scala
s" Hello $name"           s" Hello ${name}"
'{ hello($name) }         '{ hello(${name}) }
${ hello('name) }         ${ hello('{name}) }
```

### Quotes
Quotes come in four flavors: quoted identifiers, quoted blocks, quoted block patterns and quoted type patterns.
Scala 2 used quoted identifiers to represent `Symbol` literals. They were deprecated in Scala 3, allowing the syntax to be used for quotation.
```scala
SimpleExpr ::= ...
             |  `'` alphaid                           // quoted identifier
             |  `'` `{` Block `}`                     // quoted block
Pattern    ::= ...
             |  `'` `{` Block `}`                     // quoted block pattern
             |  `'` `[` Type `]`                      // quoted type pattern
```

Quoted blocks and quoted block patterns contain an expression equivalent to a normal block of code.
When entering either of those we track the fact that we are in a quoted block (`inQuoteBlock`) which is used for spliced identifiers.
When entering a quoted block pattern we additionally track the fact that we are in a quoted pattern (`inQuotePattern`) which is used to distinguish spliced blocks and splice patterns.
Lastly, the quoted type pattern simply contains a type.

### Splices
Splices come in three flavors: spliced identifiers, spliced blocks and splice patterns.
Scala specifies identifiers containing `$` as valid identifiers but reserves them for compiler and standard library use only.
Unfortunately, many libraries have used such identifiers in Scala 2. Therefore to mitigate the cost of migration, we still support them.
We work around this by only allowing spliced identifiers[^3] within quoted blocks or quoted block patterns (`inQuoteBlock`).
Splice blocks and splice patterns can contain an arbitrary block or pattern respectively.
They are distinguished based on their surrounding quote (`inQuotePattern`), a quote block will contain spliced blocks, and a quote block pattern will contain splice patterns.

```scala
SimpleExpr ::= ...
             |  `$` alphaid         if  inQuoteBlock    // spliced identifier
             |  `$` `{` Block `}`   if !inQuotePattern  // spliced block
             |  `$` `{` Pattern `}` if  inQuotePattern  // splice pattern
```

### Quoted Pattern Type Variables
Quoted pattern type variables in quoted patterns and quoted type patterns do not require additional syntax.
Any type definition or reference with a name composed of lower cases is assumed to be a pattern type variable definition while typing.
A backticked type name with lower cases is interpreted as a reference to the type with that name.


## Implementation

### Run-Time Representation

The standard library defines the `Quotes` interface which contains all the logic and the abstract classes `Expr` and `Type`.
The compiler implements the `Quotes` interface and provides the implementation of `Expr` and `Type`.

##### `class Expr`
Expressions of type `Expr[T]` are represented by the following abstract class:
```scala
abstract class Expr[+T] private[scala]
```
The only implementation of `Expr` is in the compiler along with the implementation of `Quotes`.
It is a class that wraps a typed AST and a `Scope` object with no methods of its own.
The `Scope` object is used to track the current splice scope and detect scope extrusions.

##### `object Expr`
The companion object of `Expr` contains a few useful static methods;
the `apply`/`unapply` methods to use `ToExpr`/`FromExpr` with ease;
the `betaReduce` and `summon` methods.
It also contains methods to create expressions out of lists or sequences of expressions: `block`, `ofSeq`, `ofList`, `ofTupleFromSeq` and `ofTuple`.

```scala
object Expr:
  def apply[T](x: T)(using ToExpr[T])(using Quotes): Expr[T] = ...
  def unapply[T](x: Expr[T])(using FromExpr[T])(using Quotes): Option[T] = ...
  def betaReduce[T](e: Expr[T])(using Quotes): Expr[T] = ...
  def summon[T: Type](using Quotes): Option[Expr[T]] = ...
  def block[T](stats: List[Expr[Any]], e: Expr[T])(using Quotes): Expr[T] = ...
  def ofSeq[T: Type](xs: Seq[Expr[T]])(using Quotes): Expr[Seq[T]] = ...
  def ofList[T: Type](xs: Seq[Expr[T]])(using Quotes): Expr[List[T]] = ...
  def ofTupleFromSeq(xs: Seq[Expr[Any]])(using Quotes): Expr[Tuple] = ...
  def ofTuple[T <: Tuple: Tuple.IsMappedBy[Expr]: Type](tup: T)(using Quotes):
      Expr[Tuple.InverseMap[T, Expr]] = ...
```

##### `class Type`
Types of type `Type[T]` are represented by the following abstract class:
```scala
abstract class Type[T <: AnyKind] private[scala]:
  type Underlying = T
```

The only implementation of `Type` is in the compiler along with the implementation of `Quotes`.
It is a class that wraps the AST of a type and a `Scope` object with no methods of its own.
The upper bound of `T` is `AnyKind` which implies that `T` may be a higher-kinded type.
The `Underlying` alias is used to select the type from an instance of `Type`.
Users never need to use this alias as they can always use `T` directly.
`Underlying` is used for internal encoding while compiling the code (see _Type Healing_).

##### `object Type`
The companion object of `Type` contains a few useful static methods.
The first and most important one is the `Type.of` given definition.
This instance of `Type[T]` is summoned by default when no other instance is available.
The `of` operation is an intrinsic operation that the compiler will transform into code that will generate the `Type[T]` at run-time.
Secondly, the `Type.show[T]` operation will show a string representation of the type, which is often useful when debugging.
Finally, the object defines `valueOfConstant` (and `valueOfTuple`) which can transform singleton types (or tuples of singleton types) into their value.


```scala
object Type:
  given of: [T <: AnyKind] => Quotes => Type[T] = ...
  def show[T <: AnyKind](using Type[T])(using Quotes): String = ...
  def valueOfConstant[T](using Type[T])(using Quotes): Option[T] = ...
  def valueOfTuple[T <: Tuple](using Type[T])(using Quotes): Option[T] = ...
```

##### `Quotes`
The `Quotes` interface is where most of the primitive operations of the quotation system are defined.

Quotes define all the `Expr[T]` methods as extension methods.
`Type[T]` does not have methods and therefore does not appear here.
These methods are available as long as `Quotes` is implicitly given in the current scope.

The `Quotes` instance is also the entry point to the [reflection API](./reflection.md) through the `reflect` object.

Finally, `Quotes` provides the internal logic used in quote un-pickling (`QuoteUnpickler`) in quote pattern matching (`QuoteMatching`).
These interfaces are added to the self-type of the trait to make sure they are implemented on this object but not visible to users of `Quotes`.

Internally, the implementation of `Quotes` will also track its current splicing scope `Scope`.
This scope will be attached to any expression that is created using this `Quotes` instance.

```scala
trait Quotes:
  this: runtime.QuoteUnpickler & runtime.QuoteMatching =>

  extension [T](self: Expr[T])
    def show: String
    def matches(that: Expr[Any]): Boolean
    def value(using FromExpr[T]): Option[T]
    def valueOrAbort(using FromExpr[T]): T
  end extension

  extension (self: Expr[Any])
    def isExprOf[X](using Type[X]): Boolean
    def asExprOf[X](using Type[X]): Expr[X]
  end extension

  // abstract object reflect ...
```


##### `Scope`
The splice context is represented as a stack (immutable list) of `Scope` objects.
Each `Scope` contains the position of the splice (used for error reporting) and a reference to the enclosing splice scope `Scope`.
A scope is a sub-scope of another if the other is contained in its parents.
This check is performed when an expression is spliced into another using the `Scope` provided in the current scope in `Quotes` and the one in the `Expr` or `Type`.

### Entry Points
The two entry points for multi-stage programming are macros and the `run` operation.

#### Macros
Inline macro definitions will inline a top-level splice (a splice not nested in a quote).
This splice needs to be evaluated at compile-time.
In _Avoiding a complete interpreter_[^1], we stated the following restrictions:

 * The top-level splice must contain a single call to a compiled static method.
 * Arguments to the function are either literal constants, quoted expressions (parameters), `Type.of` for type parameters and a reference to `Quotes`.

These restrictions make the implementation of the interpreter quite simple.
Java Reflection is used to call the single function call in the top-level splice.
The execution of that function is entirely done on compiled bytecode.
These are Scala static methods and may not always become Java static methods, they might be inside module objects.
As modules are encoded as class instances, we need to interpret the prefix of the method to instantiate it before we can invoke the method.

The code of the arguments has not been compiled and therefore needs to be interpreted by the compiler.
Interpreting literal constants is as simple as extracting the constant from the AST that represents literals.
When interpreting a quoted expression, the contents of the quote is kept as an AST which is wrapped inside the implementation of `Expr`.
Calls to `Type.of[T]` also wrap the AST of the type inside the implementation of `Type`.
Finally, the reference to `Quotes` is supposed to be the reference to the quotes provided by the splice.
This reference is interpreted as a new instance of `Quotes` that contains a fresh initial `Scope` with no parents.

The result of calling the method via Java Reflection will return an `Expr` containing a new AST that was generated by the implementation of that macro.
The scope of this `Expr` is checked to make sure it did not extrude from some splice or `run` operation.
Then the AST is extracted from the `Expr` and it is inserted as replacement for the AST that contained the top-level splice.


#### Run-time Multi-Stage Programming

To be able to compile the code, the `scala.quoted.staging` library defines the `Compiler` trait.
An instance of `staging.Compiler` is a wrapper over the normal Scala~3 compiler.
To be instantiated it requires an instance of the JVM _classloader_ of the application.

```scala
import scala.quoted.staging.*
given Compiler = Compiler.make(getClass.getClassLoader)
```

The classloader is needed for the compiler to know which dependencies have been loaded and to load the generated code using the same classloader. Below is an example method `mkPower2` that is passed to `staging.run`:

```scala
def mkPower2()(using Quotes): Expr[Double => Double] = ...

run(mkPower2())
```
To run the previous example, the compiler will create code equivalent to the following class and compile it using a new `Scope` without parents.

```scala
class RunInstance:
  def exec(): Double => Double = ${ mkPower2() }
```
Finally, `run` will interpret `(new RunInstance).exec()` to evaluate the contents of the quote.
To do this, the resulting `RunInstance` class is loaded in the JVM using Java Reflection, instantiated and then the `exec` method is invoked.


### Compilation

Quotes and splices are primitive forms in the generated typed abstract syntax trees.
These need to be type-checked with some extra rules, e.g., staging levels need to be checked and the references to generic types need to be adapted.
Finally, quoted expressions that will be generated at run-time need to be encoded (serialized/pickled) and decoded (deserialized/unpickled).

#### Typing Quoted Expressions

The typing process for quoted expressions and splices with `Expr` is relatively straightforward.
At its core, quotes are desugared into calls to `quote`, splices are desugared into calls to `splice`.
We track the quotation level when desugaring into these methods.


```scala
def quote[T](x: T): Quotes ?=> Expr[T]

def splice[T](x: Quotes ?=> Expr[T]): T
```

It would be impossible to track the quotation levels if users wrote calls to these methods directly.
To know if it is a call to one of those methods we would need to type it first, but to type it we would need to know if it is one of these methods to update the quotation level.
Therefore these methods can only be used by the compiler.

At run-time, the splice needs to have a reference to the `Quotes` that created its surrounding quote.
To simplify this for later phases, we track the current `Quotes` and encode a reference directly in the splice using `nestedSplice` instead of `splice`.

```scala
def nestedSplice[T](q: Quotes)(x: q.Nested ?=> Expr[T]): T
```
With this addition, the original `splice` is only used for top-level splices.

The levels are mostly used to identify top-level splices that need to be evaluated while typing.
We do not use the quotation level to influence the typing process.
Level checking is performed at a later phase.
This ensures that a source expression in a quote will have the same elaboration as a source expression outside the quote.



#### Quote Pattern Matching

Pattern matching is defined in the trait `QuoteMatching`, which is part of the self type of `Quotes`.
It is implemented by `Quotes` but not available to users of `Quotes`.
To access it, the compiler generates a cast from `Quotes` to `QuoteMatching` and then selects one of its two members: `ExprMatch` or `TypeMatch`.
`ExprMatch` defines an `unapply` extractor method that is used to encode quote patterns and `TypeMatch` defines an `unapply` method for quoted type patterns.

```scala
trait Quotes:
  self: runtime.QuoteMatching & ...  =>
  ...

trait QuoteMatching:
  object ExprMatch:
    def unapply[TypeBindings <: Tuple, Tup <: Tuple]
               (scrutinee: Expr[Any])
               (using pattern: Expr[Any]): Option[Tup] = ...
  object TypeMatch:
    ...
```

These extractor methods are only meant to be used in code generated by the compiler.
The call to the extractor that is generated has an already elaborated form that cannot be written in source, namely explicit type parameters and explicit contextual parameters.

This extractor returns a tuple type `Tup` which cannot be inferred from the types in the method signature.
This type will be computed when typing the quote pattern and will be explicitly added to the extractor call.
To refer to type variables in arbitrary places of `Tup`, we need to define them all before their use, hence we have `TypeBindings`, which will contain all pattern type variable definitions.
The extractor also receives a given parameter of type `Expr[Any]` that will contain an expression that represents the pattern.
The compiler will explicitly add this pattern expression.
We use a given parameter because these are the only parameters we are allowed to add to the extractor call in a pattern position.

This extractor is a bit convoluted, but it encodes away all the quotation-specific features.
It compiles the pattern down into a representation that the pattern matcher compiler phase understands.

The quote patterns are encoded into two parts: a tuple pattern that is tasked with extracting the result of the match and a quoted expression representing the pattern.
For example, if the pattern has no `$` we will have an `EmptyTuple` as the pattern and `'{1}` to represent the pattern.

```scala
  case '{ 1 } =>
// is elaborated to
  case ExprMatch(EmptyTuple)(using '{1}) =>
//               ^^^^^^^^^^  ^^^^^^^^^^
//                pattern    expression
```
When extracting expressions, each pattern that is contained in a splice `${..}` will be placed in order in the tuple pattern.
In the following case, the `f` and `x` are placed in a tuple pattern `(f, x)`.
The type of the tuple is encoded in the `Tup` and not only in the tuple itself.
Otherwise, the extractor would return a tuple `Tuple` for which the types need to be tested which is in turn not possible due to type erasure.

```scala
  case '{ ((y: Int) => $f(y)).apply($x) } =>
// is elaborated to
  case ExprMatch[.., (Expr[Int => Int], Expr[Int])]((f, x))(using pattern) =>
// pattern = '{ ((y: Int) => pat[Int](y)).apply(pat[Int]()) }
```
The contents of the quote are transformed into a valid quote expression by replacing the splice with a marker expression `pat[T](..)`.
The type `T` is taken from the type of the splice and the arguments are the HOAS arguments.
This implies that a `pat[T]()` is a closed pattern and `pat[T](y)` is an HOAS pattern that can refer to `y`.


Type variables in quoted patterns are first normalized to have all definitions at the start of the pattern.
For each definition of a type variable `t` in the pattern we will add a type variable definition in `TypeBindings`.
Each one will have a corresponding `Type[t]` that will get extracted if the pattern matches.
These `Type[t]` are also listed in the `Tup` and added in the tuple pattern.
It is additionally marked as `using` in the pattern to make it implicitly available in this case branch.


```scala
  case '{ type t; ($xs: List[t]).map[t](identity[t]) } =>
// is elaborated to
  case ExprMatch[(t), (Type[t], Expr[List[t]])]((using t, xs))(using p) =>
//               ^^^  ^^^^^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^  ^^^^^^^
//     type bindings        result type            pattern     expression
// p = '{ @patternType type u; pat[List[u]]().map[u](identity[u]) }
```

The contents of the quote are transformed into a valid quote expression by replacing type variables with fresh ones that do not escape the quote scope.
These are also annotated to be easily identifiable as pattern variables.

#### Level Consistency Checking
Level consistency checking is performed after typing the program as a static check.
To check level consistency we traverse the tree top-down remembering the context staging level.
Each local definition in scope is recorded with its level and each term reference to a definition is checked against the current staging level.
```scala
// level 0
'{ // level 1
  val x = ... // level 1 with (x -> 1)
  ${ // level 0 (x -> 1)
    val y = ... // level 0 with (x -> 1, y -> 0)
    x // error: defined at level 1 but used in level 0
  }
  // level 1 (x -> 1)
  x // x is ok
}
```

#### Type Healing

When using a generic type `T` in a future stage, it is necessary to have a given `Type[T]` in scope.
The compiler needs to identify those references and link them with the instance of `Type[T]`.
For instance consider the following example:

```scala
def emptyList[T](using t: Type[T])(using Quotes): Expr[List[T]] =
  '{ List.empty[T] }
```

For each reference to a generic type `T` that is defined at level 0 and used at level 1 or greater, the compiler will summon a `Type[T]`.
This is usually the given type that is provided as parameter, `t` in this case.
We can use the type `t.Underlying` to replace `T` as it is an alias of that type.
But `t.Underlying` contains the extra information that it is `t` that will be used in the evaluation of the quote.
In a sense, `Underlying` acts like a splice for types.

```scala
def emptyList[T](using t: Type[T])(using Quotes): Expr[List[T]] =
  '{ List.empty[t.Underlying] }
```

Due to some technical limitations, it is not always possible to replace the type reference with the AST containing `t.Underlying`.
To overcome this limitation, we can simply define a list of type aliases at the start of the quote and insert the `t.Underlying` there.
This has the added advantage that we do not have to repeatedly insert the `t.Underlying` in the quote.

```scala
def emptyList[T](using t: Type[T])(using Quotes): Expr[List[T]] =
  '{ type U = t.Underlying; List.empty[U] }
```
These aliases can be used at any level within the quote and this transformation is only performed on quotes that are at level 0.

```scala
  '{ List.empty[T] ... '{ List.empty[T] } ... }
// becomes
  '{ type U = t.Underlying; List.empty[U] ... '{ List.empty[U] } ... }
```
If we define a generic type at level 1 or greater, it will not be subject to this transformation.
In some future compilation stage, when the definition of the generic type is at level 0, it will be subject to this transformation.
This simplifies the transformation logic and avoids leaking the encoding into code that a macro could inspect.

```scala
'{
  def emptyList[T: Type](using Quotes): Expr[List[T]] = '{ List.empty[T] }
  ...
}
```
A similar transformation is performed on `Type.of[T]`.
Any generic type in `T` needs to have an implicitly given `Type[T]` in scope, which will also be used as a path.
The example:

```scala
def empty[T](using t: Type[T])(using Quotes): Expr[T] =
  Type.of[T] match ...
// becomes
def empty[T](using t: Type[T])(using Quotes): Expr[T] =
  Type.of[t.Underlying] match ...
// then becomes
def empty[T](using t: Type[T])(using Quotes): Expr[T] =
  t match ...
```

The operation `Type.of[t.Underlying]` can be optimized to just `t`.
But this is not always the case.
If the generic reference is nested in the type, we will need to keep the `Type.of`.

```scala
def matchOnList[T](using t: Type[T])(using Quotes): Expr[List[T]] =
  Type.of[List[T]] match ...
// becomes
def matchOnList[T](using t: Type[T])(using Quotes): Expr[List[T]] =
  Type.of[List[t.Underlying]] match ...
```

By doing this transformation, we ensure that each abstract type `U` used in `Type.of` has an implicit `Type[U]` in scope.
This representation makes it simpler to identify parts of the type that are statically known from those that are known dynamically.
Type aliases are also added within the type of the `Type.of` though these are not valid source code.
These would look like `Type.of[{type U = t.Underlying; Map[U, U]}]` if written in source code.


#### Splice Normalization

The contents of a splice may refer to variables defined in the enclosing quote.
This complicates the process of serialization of the contents of the quotes.
To make serialization simple, we first transform the contents of each level 1 splice.
Consider the following example:

```scala
def power5to(n: Expr[Int]): Expr[Double] = '{
  val x: Int = 5
  ${ powerCode('{x}, n) }
}
```

The variable `x` is defined in the quote and used in the splice.
The normal form will extract all references to `x` and replace them with a staged version of `x`.
We will replace the reference to `x` of type `T` with a `$y` where `y` is of type `Expr[T]`.
Then we wrap the new contents of the splice in a lambda that defines `y` and apply it to the quoted version of `x`.
After this transformation we have 2 parts, a lambda without references to the quote, which knows how to compute the contents of the splice, and a sequence of quoted arguments that refer to variables defined in the lambda.

```scala
def power5to(n: Expr[Int]): Expr[Double] = '{
  val x: Int = 5
  ${ ((y: Expr[Int]) => powerCode('{$y}, n)).apply('x) }
}
```

In general, the splice normal form has the shape `${ <lambda>.apply(<args>*) }` and the following constraints:
 * `<lambda>` a lambda expression that does not refer to variables defined in the outer quote
 * `<args>` sequence of quoted expressions or `Type.of` containing references to variables defined in the enclosing quote and no references to local variables defined outside the enclosing quote


##### Function references normalization
A reference to a function `f` that receives parameters is not a valid value in Scala.
Such a function reference `f` can be eta-expanded as `x => f(x)` to be used as a lambda value.
Therefore function references cannot be transformed by the normalization as directly as other expressions as we cannot represent `'{f}` with a method reference type.
We can use the eta-expanded form of `f` in the normalized form.
For example, consider the reference to `f` below.

```scala
'{
  def f(a: Int)(b: Int, c: Int): Int = 2 + a + b + c
  ${ '{ f(3)(4, 5) } }
}
```

To normalize this code, we can eta-expand the reference to `f` and place it in a quote containing a proper expression.
Therefore the normalized form of the argument `'{f}` becomes the quoted lambda `'{ (a: Int) => (b: Int, c: Int) => f(a)(b, c) }` and is an expression of type `Expr[Int => (Int, Int) => Int]`.
The eta-expansion produces one curried lambda per parameter list.
The application `f(3)(4, 5)` does not become `$g(3)(4, 5)` but `$g.apply(3).apply(4, 5)`.
We add the `apply` because `g` is not a quoted reference to a function but a curried lambda.

```scala
'{
  def f(a: Int)(b: Int, c: Int): Int = 2 + a + b + c
  ${
    (
      (g: Expr[Int => (Int, Int) => Int]) => '{$g.apply(3).apply(4, 5)}
    ).apply('{ (a: Int) => (b: Int, c: Int) => f(a)(b, c) })
  }
}
```

Then we can apply it and beta-reduce the application when generating the code.

```scala
      (g: Expr[Int => Int => Int]) => betaReduce('{$g.apply(3).apply(4)})
```


##### Variable assignment normalization
A reference to a mutable variable in the left-hand side of an assignment cannot be transformed directly as it is not in an expression position.
```scala
'{
  var x: Int = 5
  ${ g('{x = 2}) }
}
```

We can use the same strategy used for function references by eta-expanding the assignment operation `x = _` into `y => x = y`.

```scala
'{
  var x: Int = 5
  ${
    g(
      (
        (f: Expr[Int => Unit]) => betaReduce('{$f(2)})
      ).apply('{ (y: Int) => x = $y })
    )
  }
}
```


##### Type normalization
Types defined in the quote are subject to a similar transformation.
In this example, `T` is defined within the quote at level 1 and used in the splice again at level 1.

```scala
'{ def f[T] = ${ '{g[T]} } }
```

The normalization will add a `Type[T]` to the lambda, and we will insert this reference.
The difference is that it will add an alias similar to the one used in type healing.
In this example, we create a `type U` that aliases the staged type.

```scala
'{
  def f[T] = ${
    (
      (t: Type[T]) => '{type U = t.Underling; g[U]}
    ).apply(Type.of[T])
  }
}
```

#### Serialization

Quoted code needs to be pickled to make it available at run-time in the next compilation phase.
We implement this by pickling the AST as a TASTy binary.

##### TASTy
The TASTy format is the typed abstract syntax tree serialization format of Scala 3.
It usually pickles the fully elaborated code after type-checking and is kept along the generated Java classfiles.


##### Pickling
We use TASTy as a serialization format for the contents of the quotes.
To show how serialization is performed, we will use the following example.
```scala
'{
  val (x, n): (Double, Int) = (5, 2)
  ${ powerCode('{x}, '{n}) } * ${ powerCode('{2}, '{n}) }
}
```

This quote is transformed into the following code when normalizing the splices.

```scala
'{
  val (x, n): (Double, Int) = (5, 2)
  ${
    ((y: Expr[Double], m: Expr[Int]) => powerCode(y, m)).apply('x, 'n)
  } * ${
    ((m: Expr[Int]) => powerCode('{2}, m)).apply('n)
  }
}
```

Splice normalization is a key part of the serialization process as it only allows references to variables defined in the quote in the arguments of the lambda in the splice.
This makes it possible to create a closed representation of the quote without much effort.
The first step is to remove all the splices and replace them with holes.
A hole is like a splice but it lacks the knowledge of how to compute the contents of the splice.
Instead, it knows the index of the hole and the contents of the arguments of the splice.
We can see this transformation in the following example where a hole is represented by `<< idx; holeType; args* >>`.

```scala
  ${ ((y: Expr[Double], m: Expr[Int]) => powerCode(y, m)).apply('x, 'n) }
// becomes
  << 0; Double; x, n >>
```

As this was the first hole it has index 0.
The hole type is `Double`, which needs to be remembered now that we cannot infer it from the contents of the splice.
The arguments of the splice are `x` and `n`; note that they do not require quoting because they were moved out of the splice.

References to healed types are handled in a similar way.
Consider the `emptyList` example, which shows the type aliases that are inserted into the quote.
```scala
'{ List.empty[T] }
// type healed to
'{ type U = t.Underlying; List.empty[U] }
```
Instead of replacing a splice, we replace the `t.Underlying` type with a type hole.
The type hole is represented by `<< idx; bounds >>`.
```scala
'{ type U = << 0; Nothing..Any >>; List.empty[U] }
```
Here, the bounds of `Nothing..Any` are the bounds of the original `T` type.
The types of a `Type.of` are transformed in the same way.


With these transformations, the contents of the quote or `Type.of` are guaranteed to be closed and therefore can be pickled.
The AST is pickled into TASTy, which is a sequence of bytes.
This sequence of bytes needs to be instantiated in the bytecode, but unfortunately it cannot be dumped into the classfile as bytes.
To reify it we encode the bytes into a Java `String`.
In the following examples we display this encoding in human readable form with the fictitious `|tasty"..."|` string literal.

```scala
// pickled AST bytes encoded in a base64 string
tasty"""
  val (x, n): (Double, Int) = (5, 2)
  << 0; Double; x, n >> * << 1; Double; n >>
"""
// or
tasty"""
  type U = << 0; Nothing..Any; >>
  List.empty[U]
"""
```
The contents of a quote or `Type.of` are not always pickled.
In some cases it is better to generate equivalent (smaller and/or faster) code that will compute the expression.
Literal values are compiled into a call to `Expr(<literal>)` using the implementation of `ToExpr` to create the quoted expression.
This is currently performed only on literal values, but can be extended to any value for which we have a `ToExpr` defined in the standard library.
Similarly, for non-generic types we can use their respective `java.lang.Class` and convert them into a `Type` using a primitive operation `typeConstructorOf` defined in the reflection API.

##### Unpickling

Now that we have seen how a quote is pickled, we can look at how to unpickle it.
We will continue with the previous example.

Holes were used to replace the splices in the quote.
When we perform this transformation we also need to remember the lambdas from the splices and their hole index.
When unpickling a hole, the corresponding splice lambda will be used to compute the contents of the hole.
The lambda will receive as parameters quoted versions of the arguments of the hole.
For example to compute the contents of `<< 0; Double; x, n >>` we will evaluate the following code

```scala
  ((y: Expr[Double], m: Expr[Int]) => powerCode(y, m)).apply('x, 'n)
```

The evaluation is not as trivial as it looks, because the lambda comes from compiled code and the rest is code that must be interpreted.
We put the AST of `x` and `n` into `Expr` objects to simulate the quotes and then we use Java Reflection to call the `apply` method.

We may have many holes in a quote and therefore as many lambdas.
To avoid the instantiation of many lambdas, we can join them together into a single lambda.
Apart from the list of arguments, this lambda will also take the index of the hole that is being evaluated.
It will perform a switch match on the index and call the corresponding lambda in each branch.
Each branch will also extract the arguments depending on the definition of the lambda.
The application of the original lambdas are beta-reduced to avoid extra overhead.

```scala
(idx: Int, args: Seq[Any]) =>
  idx match
    case 0 => // for << 0; Double; x, n >>
      val x = args(0).asInstanceOf[Expr[Double]]
      val n = args(1).asInstanceOf[Expr[Int]]
      powerCode(x, n)
    case 1 => // for << 1; Double; n >>
      val n = args(0).asInstanceOf[Expr[Int]]
      powerCode('{2}, n)
```

This is similar to what we do for splices when we replace the type aliased with holes we keep track of the index of the hole.
Instead of lambdas, we will have a list of references to instances of `Type`.
From the following example we would extract `t`, `u`, ... .

```scala
  '{ type T1 = t1.Underlying; type Tn = tn.Underlying; ... }
// with holes
  '{ type T1 = << 0; ... >>; type Tn = << n-1; ... >>; ... }
```

As the type holes are at the start of the quote, they will have the first `N` indices.
This implies that we can place the references in a sequence `Seq(t, u, ...)` where the index in the sequence is the same as the hole index.

Lastly, the quote itself is replaced by a call to `QuoteUnpickler.unpickleExpr` which will unpickle the AST, evaluate the holes, i.e., splices, and wrap the resulting AST in an `Expr[Int]`.
This method takes takes the pickled `|tasty"..."|`, the types and the hole lambda.
Similarly, `Type.of` is replaced with a call to `QuoteUnpickler.unpickleType` but only receives the pickled `|tasty"..."|` and the types.
Because `QuoteUnpickler` is part of the self-type of the `Quotes` class, we have to cast the instance but know that this cast will always succeed.

```scala
quotes.asInstanceOf[runtime.QuoteUnpickler].unpickleExpr[T](
  pickled = tasty"...",
  types = Seq(...),
  holes = (idx: Int, args: Seq[Any]) => idx match ...
)
```

[^1]: [Scalable Metaprogramming in Scala 3](https://infoscience.epfl.ch/record/299370)
[^2]: [Multi-stage programming with generative and analytical macros](https://dl.acm.org/doi/10.1145/3486609.3487203).
[^3]: In quotes, identifiers starting with `$` must be surrounded by backticks (`` `$` ``). For example `$conforms` from `scala.Predef`.
