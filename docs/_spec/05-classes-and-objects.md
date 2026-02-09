---
title: Classes & Objects
layout: default
chapter: 5
---

# Classes and Objects

```ebnf
TmplDef          ::= [‘case’] ‘class’ ClassDef
                  |  [‘case’] ‘object’ ObjectDef
                  |  ‘trait’ TraitDef
```

[Classes](#class-definitions) and [objects](#object-definitions) are both defined in terms of _templates_.

## Templates

```ebnf
ClassTemplate   ::=  [EarlyDefs] ClassParents [TemplateBody]
TraitTemplate   ::=  [EarlyDefs] TraitParents [TemplateBody]
ClassParents    ::=  Constr {‘with’ AnnotType}
TraitParents    ::=  AnnotType {‘with’ AnnotType}
TemplateBody    ::=  [nl] ‘{’ [SelfType] TemplateStat {semi TemplateStat} ‘}’
SelfType        ::=  id [‘:’ Type] ‘=>’
                 |   this ‘:’ Type ‘=>’
```

A _template_ defines the type signature, behavior and initial state of a trait or class of objects or of a single object.
Templates form part of instance creation expressions, class definitions, and object definitions.
A template `´sc´ with ´mt_1´ with ... with ´mt_n´ { ´\mathit{stats}´ }` consists of a constructor invocation ´sc´ which defines the template's _superclass_, trait references `´mt_1, ..., mt_n´` ´(n \geq 0)´, which define the template's _traits_, and a statement sequence ´\mathit{stats}´ which contains initialization code and additional member definitions for the template.

Each trait reference ´mt_i´ must denote a [trait](#traits).
By contrast, the superclass constructor ´sc´ normally refers to a class which is not a trait.
It is possible to write a list of parents that starts with a trait reference, e.g. `´mt_1´ with ... with ´mt_n´`.
In that case the list of parents is implicitly extended to include the supertype of ´mt_1´ as the first parent type.
The new supertype must have at least one constructor that does not take parameters.
In the following, we will always assume that this implicit extension has been performed, so that the first parent class of a template is a regular superclass constructor, not a trait reference.

The list of parents of a template must be well-formed.
This means that the class denoted by the superclass constructor ´sc´ must be a subclass of the superclasses of all the traits ´mt_1, ..., mt_n´.
In other words, the non-trait classes inherited by a template form a chain in the inheritance hierarchy which starts with the template's superclass.

It is forbidden for a template's superclass constructor ´sc´ to be an [enum class](#enum-definitions), unless the template is the implementation of an [enum case](#enum-definitions) of ´sc´.

The _least proper supertype_ of a template is the class type or [compound type](03-types.html#compound-types) consisting of all its parent class types.

The statement sequence ´\mathit{stats}´ contains member definitions that define new members or overwrite members in the parent classes.
If the template forms part of an abstract class or trait definition, the statement part ´\mathit{stats}´ may also contain definitions of abstract members.
If the template forms part of a concrete class definition, ´\mathit{stats}´ may still contain definitions of abstract type members, but not of abstract term members.
Furthermore, ´\mathit{stats}´ may in any case also contain expressions; these are executed in the order they are given as part of the initialization of a template.

The sequence of template statements may be prefixed with a formal parameter definition and an arrow, e.g. `´x´ =>`, or `´x´:´T´ =>`.
If a formal parameter is given, it can be used as an alias for the reference `this` throughout the body of the template.
If the formal parameter comes with a type ´T´, this definition affects the _self type_ ´S´ of the underlying class or object as follows:
Let ´C´ be the type of the class or trait or object defining the template.
If a type ´T´ is given for the formal self parameter, ´S´ is the greatest lower bound of ´T´ and ´C´.
If no type ´T´ is given, ´S´ is just ´C´.
Inside the template, the type of `this` is assumed to be ´S´.

The self type of a class or object must conform to the self types of all classes which are inherited by the template ´t´.

A second form of self type annotation reads just `this: ´S´ =>`.
It prescribes the type ´S´ for `this` without introducing an alias name for it.

###### Example
Consider the following class definitions:

```scala
class Base extends Object {}
trait Mixin extends Base {}
object O extends Mixin {}
```

In this case, the definition of `O` is expanded to:

```scala
object O extends Base with Mixin {}
```

<!-- TODO: Make all references to Java generic -->

**Inheriting from Java Types**

A template may have a Java class as its superclass and Java interfaces as its mixins.

**Template Evaluation**

Consider a template `´sc´ with ´mt_1´ with ´mt_n´ { ´\mathit{stats}´ }`.

If this is the template of a [trait](#traits) then its _mixin-evaluation_ consists of an evaluation of the statement sequence ´\mathit{stats}´.

If this is not a template of a trait, then its _evaluation_ consists of the following steps.

- First, the superclass constructor ´sc´ is
  [evaluated](#constructor-invocations).
- Then, all base classes in the template's [linearization](#class-linearization) up to the template's superclass denoted by ´sc´ are evaluated.
evaluation happens in reverse order of occurrence in the linearization. Each evaluation occurs as follows:
  - First, arguments to ´mt_i´ are evaluated from left to right, and set as parameters of ´mt_i´.
  - ´mt_i´ is then mixin-evaluated.
- Finally, the statement sequence ´\mathit{stats}\,´ is evaluated.

### Constructor Invocations

```ebnf
Constr  ::=  AnnotType {‘(’ [Exprs] ‘)’}
```

Constructor invocations define the type, members, and initial state of objects created by an instance creation expression, or of parts of an object's definition which are inherited by a class or object definition.
A constructor invocation is a method application `´x´.´c´[´\mathit{targs}´](´\mathit{args}_1´)...(´\mathit{args}_n´)`, where ´x´ is a [stable identifier](03-types.html#paths), ´c´ is a type name which either designates a class or defines an alias type for one, ´\mathit{targs}´ is a type argument list, ´\mathit{args}_1, ..., \mathit{args}_n´ are argument lists, and there is a constructor of that class which is [applicable](06-expressions.html#method-applications) to the given arguments.
If the constructor invocation uses named or default arguments, it is transformed into a block expression using the same transformation as described [here](sec:named-default).

The prefix `´x´.` can be omitted.
A type argument list can be given only if the class ´c´ takes type parameters.
Even then it can be omitted, in which case a type argument list is synthesized using [local type inference](06-expressions.html#local-type-inference).
If no explicit arguments are given, an empty list `()` is implicitly supplied.

An evaluation of a constructor invocation `´x´.´c´[´\mathit{targs}´](´\mathit{args}_1´)...(´\mathit{args}_n´)` consists of the following steps:

- First, the prefix ´x´ is evaluated.
- Then, the arguments ´\mathit{args}_1, ..., \mathit{args}_n´ are evaluated from left to right.
- Finally, the class being constructed is initialized by evaluating the template of the class referred to by ´c´.

### Class Linearization

The classes reachable through transitive closure of the direct inheritance relation from a class ´C´ are called the _base classes_ of ´C´.
Because of mixins, the inheritance relationship on base classes forms in general a directed acyclic graph.
A linearization of this graph is defined as follows.

###### Definition: linearization
Let ´C´ be a class with template ´C_1´ with ... with ´C_n´ { ´\mathit{stats}´ }`.
The _linearization_ of ´C´, ´\mathcal{L}(C)´ is defined as follows:
$$
\mathcal{L}(C) = C, \mathcal{L}(C_n) \; \vec{+} \; ... \; \vec{+} \; \mathcal{L}(C_1)
$$

Here ´\vec{+}´ denotes concatenation where elements of the right operand replace identical elements of the left operand:

$$
\begin{array}{lcll}
\{a, A\} \;\vec{+}\; B &=& a, (A \;\vec{+}\; B)  &{\bf if} \; a \not\in B \\\\
                       &=& A \;\vec{+}\; B       &{\bf if} \; a \in B
\end{array}
$$

###### Example
Consider the following class definitions.

```scala
abstract class AbsIterator extends AnyRef { ... }
trait RichIterator extends AbsIterator { ... }
class StringIterator extends AbsIterator { ... }
class Iter extends StringIterator with RichIterator { ... }
```

Then the linearization of class `Iter` is

```scala
{ Iter, RichIterator, StringIterator, AbsIterator, AnyRef, Any }
```

Note that the linearization of a class refines the inheritance relation: if ´C´ is a subclass of ´D´, then ´C´ precedes ´D´ in any linearization where both ´C´ and ´D´ occur.
[Linearization](#definition:-linearization) also satisfies the property that a linearization of a class always contains the linearization of its direct superclass as a suffix.

For instance, the linearization of `StringIterator` is

```scala
{ StringIterator, AbsIterator, AnyRef, Any }
```

which is a suffix of the linearization of its subclass `Iter`.
The same is not true for the linearization of mixins.
For instance, the linearization of `RichIterator` is

```scala
{ RichIterator, AbsIterator, AnyRef, Any }
```

which is not a suffix of the linearization of `Iter`.

### Class Members

A class ´C´ defined by a template `´C_1´ with ... with ´C_n´ { ´\mathit{stats}´ }` can define members in its statement sequence ´\mathit{stats}´ and can inherit members from all parent classes.
Scala adopts Java and C\#'s conventions for static overloading of methods.
It is thus possible that a class defines and/or inherits several methods with the same name.
To decide whether a defined member of a class ´C´ overrides a member of a parent class, or whether the two co-exist as overloaded variants in ´C´, Scala uses the following definition of _matching_ on members:

###### Definition: matching
A member definition ´M´ _matches_ a member definition ´M'´, if ´M´ and ´M'´ bind the same name, and one of following holds.

1. Neither ´M´ nor ´M'´ is a method definition.
2. ´M´ and ´M'´ define both monomorphic methods with equivalent argument types.
3. ´M´ is defined in Java and defines a method with an empty parameter list `()` and ´M'´ defines a parameterless method.
4. ´M´ and ´M'´ define both polymorphic methods with equal number of argument types ´\overline T´, ´\overline T'´ and equal numbers of type parameters ´\overline t´, ´\overline t'´, say, and  ´\overline T' = [\overline t'/\overline t]\overline T´.

<!--
every argument type
´T_i´ of ´M´ is equal to the corresponding argument type ´T`_i´ of
´M`´ where every occurrence of a type parameter ´t`´ of ´M`´ has been replaced by the corresponding type parameter ´t´ of ´M´.
-->

Member definitions fall into two categories: concrete and abstract.
Members of class ´C´ are either _directly defined_ (i.e. they appear in ´C´'s statement sequence ´\mathit{stats}´) or they are _inherited_.
There are two rules that determine the set of members of a class, one for each category:

A _concrete member_ of a class ´C´ is any concrete definition ´M´ in some class ´C_i \in \mathcal{L}(C)´, except if there is a preceding clas ´C_j \in \mathcal{L}(C)´ where ´j < i´ which directly defines a concrete member ´M'´ matching ´M´.

An _abstract member_ of a class ´C´ is any abstract definition ´M´ in some class ´C_i \in \mathcal{L}(C)´, except if ´C´ contains already a concrete member ´M'´ matching ´M´, or if there is a preceding class ´C_j \in \mathcal{L}(C)´ where ´j < i´ which directly defines an abstract member ´M'´ matching ´M´.

This definition also determines the [overriding](#overriding) relationships between matching members of a class ´C´ and its parents.
First, a concrete definition always overrides an abstract definition.
Second, for definitions ´M´ and ´M´' which are both concrete or both abstract, ´M´ overrides ´M'´ if ´M´ appears in a class that precedes (in the linearization of ´C´) the class in which ´M'´ is defined.

It is an error if a template directly defines two matching members.
It is also an error if a template contains two members (directly defined or inherited) with the same name and the same [erased type](03-types.html#type-erasure).
Finally, a template is not allowed to contain two methods (directly defined or inherited) with the same name which both define default arguments.

###### Example
Consider the trait definitions:

```scala
trait A { def f: Int }
trait B extends A { def f: Int = 1 ; def g: Int = 2 ; def h: Int = 3 }
trait C extends A { override def f: Int = 4 ; def g: Int }
trait D extends B with C { def h: Int }
```

Then trait `D` has a directly defined abstract member `h`.
It inherits member `f` from trait `C` and member `g` from trait `B`.

### Overriding

<!-- TODO: Explain that classes cannot override each other -->

A member ´M´ of class ´C´ that [matches](#class-members) a non-private member ´M'´ of a base class of ´C´ is said to _override_ that member.
In this case the binding of the overriding member ´M´ must [subsume](03-types.html#conformance) the binding of the overridden member ´M'´.
Furthermore, the following restrictions on modifiers apply to ´M´ and ´M'´:
- ´M'´ must not be a class.
- ´M'´ must not be labeled `final`.
- ´M´ must not be [`private`](#modifiers).
- If ´M´ is labeled `private[´C´]` for some enclosing class or package ´C´, then ´M'´ must be labeled `private[´C'´]` for some class or package ´C'´ where ´C'´ equals ´C´ or ´C'´ is contained in ´C´.

<!-- TODO: check whether this is accurate -->
- If ´M´ is labeled `protected`, then ´M'´ must also be labeled `protected`.
- If ´M'´ is not an abstract member, then ´M´ must be labeled `override`.
Furthermore, one of two possibilities must hold:
    - either ´M´ is defined in a subclass of the class where is ´M'´ is defined,
    - or both ´M´ and ´M'´ override a third member ´M''´ which is defined in a base class of both the classes containing ´M´ and ´M'´
- If ´M'´ is [incomplete](#modifiers) in ´C´ then ´M´ must be labeled `abstract override`.
- If ´M´ and ´M'´ are both concrete value definitions, then either none of them is marked `lazy` or both must be marked `lazy`.

- A stable member can only be overridden by a stable member.
For example, this is not allowed:

```scala
class X { val stable = 1}
class Y extends X { override var stable = 1 } // error
```

Another restriction applies to abstract type members:
An abstract type member with a [volatile type](03-types.html#volatile-types) as its upper bound may not override an abstract type member which does not have a volatile upper bound.

A special rule concerns parameterless methods.
If a parameterless method defined as `def ´f´: ´T´ = ...` or `def ´f´ = ...` overrides a method defined in Java of type ´()T'´ which has an empty parameter list, then ´f´ is also assumed to have an empty parameter list.

An overriding method inherits all default arguments from the definition in the superclass.
By specifying default arguments in the overriding method it is possible to add new defaults (if the corresponding parameter in the superclass does not have a default) or to override the defaults of the superclass (otherwise).

###### Example

Consider the definitions:

```scala
trait Root { type T <: Root }
trait A extends Root { type T <: A }
trait B extends Root { type T <: B }
trait C extends A with B
```

Then the class definition `C` is not well-formed because the binding of `T` in `C` is `type T <: B`, which fails to subsume the binding `type T <: A` of `T`
in type `A`.
The problem can be solved by adding an overriding definition of type `T` in class `C`:

```scala
class C extends A with B { type T <: C }
```

### Inheritance Closure

Let ´C´ be a class type.
The _inheritance closure_ of ´C´ is the smallest set ´\mathscr{S}´ of types such that

- ´C´ is in ´\mathscr{S}´.
- If ´T´ is in ´\mathscr{S}´, then every type ´T'´ which forms syntactically a part of ´T´ is also in ´\mathscr{S}´.
- If ´T´ is a class type in ´\mathscr{S}´, then all [parents](#templates) of ´T´ are also in ´\mathscr{S}´.

It is a static error if the inheritance closure of a class type consists of an infinite number of types.
(This restriction is necessary to make subtyping decidable[^kennedy]).

[^kennedy]: Kennedy, Pierce. [On Decidability of Nominal Subtyping with Variance.]( https://research.microsoft.com/pubs/64041/fool2007.pdf) in FOOL 2007

## Modifiers

```ebnf
Modifier          ::=  LocalModifier
                    |  AccessModifier
                    |  ‘override’
LocalModifier     ::=  ‘abstract’
                    |  ‘final’
                    |  ‘sealed’
                    |  ‘implicit’
                    |  ‘lazy’
                    |  ‘infix’
AccessModifier    ::=  (‘private’ | ‘protected’) [AccessQualifier]
AccessQualifier   ::=  ‘[’ (id | ‘this’) ‘]’
```

Member definitions may be preceded by modifiers which affect the accessibility and usage of the identifiers bound by them.
If several modifiers are given, their order does not matter, but the same modifier may not occur more than once.
Modifiers preceding a repeated definition apply to all constituent definitions.
The rules governing the validity and meaning of a modifier are as follows.

### `private`
The `private` modifier can be used with any definition in a template.
Private members of a template can be accessed only from within the directly enclosing template and its companion module or [companion class](#object-definitions).

The `private` modifier is also valid for [top-level](09-top-level-definitions.html#packagings) templates.

A `private` modifier can be _qualified_ with an identifier ´C´ (e.g. `private[´C´]`) that must denote a class or package enclosing the definition.
Members labeled with such a modifier are accessible respectively only from code inside the package ´C´ or only from code inside the class ´C´ and its [companion module](#object-definitions).

A different form of qualification is `private[this]`.
A member ´M´ marked with this modifier is called _object-protected_; it can be accessed only from within the object in which it is defined.
That is, a selection ´p.M´ is only legal if the prefix is `this` or `´O´.this`, for some class ´O´ enclosing the reference.
In addition, the restrictions for unqualified `private` apply.

Members marked private without a qualifier are called _class-private_, whereas members labeled with `private[this]` are called _object-private_.
A member _is private_ if it is either class-private or object-private, but not if it is marked `private[´C´]` where ´C´ is an identifier; in the latter case the member is called _qualified private_.

Class-private or object-private members may not be abstract, and may not have `protected` or `override` modifiers.
They are not inherited by subclasses and they may not override definitions in parent classes.

### `protected`
The `protected` modifier applies to class member definitions.
Protected members of a class can be accessed from within
  - the template of the defining class,
  - all templates that have the defining class as a base class,
  - the companion module of any of those classes.

A `protected` modifier can be qualified with an identifier ´C´ (e.g. `protected[´C´]`) that must denote a class or package enclosing the definition.
Members labeled with such a modifier are also accessible respectively from all code inside the package ´C´ or from all code inside the class ´C´ and its [companion module](#object-definitions).

A protected identifier ´x´ may be used as a member name in a selection `´r´.´x´` only if one of the following applies:
  - The access is within the template defining the member, or, if a qualification ´C´ is given, inside the package ´C´, or the class ´C´, or its companion module, or
  - ´r´ is one of the reserved words `this` and `super`, or
  - ´r´'s type conforms to a type-instance of the class which contains the access.

A different form of qualification is `protected[this]`.
A member ´M´ marked with this modifier is called _object-protected_; it can be accessed only from within the object in which it is defined. That is, a selection ´p.M´ is only legal if the prefix is `this` or `´O´.this`, for some class ´O´ enclosing the reference. In addition, the restrictions for unqualified `protected` apply.

### `override`
The `override` modifier applies to class member definitions which are not themselves classes.
It is mandatory for member definitions that override some other concrete member definition in a parent class.
If an `override` modifier is given, there must be at least one overridden member definition (either concrete or abstract).

### `abstract override`
The `override` modifier has an additional significance when combined with the `abstract` modifier.
That modifier combination is only allowed for value members of traits.

We call a member ´M´ of a template _incomplete_ if it is either abstract, or it is labeled `abstract` and `override` and every member overridden by ´M´ is again incomplete.

Note that the `abstract override` modifier combination does not influence the concept whether a member is concrete or abstract.

### `abstract`
The `abstract` modifier is used in class definitions.
It is redundant for traits, and mandatory for all other classes which have incomplete members.
Abstract classes cannot be [instantiated](06-expressions.html#instance-creation-expressions) with a constructor invocation unless followed by mixins and/or a refinement which override all incomplete members of the class.
Only abstract classes and traits can have abstract term members.

The `abstract` modifier can also be used in conjunction with `override` for class member definitions.
In that case the previous discussion applies.

### `final`
The `final` modifier applies to class member definitions and to class definitions.
A `final` class member definition may not be overridden in subclasses.
A `final` class may not be inherited by a template.
`final` is redundant for object definitions.
Members of final classes or objects are implicitly also final, so the `final` modifier is generally redundant for them, too.
Note, however, that [constant value definitions](04-basic-definitions.html#value-definitions) do require an explicit `final` modifier, even if they are defined in a final class or object.
`final` is permitted for abstract classes but it may not be applied to traits or incomplete members, and it may not be combined in one modifier list with `sealed`.

### `sealed`
The `sealed` modifier applies to class definitions.
A `sealed` class may not be directly inherited, except if the inheriting template is defined in the same source file as the inherited class.
However, subclasses of a sealed class can be inherited anywhere.

### `lazy`
The `lazy` modifier applies to value definitions.
A `lazy` value is initialized the first time it is accessed (which might never
happen at all).
Attempting to access a lazy value during its initialization might lead to looping behavior.
If an exception is thrown during initialization, the value is considered uninitialized, and a later access will retry to evaluate its right hand side.

### `infix`
The `infix` modifier applies to method definitions and type definitions.
It signals that the method or type is intended for use in infix position, even if it has an alphanumeric name.

If a method overrides another, their `infix` annotations must agree. Either both are annotated with `infix`, or none of them are.

The first non-receiver parameter list of an `infix` method must define exactly one parameter. Examples:

```scala
infix def op1(x: S): R             // ok
infix def op2[T](x: T)(y: S): R    // ok
infix def op3[T](x: T, y: S): R    // error: two parameters
extension (x: A)
  infix def op4(y: B): R          // ok
  infix def op5(y1: B, y2: B): R  // error: two parameters
```

`infix` modifiers can also be given to type, trait or class definitions that have exactly two type parameters. An infix type like

```scala
infix type op[X, Y]
```

can be applied using infix syntax, i.e., `A op B`.

###### Example
The following code illustrates the use of qualified private:

```scala
package outerpkg.innerpkg
class Outer {
  class Inner {
    private[Outer] def f()
    private[innerpkg] def g()
    private[outerpkg] def h()
  }
}
```

Here, accesses to the method `f` can appear anywhere within `Outer`, but not outside it.
Accesses to method `g` can appear anywhere within the package `outerpkg.innerpkg`, as would be the case for package-private methods in Java.
Finally, accesses to method `h` can appear anywhere within package `outerpkg`, including packages contained in it.

###### Example
A useful idiom to prevent clients of a class from constructing new instances of that class is to declare the class `abstract` and `sealed`:

```scala
object m {
  abstract sealed class C (x: Int) {
    def nextC = new C(x + 1) {}
  }
  val empty = new C(0) {}
}
```

For instance, in the code above clients can create instances of class `m.C` only by calling the `nextC` method of an existing `m.C` object; it is not possible for clients to create objects of class `m.C` directly.
Indeed the following two lines are both in error:

```scala
new m.C(0)    // **** error: C is abstract, so it cannot be instantiated.
new m.C(0) {} // **** error: illegal inheritance from sealed class.
```

A similar access restriction can be achieved by marking the primary constructor `private` ([example](#example-private-constructor)).

## Class Definitions

```ebnf
TmplDef           ::=  ‘class’ ClassDef
ClassDef          ::=  id [TypeParamClause] {Annotation}
                       [AccessModifier] ClassParamClauses ClassTemplateOpt
ClassParamClauses ::=  {ClassParamClause}
                       [[nl] ‘(’ implicit ClassParams ‘)’]
ClassParamClause  ::=  [nl] ‘(’ [ClassParams] ‘)’
ClassParams       ::=  ClassParam {‘,’ ClassParam}
ClassParam        ::=  {Annotation} {Modifier} [(‘val’ | ‘var’)]
                       id [‘:’ ParamType] [‘=’ Expr]
ClassTemplateOpt  ::=  ‘extends’ ClassTemplate | [[‘extends’] TemplateBody]
```

The most general form of class definition is

```scala
class ´c´[´\mathit{tps}\,´] ´as´ ´m´(´\mathit{ps}_1´)...(´\mathit{ps}_n´) extends ´t´    ´\quad(n \geq 0)´.
```

Here,

  - ´c´ is the name of the class to be defined.
  - ´\mathit{tps}´ is a non-empty list of type parameters of the class being defined.
  The scope of a type parameter is the whole class definition including the type parameter section itself.
  It is illegal to define two type parameters with the same name.
  The type parameter section `[´\mathit{tps}\,´]` may be omitted.
  A class with a type parameter section is called _polymorphic_, otherwise it is called _monomorphic_.
  - ´as´ is a possibly empty sequence of [annotations](11-annotations.html#user-defined-annotations).
  If any annotations are given, they apply to the primary constructor of the class.
  - ´m´ is an [access modifier](#modifiers) such as `private` or `protected`, possibly with a qualification.
  If such an access modifier is given it applies to the primary constructor of the class.
  - ´(\mathit{ps}\_1)...(\mathit{ps}\_n)´ are formal value parameter clauses for the _primary constructor_ of the class.
  The scope of a formal value parameter includes all subsequent parameter sections and the template ´t´.
  However, a formal value parameter may not form part of the types of any of the parent classes or members of the class template ´t´.
  It is illegal to define two formal value parameters with the same name.

    If a class has no formal parameter section that is not implicit, an empty parameter section `()` is assumed.

    If a formal parameter definition ´x: T´ is preceded by a `val` or `var` keyword, an accessor [definition](04-basic-definitions.html#value-definitions) for this parameter is implicitly added to the class.

    The accessor introduces a value member ´x´ of class ´c´ that is defined as an alias of the parameter.
    If the introducing keyword is `var`, a setter accessor [`´x´_=`](04-basic-definitions.html#variable-definitions) is also implicitly added to the class.
    An invocation of that setter `´x´_=(´e´)` changes the value of the parameter to the result of evaluating ´e´.

    The formal parameter definition may contain modifiers, which then carry over to the accessor definition(s).
    When access modifiers are given for a parameter, but no `val` or `var` keyword, `val` is assumed.
    A formal parameter prefixed by `val` or `var` may not at the same time be a [call-by-name parameter](04-basic-definitions.html#by-name-parameters).

  - ´t´ is a [template](#templates) of the form

    ```scala
    ´sc´ with ´mt_1´ with ... with ´mt_m´ { ´\mathit{stats}´ } // ´m \geq 0´
    ```

    which defines the base classes, behavior and initial state of objects of the class.
    The extends clause `extends ´sc´ with ´mt_1´ with ... with ´mt_m´` can be omitted, in which case `extends scala.AnyRef` is assumed.
    The class body `{ ´\mathit{stats}´ }` may also be omitted, in which case the empty body `{}` is assumed.

This class definition defines a type `´c´[´\mathit{tps}\,´]` and a constructor which when applied to parameters conforming to types ´\mathit{ps}´ initializes instances of type `´c´[´\mathit{tps}\,´]` by evaluating the template ´t´.

###### Example – `val` and `var` parameters
The following example illustrates `val` and `var` parameters of a class `C`:

```scala
class C(x: Int, val y: String, var z: List[String])
val c = new C(1, "abc", List())
c.z = c.y :: c.z
```

###### Example – Private Constructor
The following class can be created only from its companion module.

```scala
object Sensitive {
  def makeSensitive(credentials: Certificate): Sensitive =
    if (credentials == Admin) new Sensitive()
    else throw new SecurityViolationException
}
class Sensitive private () {
  ...
}
```

### Constructor Definitions

```ebnf
FunDef         ::= ‘this’ ParamClause ParamClauses
                   (‘=’ ConstrExpr | [nl] ConstrBlock)
ConstrExpr     ::= SelfInvocation
                |  ConstrBlock
ConstrBlock    ::= ‘{’ SelfInvocation {semi BlockStat} ‘}’
SelfInvocation ::= ‘this’ ArgumentExprs {ArgumentExprs}
```

A class may have additional constructors besides the primary constructor.
These are defined by constructor definitions of the form `def this(´\mathit{ps}_1´)...(´\mathit{ps}_n´) = ´e´`.
Such a definition introduces an additional constructor for the enclosing class, with parameters as given in the formal parameter lists ´\mathit{ps}_1 , ..., \mathit{ps}_n´, and whose evaluation is defined by the constructor expression ´e´.
The scope of each formal parameter is the subsequent parameter sections and the constructor expression ´e´.
A constructor expression is either a self constructor invocation `this(´\mathit{args}_1´)...(´\mathit{args}_n´)` or a block which begins with a self constructor invocation.
The self constructor invocation must construct a generic instance of the class.
I.e. if the class in question has name ´C´ and type parameters `[´\mathit{tps}\,´]`, then a self constructor invocation must generate an instance of `´C´[´\mathit{tps}\,´]`; it is not permitted to instantiate formal type parameters.

The signature and the self constructor invocation of a constructor definition are type-checked and evaluated in the scope which is in effect at the point of the enclosing class definition, augmented by any type parameters of the enclosing class.
The rest of the constructor expression is type-checked and evaluated as a method body in the current class.

If there are auxiliary constructors of a class ´C´, they form together with ´C´'s primary [constructor](#class-definitions) an overloaded constructor definition.
The usual rules for [overloading resolution](06-expressions.html#overloading-resolution) apply for constructor invocations of ´C´, including for the self constructor invocations in the constructor expressions themselves.
However, unlike other methods, constructors are never inherited.
To prevent infinite cycles of constructor invocations, there is the restriction that every self constructor invocation must refer to a constructor definition which precedes it (i.e. it must refer to either a preceding auxiliary constructor or the primary constructor of the class).

###### Example
Consider the class definition

```scala
class LinkedList[A]() {
  var head: A = _
  var tail: LinkedList[A] = null
  def this(head: A) = { this(); this.head = head }
  def this(head: A, tail: LinkedList[A]) = { this(head); this.tail = tail }
}
```

This defines a class `LinkedList` with three constructors.
The second constructor constructs a singleton list, while the third one constructs a list with a given head and tail.

### Case Classes

```ebnf
TmplDef  ::=  ‘case’ ‘class’ ClassDef
```

If a class definition is prefixed with `case`, the class is said to be a _case class_.

A case class is required to have a parameter section that is not implicit.
The formal parameters in the first parameter section are called _elements_ and are treated specially.
First, the value of such a parameter can be extracted as a field of a constructor pattern.
Second, a `val` prefix is implicitly added to such a parameter, unless the parameter already carries a `val` or `var` modifier.
Hence, an accessor definition for the parameter is [generated](#class-definitions).

A case class definition of `´c´[´\mathit{tps}\,´](´\mathit{ps}_1\,´)...(´\mathit{ps}_n´)` with type parameters ´\mathit{tps}´ and value parameters ´\mathit{ps}´ implies the definition of a companion object, which serves as an [extractor object](08-pattern-matching.html#extractor-patterns).
It has the following shape:

```scala
object ´c´ {
  def apply[´\mathit{tps}\,´](´\mathit{ps}_1\,´)...(´\mathit{ps}_n´): ´c´[´\mathit{tps}\,´] = new ´c´[´\mathit{Ts}\,´](´\mathit{xs}_1\,´)...(´\mathit{xs}_n´)
  def unapply[´\mathit{tps}\,´](´x´: ´c´[´\mathit{tps}\,´]) =
    if (x eq null) scala.None
    else scala.Some(´x.\mathit{xs}_{11}, ... , x.\mathit{xs}_{1k}´)
}
```

Here, ´\mathit{Ts}´ stands for the vector of types defined in the type parameter section ´\mathit{tps}´, each ´\mathit{xs}\_i´ denotes the parameter names of the parameter section ´\mathit{ps}\_i´, and ´\mathit{xs}\_{11}, ... , \mathit{xs}\_{1k}´ denote the names of all parameters in the first parameter section ´\mathit{xs}\_1´.
If a type parameter section is missing in the class, it is also missing in the `apply` and `unapply` methods.

If the companion object ´c´ is already defined, the  `apply` and `unapply` methods are added to the existing object.
If the object ´c´ already has a [matching](#definition-matching) `apply` (or `unapply`) member, no new definition is added.
The definition of `apply` is omitted if class ´c´ is `abstract`.

If the case class definition contains an empty value parameter list, the `unapply` method returns a `Boolean` instead of an `Option` type and is defined as follows:

```scala
def unapply[´\mathit{tps}\,´](´x´: ´c´[´\mathit{tps}\,´]) = x ne null
```

The name of the `unapply` method is changed to `unapplySeq` if the first parameter section ´\mathit{ps}_1´ of ´c´ ends in a [repeated parameter](04-basic-definitions.html#repeated-parameters).

A method named `copy` is implicitly added to every case class unless the class already has a member (directly defined or inherited) with that name, or the class has a repeated parameter.
The method is defined as follows:

```scala
def copy[´\mathit{tps}\,´](´\mathit{ps}'_1\,´)...(´\mathit{ps}'_n´): ´c´[´\mathit{tps}\,´] = new ´c´[´\mathit{Ts}\,´](´\mathit{xs}_1\,´)...(´\mathit{xs}_n´)
```

Again, `´\mathit{Ts}´` stands for the vector of types defined in the type parameter section `´\mathit{tps}´` and each `´xs_i´` denotes the parameter names of the parameter section `´ps'_i´`.
The value parameters `´ps'_{1,j}´` of first parameter list have the form `´x_{1,j}´:´T_{1,j}´=this.´x_{1,j}´`, the other parameters `´ps'_{i,j}´` of the `copy` method are defined as `´x_{i,j}´:´T_{i,j}´`.
In all cases `´x_{i,j}´` and `´T_{i,j}´` refer to the name and type of the corresponding class parameter `´\mathit{ps}_{i,j}´`.

Every case class implicitly overrides some method definitions of class [`scala.AnyRef`](12-the-scala-standard-library.html#root-classes) unless a definition of the same method is already given in the case class itself or a concrete definition of the same method is given in some base class of the case class different from `AnyRef`.
In particular:

- Method `equals: (Any)Boolean` is structural equality, where two instances are equal if they both belong to the case class in question and they have equal (with respect to `equals`) constructor arguments (restricted to the class's _elements_, i.e., the first parameter section).
- Method `hashCode: Int` computes a hash-code. If the hashCode methods of the data structure members map equal (with respect to equals) values to equal hash-codes, then the case class hashCode method does too.
- Method `toString: String` returns a string representation which contains the name of the class and its elements.

###### Example
Here is the definition of abstract syntax for lambda calculus:

```scala
class Expr
case class Var   (x: String)          extends Expr
case class Apply (f: Expr, e: Expr)   extends Expr
case class Lambda(x: String, e: Expr) extends Expr
```

This defines a class `Expr` with case classes `Var`, `Apply` and `Lambda`. A call-by-value evaluator for lambda expressions could then be written as follows.

```scala
type Env = String => Value
case class Value(e: Expr, env: Env)

def eval(e: Expr, env: Env): Value = e match {
  case Var (x) =>
    env(x)
  case Apply(f, g) =>
    val Value(Lambda (x, e1), env1) = eval(f, env)
    val v = eval(g, env)
    eval (e1, (y => if (y == x) v else env1(y)))
  case Lambda(_, _) =>
    Value(e, env)
}
```

It is possible to define further case classes that extend type `Expr` in other parts of the program, for instance

```scala
case class Number(x: Int) extends Expr
```

This form of extensibility can be excluded by declaring the base class `Expr` `sealed`; in this case, all classes that directly extend `Expr` must be in the same source file as `Expr`.

## Traits

```ebnf
TmplDef          ::=  ‘trait’ ClassDef
```

A _trait_ is a class that is meant to be added to some other class as a mixin.
Furthermore, no constructor arguments are passed to the superclass of the trait.
This is not necessary as traits are initialized after the superclass is initialized.

Assume a trait ´D´ defines some aspect of an instance ´x´ of type ´C´ (i.e. ´D´ is a base class of ´C´).
Then the _actual supertype_ of ´D´ in ´x´ is the compound type consisting of all the base classes in ´\mathcal{L}(C)´ that succeed ´D´.
The actual supertype gives the context for resolving a [`super` reference](06-expressions.html#this-and-super) in a trait.
Note that the actual supertype depends on the type to which the trait is added in a mixin composition; it is not statically known at the time the trait is defined.

If ´D´ is not a trait, then its actual supertype is simply its least proper supertype (which is statically known).

###### Example
The following trait defines the property of being comparable to objects of some type.
It contains an abstract method `<` and default implementations of the other comparison operators `<=`, `>`, and `>=`.

```scala
trait Comparable[T <: Comparable[T]] { self: T =>
  def < (that: T): Boolean
  def <=(that: T): Boolean = this < that || this == that
  def > (that: T): Boolean = that < this
  def >=(that: T): Boolean = that <= this
}
```

###### Example
Consider an abstract class `Table` that implements maps from a type of keys `A` to a type of values `B`.
The class has a method `set` to enter a new key / value pair into the table, and a method `get` that returns an optional value matching a given key.
Finally, there is a method `apply` which is like `get`, except that it returns a given default value if the table is undefined for the given key.
This class is implemented as follows.

```scala
abstract class Table[A, B](defaultValue: B) {
  def get(key: A): Option[B]
  def set(key: A, value: B): Unit
  def apply(key: A) = get(key) match {
    case Some(value) => value
    case None => defaultValue
  }
}
```

Here is a concrete implementation of the `Table` class.

```scala
class ListTable[A, B](defaultValue: B) extends Table[A, B](defaultValue) {
  private var elems: List[(A, B)] = Nil
  def get(key: A) = elems.find(_._1 == key).map(_._2)
  def set(key: A, value: B) = { elems = (key, value) :: elems }
}
```

Here is a trait that prevents concurrent access to the `get` and `set` operations of its parent class:

```scala
trait SynchronizedTable[A, B] extends Table[A, B] {
  abstract override def get(key: A): B =
    synchronized { super.get(key) }
  abstract override def set(key: A, value: B) =
    synchronized { super.set(key, value) }
}
```

Note that `SynchronizedTable` does not pass an argument to its superclass, `Table`, even  though `Table` is defined with a formal parameter.
Note also that the `super` calls in `SynchronizedTable`'s `get` and `set` methods statically refer to abstract methods in class `Table`.
This is legal, as long as the calling method is labeled [`abstract override`](#modifiers).

Finally, the following mixin composition creates a synchronized list table with strings as keys and integers as values and with a default value `0`:

```scala
object MyTable extends ListTable[String, Int](0) with SynchronizedTable[String, Int]
```

The object `MyTable` inherits its `get` and `set` method from `SynchronizedTable`.
The `super` calls in these methods are re-bound to refer to the corresponding implementations in `ListTable`, which is the actual supertype of `SynchronizedTable` in `MyTable`.

### Extending parameterized traits

Extra rules apply for extending a trait with parameters:

1. If a class `´C´` extends a parameterized trait `´T´`, and its superclass does not, `´C´` _must_ pass arguments to `´T´`.

2. If a class `´C´` extends a parameterized trait `´T´`, and its superclass does as well, `´C´` _must not_  pass arguments to `´T´`.

3. Traits must never pass arguments to parent traits.

4. If a class `´C´` extends an unparameterized trait `´T_i´` and the base types of `´T_i´` include parameterized trait `´T_j´`, and the superclass of `´C´` does not extend `´T_j´`, then `´C´` _must_ also explicitly extend `´T_j´` and pass arguments.
This rule is relaxed if the missing trait contains only context parameters. In that case the trait reference is implicitly inserted as an additional parent with inferred arguments.

###### Example - Preventing ambiguities

The following listing tries to extend `Greeting` twice, with different parameters.

```scala
trait Greeting(val name: String):
  def msg = s"How are you, $name"

class C extends Greeting("Bob")

class D extends C, Greeting("Bill") // error

@main def greet = println(D().msg)
```

Should this program print "Bob" or "Bill"? In fact this program is illegal, because it violates rule 2 above.
Instead, `D` can extend `Greeting` without passing arguments.

###### Example - Overriding

Here's a variant of `Greeting` that overrides `msg`:
```scala
trait FormalGreeting extends Greeting:
  override def msg = s"How do you do, $name"
```

Due to rule 4, the following class extending `FormalGreeting` is required to also extend `Greeting` with arguments:
```scala
class GreetBobFormally extends FormalGreeting, Greeting("Bob")
```

###### Example - Inferred context parameters

Here's a variant of `Greeting` where the addressee is a context parameter of type `ImpliedName`:

```scala
trait ImpliedGreeting(using val iname: ImpliedName):
  def msg = s"How are you, $iname"

case class ImpliedName(name: String):
  override def toString = name

trait ImpliedFormalGreeting extends ImpliedGreeting:
  override def msg = s"How do you do, $iname"

class F(using iname: ImpliedName) extends ImpliedFormalGreeting
```

The definition of `F` in the last line is implicitly expanded to
```scala
class F(using iname: ImpliedName) extends
  Object, // implicitly inserted
  ImpliedGreeting(using iname), // implicitly inserted
  ImpliedFormalGreeting
```
Due to rule 4, `F` is required to also extend `ImpliedGreeting` and pass arguments to it, however note that because `ImpliedGreeting` has only context parameters the extension was added implicitly.

## Object Definitions

```ebnf
TmplDef         ::=  ‘object’ ObjectDef
ObjectDef       ::=  id ClassTemplate
```

An _object definition_ defines a single object of a new class.
Its most general form is `object ´m´ extends ´t´`.
Here, ´m´ is the name of the object to be defined, and ´t´ is a [template](#templates) of the form

```scala
´sc´ with ´mt_1´ with ... with ´mt_n´ { ´\mathit{stats}´ }
```

which defines the base classes, behavior and initial state of ´m´.
The extends clause `extends ´sc´ with ´mt_1´ with ... with ´mt_n´` can be omitted, in which case `extends scala.AnyRef` is assumed.
The class body `{ ´\mathit{stats}´ }` may also be omitted, in which case the empty body `{}` is assumed.

The object definition defines a single object (or: _module_) conforming to the template ´t´.
It is roughly equivalent to the following definition of a lazy value:

```scala
lazy val ´m´ = new ´sc´ with ´mt_1´ with ... with ´mt_n´ { this: ´m.type´ => ´\mathit{stats}´ }
```

Note that the value defined by an object definition is instantiated lazily.
The `new ´m´$cls` constructor is evaluated not at the point of the object definition, but is instead evaluated the first time ´m´ is dereferenced during execution of the program (which might be never at all).
An attempt to dereference ´m´ again during evaluation of the constructor will lead to an infinite loop or run-time error.
Other threads trying to dereference ´m´ while the constructor is being evaluated block until evaluation is complete.

The expansion given above is not accurate for top-level objects.
It cannot be because variable and method definition cannot appear on the top-level outside of a [package object](09-top-level-definitions.html#package-objects).
Instead, top-level objects are translated to static fields.

###### Example
Classes in Scala do not have static members; however, an equivalent effect can be achieved by an accompanying object definition E.g.

```scala
abstract class Point {
  val x: Double
  val y: Double
  def isOrigin = (x == 0.0 && y == 0.0)
}
object Point {
  val origin = new Point() { val x = 0.0; val y = 0.0 }
}
```

This defines a class `Point` and an object `Point` which contains `origin` as a member.
Note that the double use of the name `Point` is legal, since the class definition defines the name `Point` in the type name space, whereas the object definition defines a name in the term namespace.

This technique is applied by the Scala compiler when interpreting a Java class with static members.
Such a class ´C´ is conceptually seen as a pair of a Scala class that contains all instance members of ´C´ and a Scala object that contains all static members of ´C´.

Generally, a _companion module_ of a class is an object which has the same name as the class and is defined in the same scope and compilation unit.
Conversely, the class is called the _companion class_ of the module.

Very much like a concrete class definition, an object definition may still contain definitions of abstract type members, but not of abstract term members.

## Enum Definitions

<!-- TODO: Agree with NTs of rest of spec -->
```ebnf
TmplDef   ::=  ‘enum’ EnumDef
EnumDef   ::=  id ClassConstr [‘extends’ ConstrApps] EnumBody
EnumBody  ::=  [nl] ‘{’ [SelfType] EnumStat {semi EnumStat} ‘}’
EnumStat  ::=  TemplateStat
            |  {Annotation [nl]} {Modifier} EnumCase
EnumCase  ::=  ‘case’ (id ClassConstr [‘extends’ ConstrApps] | ids)
```

An _enum definition_ implies the definition of an _enum class_, a companion object, and one or more _enum cases_.

Enum definitions are useful to encode both Generalised Algebraic Data Types and Enumerated Types.

The compiler expands enum definitions to code that only uses Scala's other language features.
As such, enum definitions in Scala are convenient _syntactic sugar_, but they are not essential to understand Scala's core.

We now explain the expansion of enum definitions in detail.
First, some terminology and notational conventions:

- We use ´E´ as a name of an enum definition, and ´C´ as a name of an enum case that appears in ´E´.
- We use `<...>` for syntactic constructs that in some circumstances might be empty.
For instance, `<value-params>` represents one or more parameter lists `(´\mathit{ps}_1\,´)...(´\mathit{ps}_n´)` or nothing at all.
- Enum classes fall into two categories:
  - _parameterized_ enum classes have at least one or more (possibly empty) term parameter clauses, denoted as `(´\mathit{ps}_1\,´)...(´\mathit{ps}_n´)`.
  - _unparameterized_ enum classes have no term parameter clauses, but may optionally have a type parameter clause, denoted as `[´\mathit{tps}\,´]`.
- Enum cases fall into three categories:
  - _Class enum cases_ are those cases that possibly have a type parameter clause `[´\mathit{tps}\,´]`, and necessarily have one or more (possibly empty) parameter clauses `(´\mathit{ps}_1\,´)...(´\mathit{ps}_n´)`.
  - _Simple enum cases_ are those cases that have no parameter clauses and no extends clause.
  That is, they consist of a name only.
  - _Value enum cases_ are those cases that have no parameter clauses but that do have a (possibly generated) `extends` clause.

- Simple enum cases and value enum cases are collectively called _singleton enum cases_.

###### Example

An example enum for a `Planet` enumeration can be given as
```scala
enum Planet(mass: Double, radius: Double):
  case Mercury extends Planet(3.303e+23, 2.4397e6)
  case Venus   extends Planet(4.869e+24, 6.0518e6)
  case Earth   extends Planet(5.976e+24, 6.37814e6)
  case Mars    extends Planet(6.421e+23, 3.3972e6)
  case Jupiter extends Planet(1.9e+27,   7.1492e7)
  case Saturn  extends Planet(5.688e+26, 6.0268e7)
  case Uranus  extends Planet(8.686e+25, 2.5559e7)
  case Neptune extends Planet(1.024e+26, 2.4746e7)

  private inline val G = 6.67300E-11
  def surfaceGravity = G * mass / (radius * radius)
  def surfaceWeight(otherMass: Double) = otherMass * surfaceGravity
end Planet
```

###### Example

An example enum for the Option ADT can be given as
```scala
enum Option[+T]:
  case Some(x: T)
  case None
```

### Lowering of Enum Definitions

###### Summary
An enum class is represented as a `sealed abstract` class that extends the `scala.reflect.Enum` trait.

Enum cases are represented as follows:
- a class enum case is mapped to a `case class` member of the enum class' companion object,
- a singleton enum case is mapped to a `val` member of the enum class' companion object, implemented by a local class definition. Whether that local class is shared with other singleton cases, and which ones, is left as an implementation detail.

###### Precise rules
The `scala.reflect.Enum` trait defines a single public method, `ordinal`:
```scala
package scala.reflect

transparent trait Enum extends Any, Product, Serializable:

  def ordinal: Int
```
There are nine desugaring rules.
Rule (1) desugars enum definitions.
Rule (2) desugars cases of comma-separated names to simple enum cases.
Rules (3) to (7) desugar inferrable details of enum cases.
Rules (8) and (9) define how fully-desugared enum cases map into `case class`es or `val`s.
Explicit `extends` clauses must be provided in the following cases, where rules (2) to (6) do not apply:
- any enum case of a parameterized enum,
- any singleton enum case of an unparameterized enum with non-variant type parameters,
- any class enum case of an enum with type parameters, where the case also has type parameters.

1.  An `enum` definition
    ```scala
    enum ´E´ <type-params> <value-params> extends <parents> { <defs> <cases> }
    ```
    expands to a `sealed abstract` class that extends the `scala.reflect.Enum` trait and an associated companion object that contains the defined cases, expanded according to rules (2 - 8).
    The enum class starts with a compiler-generated import that imports the names `<caseIds>` of all cases so that they can be used without prefix in the class.
    ```scala
    sealed abstract class ´E´ <type-params> <value-params>
        extends <parents> with scala.reflect.Enum {
      import ´E´.{ <caseIds> }
      <defs>
    }
    object ´E´ { <cases> }
    ```

2. A simple enum case consisting of a comma-separated list of names
   ```scala
   case ´C_1´, ..., ´C_n´
   ```
   expands to the following simple enum cases
   ```scala
   case ´C_1´; ...; case ´C_n´
   ```
   Any modifiers or annotations on the original case extend to all expanded cases.
   <p>This result is then further rewritten by either (3 or 4).</p>

3. A simple enum case `´C´` of an unparameterized enum `´E´` without type parameters
   ```scala
   case ´C´
   ```
  expands to the following value enum case:
   ```scala
   case ´C´ extends ´E´
   ```
   This result is then further rewritten with rule (8).

4. A simple enum case  `´C´` of an unparameterized enum `´E´[´\mathit{tps}´]` with type parameters
   ```scala
   case ´C´
   ```
   where `´\mathit{tps}´` are of the following form
   ```scala
   ´\mathit{v}_1´ ´T_1´ >: ´L_1´ <: ´U_1´ ,   ... ,   ´\mathit{v}_n´ ´T_n´ >: ´L_n´ <: ´U_n´      (n > 0)
   ```
   and where each of the variances `´\mathit{v}_i´` is either `'+'` or `'-'`, expands to the following value enum case:
   ```scala
   case ´C´ extends ´E´[´B_1´, ..., ´B_n´]
   ```
   where `´B_i´` is `´L_i´` if `´\mathit{v}_i´ = '+'` and `´U_i´` if `´\mathit{v}_i´ = '-'`.
   <p>This result is then further rewritten with rule (8).</p>

5. A class enum case with type parameters, but without an extends clause
   ```scala
   case ´C´[´\mathit{tps}´](´\mathit{ps}_1\,´)...(´\mathit{ps}_n´)
   ```
   of an unparameterized enum `´E´` without type parameters expands to
   ```scala
   case ´C´[´\mathit{tps}´](´\mathit{ps}_1\,´)...(´\mathit{ps}_n´) extends ´E´
   ```
   This result is then further rewritten with rule (9).

6. A class enum case without type parameters or an extends clause
   ```scala
   case ´C´(´\mathit{ps}_1\,´)...(´\mathit{ps}_n´)
   ```
   of an unparameterized enum `´E´[´\mathit{tps}´]` with type parameters expands to
   ```scala
   case ´C´(´\mathit{ps}_1\,´)...(´\mathit{ps}_n´) extends ´E´[´\mathit{tps}´]
   ```
   This result is then further rewritten with rule (7).

7. A class enum case without type parameters, but has an extends clause
   ```scala
   case ´C´(´\mathit{ps}_1\,´)...(´\mathit{ps}_n´) extends <parents>
   ```
   of an enum `´E´[´\mathit{tps}´]` with type parameters expands to
   ```scala
   case ´C´[´\mathit{tps}´](´\mathit{ps}_1\,´)...(´\mathit{ps}_n´) extends <parents>
   ```
   provided at least one of the parameters `´\mathit{tps}´` is mentioned in a parameter type in `(´\mathit{ps}_1\,´)...(´\mathit{ps}_n´)` or in a type argument in `<parents>`.
   <br/><br/>
   This result is then further rewritten with rule (9).

8. A singleton enum case
   ```scala
   case ´C´ extends <parents>
   ```
   expands to the following `val` definition in `´E´`'s companion object:
   ```scala
   val ´C´ = $factory(_$ordinal = ´\mathit{n}´, $name = "C")
   ```
   where `´\mathit{n}´` is the ordinal number of the case in the companion object, starting from 0.
   `$factory` is a placeholder that expands its arguments into an expression that produces something equivalent to
   a new instance of the following (possibly shared) anonymous class:
   ```scala
   new <parents> {
      def ordinal: Int = _$ordinal
      override def toString: String = $name
   }
   ```
   The anonymous class also implements the abstract `Product` methods that it inherits from `Enum`.
   <br/><br/>
   **NOTE:** It is an error if a value case refers to a type parameter of `´E´` in a type argument within `<parents>`.

9. A class enum case
   ```scala
   case ´C´ <type-params> <value-params> extends <parents>
   ```
   expands analogous to a final case class in `´E´`'s companion object:
   ```scala
   final case class ´C´ <type-params> <value-params> extends <parents> {
      def ordinal = ´\mathit{n}´
   }
   ```
   where `´\mathit{n}´` is the ordinal number of the case in the companion object, starting from 0.
   <br/><br/>
   **NOTE:** It is an error if a class case refers to a type parameter of `´E´` in a parameter type in `<type-params>` or `<value-params>` or in a type argument of `<parents>`, unless that parameter is already a type parameter of the case, i.e. the parameter name is defined in `<type-params>`.

###### Superclass of an enum case

an enum case (singleton or class) with explicit extends clause
```scala
case ´C´ <type-params> <value-params> extends <parents>
```

must extend the parent enum `´E´` as the first parent of `<parents>`.

###### Example
Consider the enumeration `RGB`, consisting of simple enum cases:
```scala
enum RGB:
  case Red, Green, Blue
```

The three simple cases will expand as follows in the companion of `RGB`:

```scala
val Red = $new(0, "Red")
val Green = $new(1, "Green")
val Blue = $new(2, "Blue")

private def $new(_$ordinal: Int, $name: String) =
  new RGB with scala.runtime.EnumValue:
    def ordinal = _$ordinal
    override def productPrefix = $name
    override def toString = $name
```

### Widening of enum cases post-construction
The compiler-generated `apply` and `copy` methods of an class enum case
```scala
case ´C´[´\mathit{tps}\,´](´\mathit{ps}_1\,´)...(´\mathit{ps}_n´) extends ´P_1´, ..., ´P_n´
```
are treated specially.
A call `´C´[´\mathit{tps}\,´](´\mathit{ps}_1\,´)...(´\mathit{ps}_n´)` of the `apply` method is ascribed the underlying type `´P_1´ & ... & ´P_n´` (dropping any [transparent traits](../other-new-features/transparent-traits.md)) as long as that type is still compatible with the expected type at the point of application.
A call `t.copy[´\mathit{tps}\,´](´\mathit{ps}_1\,´)...(´\mathit{ps}_n´)` of `´C´`'s `copy` method is treated in the same way.

### Translation of enums with only singleton cases

An enum `´E´` (possibly generic) that defines one or more singleton cases, and no class cases will define the following additional synthetic members in its companion object (where `´E'´` denotes `´E´` with any type parameters replaced by wildcards):

   - A method `valueOf(name: String): ´E'´`.
   It returns the singleton case value whose identifier is `name`.
   - A method `values` which returns an `Array[´E'´]` of all singleton case values defined by `E`, in the order of their definitions.

### Translation of Java-compatible enums

A Java-compatible enum is an enum that extends `java.lang.Enum`.
The translation rules are the same as above, with the reservations defined in this section.

- It is a compile-time error for a Java-compatible enum to have class cases.

- Cases such as `case C` expand to a `@static val` as opposed to a `val`.
This allows them to be generated as static fields of the enum type, thus ensuring they are represented the same way as Java enums.

### Scopes for Enum Cases

A case in an `enum` is treated similarly to a secondary constructor.
It can access neither the enclosing `enum` using `this`, nor its value parameters or instance members using simple identifiers.

Even though translated enum cases are located in the enum's companion object, referencing this object or its members via `this` or a simple identifier is also illegal.
The compiler typechecks enum cases in the scope of the enclosing companion object but flags any such illegal accesses as errors.

### Variance for Type Parameters

A parameterized enum case ´C´  of enum ´E´ with _inferred_ type parameters will copy variance annotations.
e.g. type parameter ´T_{i}´ from ´E´ will have the same variance as type parameter `´T'_{i}´` in ´C´.

###### Example

The following enum `View` has a contravariant type parameter ´T´ and a single case `Refl`, representing a function mapping a type `T` to itself:

```scala
enum View[-´T´]:
  case Refl(f: ´T´ => ´T´)
```

`Refl` expands to the following enum:

```scala
enum View[-´T´]:
  case Refl[-´T'´](f: ´T'´ => ´T'´) extends View[´T'´]
```

The definition of `Refl` is incorrectly typed, as it uses contravariant type `´T'´` in the covariant result position of a function type.

A correctly typed version would use an _explicit_, _invariant_ type parameter `´R´` on case `Refl`:

```scala
enum View[-´T´]:
  case Refl[´R´](f: ´R´ => ´R´) extends View[´R´]
```
