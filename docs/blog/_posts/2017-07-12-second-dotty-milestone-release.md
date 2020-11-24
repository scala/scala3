---
layout: blog-page
title: Announcing Dotty 0.2.0-RC1, with new optimizations, improved stability and IDE support
author: Dmytro Petrashko
authorImg: images/petrashko.jpg
date: 2017-07-12
---

Today, we are excited to release Dotty version 0.2.0-RC1. This release
serves as a technology preview that demonstrates new language features
and the compiler supporting them.

This release is based on the [previous milestone](/blog/_posts/2017-05-31-first-dotty-milestone-release.html).
The highlights of this release are:
 - substantial improvement of quality of generated code for pattern matching
 - improvements in VS Code IDE stability
 - support Windows in VS Code IDE
 - improved compatibility with scalac
 - initial support for reproducible builds


<!--more-->

This is our second scheduled release according to our [6-week release schedule](https://dotty.epfl.ch/docs/usage/version-numbers.html).

## What’s in the 0.2.0-RC1 technology preview?
The [previous technology preview](/blog/_posts/2017-05-31-first-dotty-milestone-release.html) has shipped new language features planned for Scala 3:
[Intersection Types](https://dotty.epfl.ch/docs/reference/new-types/intersection-types.html),
[Union Types](https://dotty.epfl.ch/docs/reference/new-types/union-types.html),
[Trait Parameters](https://dotty.epfl.ch/docs/reference/other-new-features/trait-parameters.html),
[Enumerations](https://dotty.epfl.ch/docs/reference/enums/enums.html),
[Algebraic Data Types](https://dotty.epfl.ch/docs/reference/enums/adts.html),
[By-Name Implicits](https://dotty.epfl.ch/docs/reference/other-new-features/implicit-by-name-parameters.html).

This technology preview is geared towards improving stability and reliability. It includes:

  - [Local optimizations upstreamed from the Dotty Linker](https://github.com/lampepfl/dotty/pull/2513), [2647](https://github.com/lampepfl/dotty/pull/2647) by ([@OlivierBlanvillain](https://github.com/OlivierBlanvillain)). See more details below.
  - [Optimizing Pattern Matcher](https://github.com/lampepfl/dotty/pull/2829) by ([@odersky](https://github.com/odersky))
  - [Idempotency checks](https://github.com/lampepfl/dotty/pull/2756) are the first step to reproducible builds
  - [Faster Base class sets](https://github.com/lampepfl/dotty/pull/2676) by ([@odersky](https://github.com/odersky)) and ([@darkdimius](https://twitter.com/darkdimius))
  - Numerous fixes to IDE and Dotty Language Server covering:

     - [Windows support for VS Code plugin](https://github.com/lampepfl/dotty/pull/2776)
     - [Fix hover-on-type for implicitly converted expressions](https://github.com/lampepfl/dotty/pull/2836)
     - [Fixes to find all references in external projects](https://github.com/lampepfl/dotty/pull/2810), [2773](https://github.com/lampepfl/dotty/pull/2773/files)
     - [Fix conflict with  dragos-vscode-scala](https://github.com/lampepfl/dotty/pull/2777)
     - [Fix ide crash on non-parsable file](https://github.com/lampepfl/dotty/pull/2752)
     - [Fix hover functionality for enum classes](https://github.com/lampepfl/dotty/pull/2722)
     - [Report errors on Dotty Language Server initialization](https://github.com/lampepfl/dotty/pull/2708)
     - [Fixes to sbt setting up Dotty IDE](https://github.com/lampepfl/dotty/pull/2690)
     - General stability improvements [2838](https://github.com/lampepfl/dotty/pull/2838), [2787](https://github.com/lampepfl/dotty/pull/2787), [2692](https://github.com/lampepfl/dotty/pull/2692)

  - Scalac compatibility improvements:

     - [Support Scala 2.12 traits](https://github.com/lampepfl/dotty/pull/2685)
     - [Fixes to handling of Scala 2 classfiles](https://github.com/lampepfl/dotty/pull/2834/files)
     - [Scalac parser crashes on Dotty.jar](https://github.com/lampepfl/dotty/pull/2719)

  - Java compatibility improvements:

     - [Fixes to handing of Java generic signatures](https://github.com/lampepfl/dotty/pull/2831)
     - [java.lang.System.out is final but that's a lie](https://github.com/lampepfl/dotty/pull/2781)

  - Improved error messages:

     - [Nicer error message for "implicit function type needs non-empty parameter list"](https://github.com/lampepfl/dotty/pull/2821)
     - [Nicer error message for nonsensical modifier combination](https://github.com/lampepfl/dotty/pull/2807/files), [2747](https://github.com/lampepfl/dotty/pull/2747)
     - [Nicer error message for supercall inside @inline method](https://github.com/lampepfl/dotty/pull/2740)
     - [Check that case classes don't inherit case classes](https://github.com/lampepfl/dotty/pull/2790)
     - [Check that named parameters don't conflict with positional ones](https://github.com/lampepfl/dotty/pull/2785)

  - Improved command line handling:

     - [Support params in a file like @file.txt](https://github.com/lampepfl/dotty/pull/2765)

  - Type system stability:

     - [Handle wildcard types in unions and intersections](https://github.com/lampepfl/dotty/pull/2742)

  - Fixes to implicit search:

     - [Fix shadowing of higher order implicits](https://github.com/lampepfl/dotty/pull/2739)


### Better generated code:

As was [spotted](https://twitter.com/gkossakowski/status/870243464528744449) by [@gkossakowski](https://twitter.com/gkossakowski)
in the previous release Dotty was on par with Scala 2.11 in speed. But why is that?
The reason is that Dotty compiled by Dotty had really horrible code generated for pattern matching.

Let's illustrate on a simple example:

```scala
case class CC(a: Int, b: Object)

  def foo(x: Any): Int = {
    val (a, b) = x match {
      case CC(s @ 1, CC(t, _)) =>
        (s , 2)
      case _ => (42, 43)
    }
    a + b
  }

  def booleans(a: Object) = {
    val (b1, b2) = (a.isInstanceOf[CC], a.isInstanceOf[List[Int]])
    (b1, b2) match {
      case (true, true) => true
      case (false, false) => true
      case _ => false
    }
  }
```


The Dotty that was released in the previous milestone didn't contain any optimizations and generated inefficient code for it.
The java-with-goto code below is equivalent to what Dotty generated.

```java
// output of dotc 0.1.2-RC1
    public int foo(Object x) {
        var3_2 = x;
        if (!(var3_2 instanceof CC)) ** GOTO lbl-1000
        var4_3 = (CC)var3_2;
        if (CC$.MODULE$.unapply((CC)var3_2) == null) ** GOTO lbl-1000
        var5_4 = CC$.MODULE$.unapply((CC)var3_2);
        s = var5_4._1();
        var7_6 = var5_4._2();
        if (1 != s) ** GOTO lbl-1000
        var8_7 = s;
        if (!(var7_6 instanceof CC)) ** GOTO lbl-1000
        var9_8 = (CC)var7_6;
        if (CC$.MODULE$.unapply((CC)var7_6) != null) {
            var10_9 = CC$.MODULE$.unapply((CC)var7_6);
            var11_10 = var10_9._2();
            v0 = Tuple2..MODULE$.apply((Object)BoxesRunTime.boxToInteger((int)1), (Object)BoxesRunTime.boxToInteger((int)2));
        } else lbl-1000: // 5 sources:
        {
            v0 = Tuple2..MODULE$.apply((Object)BoxesRunTime.boxToInteger((int)42), (Object)BoxesRunTime.boxToInteger((int)43));
        }
        var2_11 = v0;
        a = BoxesRunTime.unboxToInt((Object)var2_11._1());
        b = BoxesRunTime.unboxToInt((Object)var2_11._2());
        return a + b;
    }

    public boolean booleans(Object a) {
        Tuple2 tuple2 = Tuple2..MODULE$.apply((Object)BoxesRunTime.boxToBoolean((boolean)(a instanceof CC)), (Object)BoxesRunTime.boxToBoolean((boolean)(a instanceof List)));
        boolean b1 = BoxesRunTime.unboxToBoolean((Object)tuple2._1());
        boolean b2 = BoxesRunTime.unboxToBoolean((Object)tuple2._2());
        Tuple2 tuple22 = Tuple2..MODULE$.apply((Object)BoxesRunTime.boxToBoolean((boolean)b1), (Object)BoxesRunTime.boxToBoolean((boolean)b2));
        Option option = Tuple2..MODULE$.unapply(tuple22);
        if (!option.isEmpty()) {
            Tuple2 tuple23 = (Tuple2)option.get();
            boolean bl = BoxesRunTime.unboxToBoolean((Object)tuple23._1());
            boolean bl2 = BoxesRunTime.unboxToBoolean((Object)tuple23._2());
            if (bl) {
                boolean bl3 = bl;
                if (bl2) {
                    boolean bl4 = bl2;
                    return true;
                }
            }
        }
        Option option2 = Tuple2..MODULE$.unapply(tuple22);
        if (option2.isEmpty()) return false;
        Tuple2 tuple24 = (Tuple2)option2.get();
        boolean bl = BoxesRunTime.unboxToBoolean((Object)tuple24._1());
        boolean bl5 = BoxesRunTime.unboxToBoolean((Object)tuple24._2());
        if (bl) return false;
        boolean bl6 = bl;
        if (bl5) return false;
        boolean bl7 = bl5;
        return true;
    }
```

Due to the new optimizing pattern matcher, Dotty now is able to generate the code below without `-optimise`

```java
// output of 0.2.0-RC1 without -optimise
    public int foo(Object x) {
        var3_2 = x;
        if (!(var3_2 instanceof CC)) ** GOTO lbl-1000
        var4_3 = CC$.MODULE$.unapply((CC)var3_2);
        s = var5_4 = var4_3._1();
        if (1 == var5_4 && (var7_6 = var4_3._2()) instanceof CC) {
            t = CC$.MODULE$.unapply((CC)var7_6)._1();
            v0 = Tuple2..MODULE$.apply((Object)BoxesRunTime.boxToInteger((int)1), (Object)BoxesRunTime.boxToInteger((int)2));
        } else lbl-1000: // 2 sources:
        {
            v0 = Tuple2..MODULE$.apply((Object)BoxesRunTime.boxToInteger((int)42), (Object)BoxesRunTime.boxToInteger((int)43));
        }
        var2_8 = v0;
        a = BoxesRunTime.unboxToInt((Object)var2_8._1());
        b = BoxesRunTime.unboxToInt((Object)var2_8._2());
        return a + b;
    }

    public boolean booleans(Object a) {
        Tuple2 tuple2 = Tuple2..MODULE$.apply((Object)BoxesRunTime.boxToBoolean((boolean)(a instanceof CC)), (Object)BoxesRunTime.boxToBoolean((boolean)(a instanceof List)));
        boolean b1 = BoxesRunTime.unboxToBoolean((Object)tuple2._1());
        boolean b2 = BoxesRunTime.unboxToBoolean((Object)tuple2._2());
        Tuple2 tuple22 = Tuple2..MODULE$.apply((Object)BoxesRunTime.boxToBoolean((boolean)b1), (Object)BoxesRunTime.boxToBoolean((boolean)b2));
        if (tuple22 != null) {
            boolean bl;
            boolean bl2 = BoxesRunTime.unboxToBoolean((Object)tuple22._1());
            if (!bl2) {
                bl = bl2;
            } else {
                if (BoxesRunTime.unboxToBoolean((Object)tuple22._2())) {
                    return true;
                }
                bl = bl2;
            }
            if (!bl) {
                if (false != BoxesRunTime.unboxToBoolean((Object)tuple22._2())) return false;
                return true;
            }
        }
        return false;
    }
```

You can clearly see that it's shorter ;-) and it actually does less work.
If you additionally enable local optimizations, you get decent generated code:

```java
// output of 0.2.0-RC1 with -optimise

    public int foo(Object x) {
        int n;
        Tuple2 tuple2;
        CC cC;
        Object object;
        if (x instanceof CC && 1 == (n = (cC = (CC)x)._1()) && (object = cC._2()) instanceof CC) {
            ((CC)object)._1();
            tuple2 = new Tuple2((Object)BoxesRunTime.boxToInteger((int)1), (Object)BoxesRunTime.boxToInteger((int)2));
        } else {
            tuple2 = new Tuple2((Object)BoxesRunTime.boxToInteger((int)42), (Object)BoxesRunTime.boxToInteger((int)43));
        }
        Tuple2 tuple22 = tuple2;
        return BoxesRunTime.unboxToInt((Object)tuple22._1()) + BoxesRunTime.unboxToInt((Object)tuple22._2());
    }

    public boolean booleans(Object a) {
        boolean bl = a instanceof CC;
        boolean bl2 = a instanceof List;
        new Tuple2((Object)BoxesRunTime.boxToBoolean((boolean)bl), (Object)BoxesRunTime.boxToBoolean((boolean)bl2));
        new Tuple2((Object)BoxesRunTime.boxToBoolean((boolean)bl), (Object)BoxesRunTime.boxToBoolean((boolean)bl2));
        if (bl && bl2) {
            return true;
        }
        boolean bl3 = bl;
        if (bl3) return false;
        if (bl2) return false;
        return true;
    }
```

This code still has a major inefficiency; it allocates tuples.
We plan to continue the migration of local optimizations from the Dotty Linker that should allow us to generate code that is as
good the code generated by the Dotty Linker with global analysis disabled:

```java
  // output of Dotty linker https://github.com/dotty-linker/dotty/tree/opto
    public int foo(Object x) {
         CC cC;
         int n = 0;
         int n2 = 0;
         if (x instanceof CC && 1 == (cC = (CC)x)._1() && cC._2() instanceof CC) {
             n = 1;
             n2 = 2;
         } else {
             n = 42;
             n2 = 43;
         }
         return n + n2;
     }

     public boolean booleans(Object a) {
         boolean bl = a instanceof CC;
         boolean bl2 = a instanceof List;
         if (bl && bl2 || !bl && !bl2) {
             return true;
         }
         return false;
 }
```

## How can you try it out?
We ship with tools that help you try out the Dotty platform:

  - [IDE features for Visual Studio Code](https://dotty.epfl.ch/docs/usage/ide-support.html)
  - [sbt support, including retro-compatibility with Scala 2](https://github.com/lampepfl/dotty-example-project)


You have several alternatives; use the `sbt-dotty` plugin, get a standalone
installation, or try it online on [Scastie].

### sbt
Using sbt 0.13.13 or newer, do:

```
sbt new lampepfl/dotty.g8
```

This will setup a new sbt project with Dotty as compiler. For more details on
using Dotty with sbt, see the
[example project](https://github.com/lampepfl/dotty-example-project).

### Standalone installation

Releases are available for download on the _Releases_
section of the Dotty repository:
[https://github.com/lampepfl/dotty/releases](https://github.com/lampepfl/dotty/releases)

We also provide a [homebrew](https://brew.sh/) package that can be installed by running:

```
brew install lampepfl/brew/dotty
```

In case you have already installed Dotty via brew, you should instead update it:

```
brew upgrade dotty
```

### Scastie

[Scastie], the online Scala playground,
supports Dotty.
You can try it out there without installing anything.


## What are the next steps?

Over the coming weeks and months, we plan to work on the following topics:

 - [Add support for using Dotty generated classes with Scala 2.12](https://github.com/lampepfl/dotty/pull/2827)
 - [Add Language-level support for HMaps and HLists](https://github.com/lampepfl/dotty/pull/2199);
 - Upstream more optimizations from Dotty Linker
 - [Add support for existing in the same classpath with Scala 2.12](https://github.com/lampepfl/dotty/pull/2827)

If you want to get your hands dirty with any of this, now is a good
moment to get involved! Join the team of contributors, including
Martin Odersky ([@odersky](https://twitter.com/odersky))
Dmitry Petrashko ([@DarkDimius](https://twitter.com/DarkDimius)),
Guillaume Martres ([@smarter](https://github.com/smarter)),
Felix Mulder ([@felixmulder](https://twitter.com/felixmulder)),
Nicolas Stucki ([@nicolasstucki](https://github.com/nicolasstucki)),
Liu Fengyun ([@liufengyun](https://github.com/liufengyun)),
Olivier Blanvillain ([@OlivierBlanvillain](https://github.com/OlivierBlanvillain)),
and others!

## Library authors: Join our community build

Dotty now has a set of libraries that are built against every nightly snapshot.
Currently this includes scalatest, squants and algebra.
Join our [community build](https://github.com/lampepfl/dotty-community-build)
 to make sure that our regression suite includes your library.


To get started, see [https://github.com/lampepfl/dotty](https://github.com/lampepfl/dotty).


[Scastie]: https://scastie.scala-lang.org/?target=dotty
