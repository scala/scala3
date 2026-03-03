---
layout: doc-page
title: "Safe Mode"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/safe.html
---

## Introduction

Safe mode is an extension of capture checking that enforces that a program is written in a capability-safe language subset that makes sure that tracked capabilities cannot be forgotten.

Full Scala 3 has elements that are incompatible with capability safety, such as type casts and other unsafe features. These are essential escape hatches in some situations. But they should be used with care, and should not be available in agent-generated code or other untrusted code that does not pass review.

To distinguish between these two usage modes, there is a safe language subset that can be specified with a command-line option or a language import:
```scala sc:nocompile
  import language.experimental.safe
```

It makes sense for agentic tooling to subject all compilations of agent-generated code to be compiled in _safe mode_ using this language import. Safe mode imposes the following restrictions:

  1. No unchecked type casts or pattern matches.
  1. No use of features from the `caps.unsafe` module.
  1. No use of `@unchecked` annotations and variants thereof
  1. No access to runtime reflection.
  1. Compile with capture checking enabled, including tracking all mutation effects.
  1. Can access global objects only if they are implemented safely themselves.

One needs to disallow unchecked casts or pattern matches since these might ``forget'' retained capabilities (Point 1 above).
The same holds for features from the `caps.unsafe` module (2), uses of `@unchecked...` annotations (3), and uses of runtime reflection (4).
One needs to make sure all capabilities are tracked in types, in particular to guarantee absence of any retained capabilities in pure functions (5).
Finally, one needs to prevent the introduction of untracked effects through library calls. That's why a library module can be accessed only if it is also implemented safely (6).

One way to implement a safe library module is to compile it under safe mode.
But one should also allow library modules that do allow some unsafe language features as long as side effects are not observable.
For instance a library module might implement a cache for function results, which uses an untracked mutable variable by annotating it with the `@untrackedCaptures` annotation, which comes from the `caps.unsafe` module. Normally, such an access would be disallowed in safe mode, but it could be permitted if one can verify by other means that the module is safe. In the concrete case of a cache, one would have to verify that the variable only holds results of previous function calls and that the called function is referentially transparent. Such verification could be done informally or formally by hand, or with a machine assisted formal proof.
Once the verification
is done, the library module can be made available for use by safe code.

This scheme is supported by a new `@assumeSafe` annotation, available in
module `caps`.
Modules tagged with this annotation are assumed to be callable from  agent-generated code. `@assumeSafe` comes with none of the restrictions that `safe` implies. Instead it is the obligation of the programmer to verify that the module is indeed safe. For instance caching a function results could be implemented like this:
```scala sc:nocompile
import caps.unsafe.untrackedCaptures
import caps.assumeSafe
import scala.collection.mutable.HashMap

@assumeSafe
class Memoized[A, B](f: A -> B) {

  @untrackedCaptures
  private val cached = HashMap[A, B]()

  def apply(x: A) = cached.getOrElseUpdate(x, f(x))
}
```
Or, here is an outline of an email function that prompts a user for confirmation before
sending. Here, we assume that the `Mailer` object is neither safe nor assumed safe.
Agents can still send email through `CheckedMailer`, but only after user confirmation.
```scala sc:nocompile
import caps.assumeSafe

@assumeSafe
object CheckedMailer {

  def sendMail(email: Email) =
    if userPrompt(s"OK to send email?\n\n$email") then
      Mailer.send(email)
}
```
There's also the `@rejectSafe` annotation in `caps`, which can be seen to be a dual to `@assumeSafe`. It renders selected members of assumed safe components inaccessible in safe mode.

Safe mode makes available a subset of the standard library, which is assumed safe. This subset is currently defined by the compiler itself, in the `dotty.tools.dotc.cc.SafeRefs` module.
It includes all of the `scala` package, with the exception of global `print` functions, number classes in the `java.lang` classes, as well as `Enum`, `String`, `Object`. It also includes `java.lang.Class`, but rejects access to all its member methods that implement runtime reflection.
The permissible subset is expected to evolve over time, as more experience with safe mode is gained.

One aspect worth noting is that the restrictions on library calls only apply to classes and objects in the global scope. Capabilities passed as parameters to untrusted code can come from any module, whether it is safe or unsafe. Here it is the job of the API designer to only expose appropriate functionality in these capabilities.

### Control Effects

Note that the restrictions imposed by safe mode do not rule out throwing exceptions. This is the case even though exceptions are a form of side effect that can leak information. The reason for this gap in coverage is that exceptions are so common that it's impractical to control them all in a static type system. It's simply too cumbersome to demand extra `CanThrow` capabilities for exceptions like buffer overflows, stack overflows, divisions by zero, or out of memory events. Rather than trying to track all these conditions in capabilities, we rely on runtime protection. Specifically, calls to untrusted code can be wrapped in a `Try`, which converts any thrown exception to a value that can be inspected by the caller. As an added safety measure, any leakage of other capabilities as fields of exceptions is also prevented since exception classes are treated as pure.

There are also other control effects similar to exceptions, for instance non-local returns or breaks, or resumptions of continuations. These effects can leak information as well. But in Scala 3, they are all ultimately represented as exceptions, so the `Try` runtime containment scheme applies to them as well.
