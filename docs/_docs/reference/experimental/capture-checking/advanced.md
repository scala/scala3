---
layout: doc-page
title: "Capability Polymorphism"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/capture-checking/advanced.html
---

Advanced use cases.

```scala sc-hidden sc-name:advanced-cc-context
import language.experimental.captureChecking
import caps.*
```

```scala sc-hidden sc-name:advanced-access-context sc-compile-with:advanced-cc-context
trait Logger:
  def log(msg: String): Unit

trait Channel[A]:
  def send(x: A): Unit
  def recv(): A
```

## Access Control
Analogously to type parameters, we can give capability parameters bounds in form of concrete capture sets:
```scala sc:fail sc-compile-with:advanced-access-context
trait AccessControl:
  def runSecure[C^ <: {trusted}](block: () ->{C} Unit): Unit

  // This is a 'brand' capability to mark what can be mentioned in trusted code
  object trusted extends SharedCapability

  val trustedLogger: Logger^{trusted}
  val trustedChannel: Channel[String]^{trusted}
  val untrustedLogger: Logger^
  val untrustedChannel: Channel[String]^

  def main() =
    runSecure: () => // ok
      trustedLogger.log("Hello from trusted code") // ok

    runSecure: () => // ok
      trustedChannel.send("I can send")            // ok
      trustedLogger.log(trustedChannel.recv())     // ok

    runSecure: () =>
      ()                                           // ok

  def mainFail() =
    runSecure: () => // error
      untrustedLogger.log("I can't be used")

    runSecure: () => // error
      untrustedChannel.send("I can't be used")
```
The idea is that every capability derived from the marker capability `trusted` (and only those) are eligible to be used in the `block` closure
passed to `runSecure`. We can enforce this by an explicit capability parameter `C` constraining the possible captures of `block` to be included in `{trusted}`.

Note that since capabilities of function types are covariant, we could have equivalently specified `runSecure`'s signature using implicit capture polymorphism to achieve the same behavior:
```scala sc-compile-with:advanced-cc-context
trait API:
  object trusted extends SharedCapability
  def runSecure(block: () ->{trusted} Unit): Unit
```

## Capture-safe Lexical Control

Capability members and paths to these members can prevent leakage
of labels for lexically-delimited control operators:
```scala sc:fail sc-compile-with:advanced-cc-context
trait Label extends SharedCapability:
  type Fv^ // the capability set occurring freely in the `block` passed to `boundary` below.

def boundary[T, C^](block: Label{type Fv = {C} } ->{C} T): T = ??? // ensure free capabilities of label and block match
def suspend[U](label: Label)[D^ <: {label.Fv}](handler: () ->{D} U): U = ??? // may only capture the free capabilities of label

def test =
  val x = 1
  boundary: outer =>
    val y = 2
    boundary: inner =>
      val z = 3
      val w = suspend(outer) {() => z} // ok
      val v = suspend(inner) {() => y} // ok
      val u = suspend(inner): () =>
        suspend(outer) {() => w + v} // ok
        y
      suspend(outer): () => // error (would leak the inner label)
        println(inner)
        x + y + z
```
A key property is that `suspend` (think `shift` from delimited continuations) targeting a specific label (such as `outer`) should not accidentally close over labels from a nested `boundary` (such as `inner`), because they would escape their defining scope this way.
By leveraging capability polymorphism, capability members, and path-dependent capabilities, we can prevent such leaks from occurring at compile time:

* `Label`s store the free capabilities `C` of the `block` passed to `boundary` in their capability member `Fv`.
* When suspending on a given label, the suspension handler can capture at most the capabilities that occur freely at the `boundary` that introduced the label. That prevents mentioning nested bound labels.

[Back to Capability Polymorphism](polymorphism.md)