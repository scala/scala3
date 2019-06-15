# Package scalaShadowing
## Members:
<pre><code class="language-scala" >final object <a href="./language$.md">language</a></pre></code>
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
* [`experimental`](../scalaShadowing/language$.md#experimental)        contains newer features that have not yet been tested in production

and, for dotty:
* [`Scala2`]               backwards compatibility mode for Scala2](../scalaShadowing/language$.md#Scala2)
* `noAutoTupling`       disable auto-tupling
* [`strictEquality`](../scalaShadowing/language$.md#strictEquality)      enable strick equality

***production*** Language Features

***experimental*** Experimental Language Features

***experimental*** 10
Dotty-specific features come at the end.
Note: Due to the more restricted language import mechanism in dotty (only
imports count, implicits are disregarded) we don't need the constructions
of the inherited language features. A simple object for each feature is
sufficient.

<pre><code class="language-scala" >final val language: <a href="./language$.md">language</a></pre></code>
The `scala.language` object controls the language features available to the programmer, as proposed in the
[**SIP-18 document**](https://docs.google.com/document/d/1nlkvpoIRkx7at1qJEZafJwthZ3GeIklTFhqmXMvTX9Q/edit).
Each of these features has to be explicitly imported into the current scope to become available:
```scala
import language.postfixOps // or language._
List(1, 2, 3) reverse
```
The language features are:
* `dynamics`            enables defining calls rewriting using the `Dynamic` trait
* `postfixOps`          enables postfix operators
* `reflectiveCalls`     enables using structural types
* `implicitConversions` enables defining implicit methods and members
* `higherKinds`         enables writing higher-kinded types
* `existentials`        enables writing existential types
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


