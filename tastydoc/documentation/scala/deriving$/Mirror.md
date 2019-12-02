scala.deriving$
# trait Mirror

## Companion object Mirror

<pre><code class="language-scala" >sealed trait Mirror</pre></code>
Mirrors allows typelevel access to enums, case classes and objects, and their sealed parents.

## Concrete Type Members:
### MirroredElemLabels
<pre><code class="language-scala" >type MirroredElemLabels: Nothing <: Tuple</pre></code>
The names of the product elements


### MirroredLabel
<pre><code class="language-scala" >type MirroredLabel: Nothing <: String</pre></code>
The name of the type


### MirroredMonoType
<pre><code class="language-scala" >type MirroredMonoType: Nothing <: Any</pre></code>
The mirrored *-type


