scala.deriving$.Mirror$
# trait Sum

<pre><code class="language-scala" >trait Sum extends Mirror</pre></code>
The Mirror for a sum type

## Constructors:
<pre><code class="language-scala" >Sum()</pre></code>

## Concrete Value Members:
### ordinal
<pre><code class="language-scala" >def ordinal(x: MirroredMonoType): Int</pre></code>
The ordinal number of the case class of `x`. For enums, `ordinal(x) == x.ordinal`

