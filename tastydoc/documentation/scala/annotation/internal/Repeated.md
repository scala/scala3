scala.annotation.internal
# class Repeated

<pre><code class="language-scala" >final class Repeated extends Annotation</pre></code>
An annotation produced by desugaring to indicate that a
sequence is a repeated parameter. I.e.
    T*  is expanded by Desugar to    Seq[T] @Repeated

## Constructors:
<pre><code class="language-scala" >Repeated()</pre></code>

