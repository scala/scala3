scala.annotation
# class threadUnsafe

<pre><code class="language-scala" >final class threadUnsafe extends Annotation with <a href="./StaticAnnotation.md">StaticAnnotation</a></pre></code>
This annotation can only be used on a field which defines a lazy val.
When this annotation is used, the initialization of the lazy val will use a
faster mechanism which is not thread-safe.

