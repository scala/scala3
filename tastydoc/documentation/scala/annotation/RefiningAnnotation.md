scala.annotation
# trait RefiningAnnotation

<pre><code class="language-scala" >trait RefiningAnnotation extends <a href="./Annotation.md">Annotation</a> with <a href="./StaticAnnotation.md">StaticAnnotation</a></pre></code>
A base trait for annotations that yield proper subtypes of the types they annotate.
Refining annotations are more "sticky" than normal ones. They are conceptually kept
around when normal refinements would also not be stripped away.

