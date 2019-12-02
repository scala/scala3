scala.annotation
# class infix

<pre><code class="language-scala" >final class infix extends Annotation with <a href="./StaticAnnotation.md">StaticAnnotation</a></pre></code>
A method annotation that suggests that the annotated method should
be used as an infix operator. Infix operations with alphanumeric
operator names require the operator to be annotated with `@infix`.

