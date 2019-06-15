scala.annotation
# class alpha

<pre><code class="language-scala" >final class alpha extends Annotation with <a href="./StaticAnnotation.md">StaticAnnotation</a></pre></code>
An annotation that defines an external name for a definition.
If an `alpha(extname)` annotation is given for a method or some other
definition, its implementation will use the name `extname` instead of
the regular name. An `alpha` annotation is mandatory for definitions
with symbolic names.

## Constructors:
<pre><code class="language-scala" >alpha(externalName: String)</pre></code>

