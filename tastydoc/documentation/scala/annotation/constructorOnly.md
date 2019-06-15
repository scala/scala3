scala.annotation
# class constructorOnly

<pre><code class="language-scala" >class constructorOnly extends Annotation with <a href="./StaticAnnotation.md">StaticAnnotation</a></pre></code>
An annotation that goes on parameters of classes or traits. It asserts
that the parameter is used only for initialization and is not kept in
the class as a field. Violations of this assertion are flagged as
compile errors. The annotation is particularly useful for implicit
parameters since for these a textual scan is not sufficient to know
where they are used.

## Annotations:
@param 
## Constructors:
<pre><code class="language-scala" >constructorOnly()</pre></code>

