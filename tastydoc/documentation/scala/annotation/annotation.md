# Package annotation
## Members:
<pre><code class="language-scala" >@setter @param @beanSetter @beanGetter @getter @field final class <a href="./static.md">static</a></pre></code>
<pre><code class="language-scala" >trait <a href="./RefiningAnnotation.md">RefiningAnnotation</a></pre></code>
A base trait for annotations that yield proper subtypes of the types they annotate.
Refining annotations are more "sticky" than normal ones. They are conceptually kept
around when normal refinements would also not be stripped away.

<pre><code class="language-scala" >final class <a href="./threadUnsafe.md">threadUnsafe</a></pre></code>
This annotation can only be used on a field which defines a lazy val.
When this annotation is used, the initialization of the lazy val will use a
faster mechanism which is not thread-safe.

<pre><code class="language-scala" >final class <a href="./alpha.md">alpha</a></pre></code>
An annotation that defines an external name for a definition.
If an `alpha(extname)` annotation is given for a method or some other
definition, its implementation will use the name `extname` instead of
the regular name. An `alpha` annotation is mandatory for definitions
with symbolic names.

<pre><code class="language-scala" >final class <a href="./infix.md">infix</a></pre></code>
A method annotation that suggests that the annotated method should
be used as an infix operator. Infix operations with alphanumeric
operator names require the operator to be annotated with `@infix`.

<pre><code class="language-scala" >@param class <a href="./constructorOnly.md">constructorOnly</a></pre></code>
An annotation that goes on parameters of classes or traits. It asserts
that the parameter is used only for initialization and is not kept in
the class as a field. Violations of this assertion are flagged as
compile errors. The annotation is particularly useful for implicit
parameters since for these a textual scan is not sufficient to know
where they are used.

