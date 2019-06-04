# Package internal
## Members:
<pre><code class="language-scala" >final class <a href="./Body.md">Body</a></pre></code>
The class associated with a `BodyAnnotation`, which indicates
an inline method's right hand side

<pre><code class="language-scala" >class <a href="./Alias.md">Alias</a></pre></code>
An annotation to record a Scala2 pickled alias.

***aliased*** A TermRef pointing to the aliased field.

<pre><code class="language-scala" >class <a href="./sharable.md">sharable</a></pre></code>
An annotation indicating to `-Ycheck:reentrant` that a class or val can be safely shared.

***see*** scala.annotation.internal.unshared

<pre><code class="language-scala" >class <a href="./unshared.md">unshared</a></pre></code>
An annotation indicating to `-Ycheck:reentrant` that an object will not be accessed from multiple threads.

***see*** scala.annotation.internal.sharable

<pre><code class="language-scala" >final class <a href="./InlineParam.md">InlineParam</a></pre></code>
An annotation produced by Namer to indicate an inline parameter

<pre><code class="language-scala" >class <a href="./Child.md">Child</a></pre></code>
An annotation to indicate a child class or object of the annotated class.
E.g. if we have
  sealed class A
  case class B() extends A
  case class C() extends A
Then the class symbol `A` would carry the annotations
`@Child[Cref]`, @Child[Bref] where `Bref`, `Cref` are TypeRefs
referring to the class symbols of `B` and `C`.
Child annotations always appear in reverse order of textual occurrence.
I.e. in the example above, it is guaranteed that the child annotation for `C`
appears before the one for `B`.
TODO: This should be `Child[T <: AnyKind]`

<pre><code class="language-scala" >class <a href="./AnnotationDefault.md">AnnotationDefault</a></pre></code>
An annotation to tag Java annotation default values

<pre><code class="language-scala" >class <a href="./SourceFile.md">SourceFile</a></pre></code>
An annotation to record a Scala2 pickled alias.

***aliased*** A TermRef pointing to the aliased field.

<pre><code class="language-scala" >final class <a href="./Repeated.md">Repeated</a></pre></code>
An annotation produced by desugaring to indicate that a
sequence is a repeated parameter. I.e.
    T*  is expanded by Desugar to    Seq[T] @Repeated

