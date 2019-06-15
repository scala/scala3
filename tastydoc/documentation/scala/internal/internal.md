# Package internal
## Members:
<pre><code class="language-scala" >final abstract class <a href="./MatchCase.md">MatchCase</a></pre></code>
A type constructor for a case in a match type.

<pre><code class="language-scala" >final object <a href="./TupledFunction$.md">TupledFunction</a></pre></code>
<pre><code class="language-scala" >final val TupledFunction: <a href="./TupledFunction$.md">TupledFunction</a></pre></code>

<pre><code class="language-scala" >final object <a href="./Quoted$.md">Quoted</a></pre></code>
<pre><code class="language-scala" >final val Quoted: <a href="./Quoted$.md">Quoted</a></pre></code>

<pre><code class="language-scala" >final abstract class <a href="./TypeBox.md">TypeBox</a></pre></code>
A type for skolems that are generated during capture conversion. Capture conversion
narrows the type of a tree whose type has wildcard arguments. A typical situation
is a tree `t` of type `C[_ >: L <: U]` and an expected type `C[X]` where `X` is an
instantiatable type variable. To be able to instantiate `X`, we cast the tree to type
`X[$n.CAP]` where `$n` is a fresh skolem type with underlying type `TypeBox[L, U]`.

<pre><code class="language-scala" >package <a href="./quoted/quoted.md">quoted</a></pre></code>