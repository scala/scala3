scala
# trait TupledFunction

<pre><code class="language-scala" >sealed trait TupledFunction[F, G]</pre></code>
Type class relating a `FunctionN[..., R]` with an equivalent tupled function `Function1[TupleN[...], R]`

***F*** a function type

***G*** a tupled function type (function of arity 1 receiving a tuple as argument)

## Annotations:
@implicitNotFound 
## Concrete Value Members:
### tupled
<pre><code class="language-scala" >def tupled(f: F): G</pre></code>

### untupled
<pre><code class="language-scala" >def untupled(g: G): F</pre></code>

