scala.tasty.reflect.FlagsOps
# class FlagsAPI

<pre><code class="language-scala" >class FlagsAPI</pre></code>
## Constructors:
<pre><code class="language-scala" >FlagsAPI(self: Flags)</pre></code>

## Concrete Value Members:
### &
<pre><code class="language-scala" >def &(that: Flags): Flags</pre></code>
Intersection of the two flag sets

### is
<pre><code class="language-scala" >def is(that: Flags): Boolean</pre></code>
Is the given flag set a subset of this flag sets

### |
<pre><code class="language-scala" >def |(that: Flags): Flags</pre></code>
Union of the two flag sets

