# Package runtime
## Members:
<pre><code class="language-scala" >final object <a href="./Arrays$.md">Arrays</a></pre></code>
All but the first two operations should be short-circuited and implemented specially by
the backend.

<pre><code class="language-scala" >final val Arrays: <a href="./Arrays$.md">Arrays</a></pre></code>
All but the first two operations should be short-circuited and implemented specially by
the backend.


<pre><code class="language-scala" >final object <a href="./LazyVals$.md">LazyVals</a></pre></code>
Helper methods used in thread-safe lazy vals.

<pre><code class="language-scala" >final val LazyVals: <a href="./LazyVals$.md">LazyVals</a></pre></code>
Helper methods used in thread-safe lazy vals.


<pre><code class="language-scala" >class <a href="./LegacyApp.md">LegacyApp</a></pre></code>
Replaces the `scala.App` class which relies on `DelayedInit` functionality,
not supported by Dotty.

