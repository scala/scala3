scala.util.control.NonLocalReturns$
# class ReturnThrowable

<pre><code class="language-scala" >class ReturnThrowable[T] extends Throwable with ControlThrowable</pre></code>
## Concrete Value Members:
### addSuppressed
<pre><code class="language-scala" >final def addSuppressed(x$0: Throwable): Unit</pre></code>

### fillInStackTrace
<pre><code class="language-scala" >override def fillInStackTrace(): Throwable</pre></code>

### getCause
<pre><code class="language-scala" >def getCause(): Throwable</pre></code>

### getLocalizedMessage
<pre><code class="language-scala" >def getLocalizedMessage(): String</pre></code>

### getMessage
<pre><code class="language-scala" >def getMessage(): String</pre></code>

### getStackTrace
<pre><code class="language-scala" >def getStackTrace(): Array[StackTraceElement]</pre></code>

### getStackTraceDepth
<pre><code class="language-scala" >private[lang] def getStackTraceDepth(): Int</pre></code>

### getStackTraceElement
<pre><code class="language-scala" >private[lang] def getStackTraceElement(x$0: Int): StackTraceElement</pre></code>

### getSuppressed
<pre><code class="language-scala" >final def getSuppressed(): Array[Throwable]</pre></code>

### initCause
<pre><code class="language-scala" >def initCause(x$0: Throwable): Throwable</pre></code>

### printStackTrace
<pre><code class="language-scala" >def printStackTrace(x$0: PrintWriter): Unit</pre></code>

### printStackTrace
<pre><code class="language-scala" >def printStackTrace(x$0: PrintStream): Unit</pre></code>

### printStackTrace
<pre><code class="language-scala" >def printStackTrace(): Unit</pre></code>

### result
<pre><code class="language-scala" >def result: T</pre></code>

### setStackTrace
<pre><code class="language-scala" >def setStackTrace(x$0: Array[StackTraceElement]): Unit</pre></code>

### throwReturn
<pre><code class="language-scala" >def throwReturn(result: T): Nothing</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

