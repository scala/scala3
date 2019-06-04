scala.quoted
# class QuoteError

## Companion object <a href="./QuoteError$.md">QuoteError</a>

<pre><code class="language-scala" >class QuoteError extends Throwable</pre></code>
Throwing this error in the implementation of a macro
will result in a compilation error with the given message.

## Constructors:
<pre><code class="language-scala" >QuoteError(message: String, from: Option[<a href="./Expr.md">Expr</a>[Nothing <: Any]])</pre></code>

## Concrete Value Members:
### !=
<pre><code class="language-scala" >final def !=(x$0: Any): Boolean</pre></code>

### ##
<pre><code class="language-scala" >final def ##: Int</pre></code>

### $asInstanceOf$
<pre><code class="language-scala" >final def $asInstanceOf$[X0]: X0</pre></code>

### $isInstanceOf$
<pre><code class="language-scala" >final def $isInstanceOf$[X0]: Boolean</pre></code>

### ==
<pre><code class="language-scala" >final def ==(x$0: Any): Boolean</pre></code>

### addSuppressed
<pre><code class="language-scala" >final def addSuppressed(x$0: Throwable): Unit</pre></code>

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### eq
<pre><code class="language-scala" >final def eq(x$0: Object): Boolean</pre></code>

### equals
<pre><code class="language-scala" >def equals(x$0: Any): Boolean</pre></code>

### fillInStackTrace
<pre><code class="language-scala" >def fillInStackTrace(): Throwable</pre></code>

### finalize
<pre><code class="language-scala" >protected def finalize(): Unit</pre></code>

### getCause
<pre><code class="language-scala" >def getCause(): Throwable</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

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

### hashCode
<pre><code class="language-scala" >def hashCode(): Int</pre></code>

### initCause
<pre><code class="language-scala" >def initCause(x$0: Throwable): Throwable</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### ne
<pre><code class="language-scala" >final def ne(x$0: Object): Boolean</pre></code>

### notify
<pre><code class="language-scala" >final def notify(): Unit</pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): Unit</pre></code>

### printStackTrace
<pre><code class="language-scala" >def printStackTrace(x$0: PrintWriter): Unit</pre></code>

### printStackTrace
<pre><code class="language-scala" >def printStackTrace(x$0: PrintStream): Unit</pre></code>

### printStackTrace
<pre><code class="language-scala" >def printStackTrace(): Unit</pre></code>

### setStackTrace
<pre><code class="language-scala" >def setStackTrace(x$0: Array[StackTraceElement]): Unit</pre></code>

### synchronized
<pre><code class="language-scala" >final def synchronized[X0](x$0: X0): X0</pre></code>

### toString
<pre><code class="language-scala" >def toString(): String</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long, x$1: Int): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(x$0: Long): Unit</pre></code>

### wait
<pre><code class="language-scala" >final def wait(): Unit</pre></code>

### from
<pre><code class="language-scala" >val from: Option[<a href="./Expr.md">Expr</a>[Nothing <: Any]]</pre></code>

