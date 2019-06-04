scala.quoted
# object Toolbox

## Companion trait <a href="./Toolbox.md">Toolbox</a>

<pre><code class="language-scala" >final object Toolbox extends Serializable</pre></code>
## Known subclasses:
<a href="./Toolbox$/ToolboxNotFoundException.md">ToolboxNotFoundException</a>, <a href="./Toolbox$/Settings$.md">Settings</a>, <a href="./Toolbox$/Settings.md">Settings</a>
## Concrete Type Members:
### Settings
<pre><code class="language-scala" >final object <a href="./Toolbox$/Settings.md">Settings</a></pre></code>
### Settings
<pre><code class="language-scala" >case class <a href="./Toolbox$/Settings.md">Settings</a></pre></code>
Setting of the Toolbox instance.

### ToolboxNotFoundException
<pre><code class="language-scala" >class <a href="./Toolbox$/ToolboxNotFoundException.md">ToolboxNotFoundException</a></pre></code>
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

### asInstanceOf
<pre><code class="language-scala" >final def asInstanceOf[X0]: X0</pre></code>

### clone
<pre><code class="language-scala" >protected def clone(): Object</pre></code>

### eq
<pre><code class="language-scala" >final def eq(x$0: Object): Boolean</pre></code>

### equals
<pre><code class="language-scala" >def equals(x$0: Any): Boolean</pre></code>

### finalize
<pre><code class="language-scala" >protected def finalize(): Unit</pre></code>

### getClass
<pre><code class="language-scala" >final def getClass(): Class[Nothing <: Any]</pre></code>

### hashCode
<pre><code class="language-scala" >def hashCode(): Int</pre></code>

### isInstanceOf
<pre><code class="language-scala" >final def isInstanceOf[X0]: Boolean</pre></code>

### make
<pre><code class="language-scala" >def make(appClassloader: ClassLoader)(settings: <a href="./Toolbox$/Settings.md">Settings</a>): <a href="./Toolbox.md">Toolbox</a></pre></code>
Create a new instance of the toolbox using the the classloader of the application.
Usuage:
```
implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
```
``````

***return*** A new instance of the toolbox

***appClassloader*** classloader of the application that generated the quotes

***settings*** toolbox settings

### ne
<pre><code class="language-scala" >final def ne(x$0: Object): Boolean</pre></code>

### notify
<pre><code class="language-scala" >final def notify(): Unit</pre></code>

### notifyAll
<pre><code class="language-scala" >final def notifyAll(): Unit</pre></code>

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

### writeReplace
<pre><code class="language-scala" >private def writeReplace(): AnyRef</pre></code>

### Settings
<pre><code class="language-scala" >final val Settings: <a href="./Toolbox$/Settings$.md">Settings$</a></pre></code>

