scala.quoted
# object Toolbox

## Companion trait <a href="./Toolbox.md">Toolbox</a>

<pre><code class="language-scala" >final object Toolbox extends Serializable</pre></code>
## Known subclasses:
<a href="./Toolbox$/ToolboxNotFoundException.md">ToolboxNotFoundException</a>, <a href="./Toolbox$/Settings$.md">Settings</a>, <a href="./Toolbox$/Settings.md">Settings</a>
## Concrete Type Members:
### Settings
<pre><code class="language-scala" >final object <a href="./Toolbox$/Settings$.md">Settings</a></pre></code>
### Settings
<pre><code class="language-scala" >case class <a href="./Toolbox$/Settings.md">Settings</a></pre></code>
Setting of the Toolbox instance.

### ToolboxNotFoundException
<pre><code class="language-scala" >class <a href="./Toolbox$/ToolboxNotFoundException.md">ToolboxNotFoundException</a></pre></code>
## Concrete Value Members:
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

### Settings
<pre><code class="language-scala" >final val Settings: <a href="./Toolbox$/Settings$.md">Settings</a></pre></code>

