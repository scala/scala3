scala.tasty.file
# object ConsumeTasty

<pre><code class="language-scala" >final object ConsumeTasty extends Serializable</pre></code>
## Concrete Value Members:
### apply
<pre><code class="language-scala" >def apply(classpath: String, classes: List[String], tastyConsumer: TastyConsumer): Unit</pre></code>
Load and process TASTy files using TASTy reflect

***tastyConsumer*** consumer that will process the tasty trees

***classes*** classes to be consumed

***classpath*** Classpath where the classes are located

