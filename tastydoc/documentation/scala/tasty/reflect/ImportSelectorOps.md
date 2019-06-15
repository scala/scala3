scala.tasty.reflect
# trait ImportSelectorOps

<pre><code class="language-scala" >trait ImportSelectorOps extends Core</pre></code>
## Known subclasses:
<a href="./ImportSelectorOps/OmitSelector$.md">OmitSelector</a>, <a href="./ImportSelectorOps/OmitSelectorAPI.md">OmitSelectorAPI</a>, <a href="./ImportSelectorOps/RenameSelector$.md">RenameSelector</a>, <a href="./ImportSelectorOps/RenameSelectorAPI.md">RenameSelectorAPI</a>, <a href="./ImportSelectorOps/SimpleSelector$.md">SimpleSelector</a>, <a href="./ImportSelectorOps/SimpleSelectorAPI.md">SimpleSelectorAPI</a>
## Constructors:
<pre><code class="language-scala" >ImportSelectorOps()</pre></code>

## Concrete Type Members:
### OmitSelector
<pre><code class="language-scala" >final object <a href="./ImportSelectorOps/OmitSelector$.md">OmitSelector</a></pre></code>
### RenameSelector
<pre><code class="language-scala" >final object <a href="./ImportSelectorOps/RenameSelector$.md">RenameSelector</a></pre></code>
### SimpleSelector
<pre><code class="language-scala" >final object <a href="./ImportSelectorOps/SimpleSelector$.md">SimpleSelector</a></pre></code>
### OmitSelectorAPI
<pre><code class="language-scala" >class <a href="./ImportSelectorOps/OmitSelectorAPI.md">OmitSelectorAPI</a></pre></code>
### RenameSelectorAPI
<pre><code class="language-scala" >class <a href="./ImportSelectorOps/RenameSelectorAPI.md">RenameSelectorAPI</a></pre></code>
### SimpleSelectorAPI
<pre><code class="language-scala" >class <a href="./ImportSelectorOps/SimpleSelectorAPI.md">SimpleSelectorAPI</a></pre></code>
## Concrete Value Members:
### OmitSelectorAPI
<pre><code class="language-scala" >final implicit def OmitSelectorAPI(self: OmitSelector): OmitSelectorAPI</pre></code>

### RenameSelectorAPI
<pre><code class="language-scala" >final implicit def RenameSelectorAPI(self: RenameSelector): RenameSelectorAPI</pre></code>

### SimpleSelectorAPI
<pre><code class="language-scala" >final implicit def SimpleSelectorAPI(self: SimpleSelector): SimpleSelectorAPI</pre></code>

### OmitSelector
<pre><code class="language-scala" >final val OmitSelector: OmitSelector</pre></code>

### RenameSelector
<pre><code class="language-scala" >final val RenameSelector: RenameSelector</pre></code>

### SimpleSelector
<pre><code class="language-scala" >final val SimpleSelector: SimpleSelector</pre></code>

