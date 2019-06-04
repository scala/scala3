scala.tasty.reflect.StandardDefinitions
# trait StandardSymbols

<pre><code class="language-scala" >trait StandardSymbols</pre></code>
Defines standard symbols (and types via its base trait).

***Group*** API

## Constructors:
<pre><code class="language-scala" >StandardSymbols()</pre></code>

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

### AnyClass
<pre><code class="language-scala" >def AnyClass: Symbol</pre></code>
The class symbol of core class `scala.Any`.

### AnyRefClass
<pre><code class="language-scala" >def AnyRefClass: Symbol</pre></code>
The type symbol of core class `scala.AnyRef`.

### AnyValClass
<pre><code class="language-scala" >def AnyValClass: Symbol</pre></code>
The class symbol of core class `scala.AnyVal`.

### ArrayClass
<pre><code class="language-scala" >def ArrayClass: Symbol</pre></code>
The class symbol of class `scala.Array`.

### ArrayModule
<pre><code class="language-scala" >def ArrayModule: Symbol</pre></code>
The module symbol of module `scala.Array`.

### Array_apply
<pre><code class="language-scala" >def Array_apply: Symbol</pre></code>
The method symbol of method `apply` in class `scala.Array`.

### Array_clone
<pre><code class="language-scala" >def Array_clone: Symbol</pre></code>
The method symbol of method `clone` in class `scala.Array`.

### Array_length
<pre><code class="language-scala" >def Array_length: Symbol</pre></code>
The method symbol of method `length` in class `scala.Array`.

### Array_update
<pre><code class="language-scala" >def Array_update: Symbol</pre></code>
The method symbol of method `update` in class `scala.Array`.

### BooleanClass
<pre><code class="language-scala" >def BooleanClass: Symbol</pre></code>
The class symbol of primitive class `scala.Boolean`.

### ByteClass
<pre><code class="language-scala" >def ByteClass: Symbol</pre></code>
The class symbol of primitive class `scala.Byte`.

### CharClass
<pre><code class="language-scala" >def CharClass: Symbol</pre></code>
The class symbol of primitive class `scala.Char`.

### ClassClass
<pre><code class="language-scala" >def ClassClass: Symbol</pre></code>
The class symbol of class `java.lang.Class`.

### DoubleClass
<pre><code class="language-scala" >def DoubleClass: Symbol</pre></code>
The class symbol of primitive class `scala.Double`.

### EmptyPackageClass
<pre><code class="language-scala" >def EmptyPackageClass: Symbol</pre></code>
The class symbol of empty package `_root_._empty_`.

### FloatClass
<pre><code class="language-scala" >def FloatClass: Symbol</pre></code>
The class symbol of primitive class `scala.Float`.

### FunctionClass
<pre><code class="language-scala" >def FunctionClass(arity: Int, isImplicit: Boolean, isErased: Boolean): Symbol</pre></code>
Function-like object that maps arity to symbols for classes `scala.FunctionX`.
*  0th element is `Function0`
*  1st element is `Function1`
*  ...
*  Nth element is `FunctionN`

### FunctionClass$default$2
<pre><code class="language-scala" >def FunctionClass$default$2: Boolean</pre></code>
Function-like object that maps arity to symbols for classes `scala.FunctionX`.
*  0th element is `Function0`
*  1st element is `Function1`
*  ...
*  Nth element is `FunctionN`

### FunctionClass$default$3
<pre><code class="language-scala" >def FunctionClass$default$3: Boolean</pre></code>
Function-like object that maps arity to symbols for classes `scala.FunctionX`.
*  0th element is `Function0`
*  1st element is `Function1`
*  ...
*  Nth element is `FunctionN`

### IntClass
<pre><code class="language-scala" >def IntClass: Symbol</pre></code>
The class symbol of primitive class `scala.Int`.

### JavaLangPackage
<pre><code class="language-scala" >def JavaLangPackage: Symbol</pre></code>
The module symbol of package `java.lang`.

### LongClass
<pre><code class="language-scala" >def LongClass: Symbol</pre></code>
The class symbol of primitive class `scala.Long`.

### NoneModule
<pre><code class="language-scala" >def NoneModule: Symbol</pre></code>
The module symbol of module `scala.None`.

### NothingClass
<pre><code class="language-scala" >def NothingClass: Symbol</pre></code>
The class symbol of core class `scala.Nothing`.

### NullClass
<pre><code class="language-scala" >def NullClass: Symbol</pre></code>
The class symbol of core class `scala.Null`.

### ObjectClass
<pre><code class="language-scala" >def ObjectClass: Symbol</pre></code>
The class symbol of core class `java.lang.Object`.

### OptionClass
<pre><code class="language-scala" >def OptionClass: Symbol</pre></code>
The class symbol of class `scala.Option`.

### PredefModule
<pre><code class="language-scala" >def PredefModule: Symbol</pre></code>
The module symbol of module `scala.Predef`.

### ProductClass
<pre><code class="language-scala" >def ProductClass: Symbol</pre></code>
Function-like object that maps arity to symbols for classes `scala.Product`

### RepeatedParamClass
<pre><code class="language-scala" >def RepeatedParamClass: ClassDefSymbol</pre></code>
A dummy class symbol that is used to indicate repeated parameters
compiled by the Scala compiler.

### RootClass
<pre><code class="language-scala" >def RootClass: Symbol</pre></code>
The class symbol of root package `_root_`.

### RootPackage
<pre><code class="language-scala" >def RootPackage: Symbol</pre></code>
The module symbol of root package `_root_`.

### ScalaNumericValueClasses
<pre><code class="language-scala" >def ScalaNumericValueClasses: List[Symbol]</pre></code>
Contains Scala numeric value classes:
* Byte
* Short
* Int
* Long
* Float
* Double
* Char

### ScalaPackage
<pre><code class="language-scala" >def ScalaPackage: Symbol</pre></code>
The module symbol of package `scala`.

### ScalaPackageClass
<pre><code class="language-scala" >def ScalaPackageClass: Symbol</pre></code>
The class symbol of package `scala`.

### ScalaPrimitiveValueClasses
<pre><code class="language-scala" >def ScalaPrimitiveValueClasses: List[Symbol]</pre></code>
Contains Scala primitive value classes:
* Byte
* Short
* Int
* Long
* Float
* Double
* Char
* Boolean
* Unit

### ShortClass
<pre><code class="language-scala" >def ShortClass: Symbol</pre></code>
The class symbol of primitive class `scala.Short`.

### SomeModule
<pre><code class="language-scala" >def SomeModule: Symbol</pre></code>
The module symbol of module `scala.Some`.

### StringClass
<pre><code class="language-scala" >def StringClass: Symbol</pre></code>
The class symbol of class `scala.String`.

### TupleClass
<pre><code class="language-scala" >def TupleClass(arity: Int): Symbol</pre></code>
Function-like object that maps arity to symbols for classes `scala.TupleX`.
*  0th element is `NoSymbol`
*  1st element is `NoSymbol`
*  2st element is `Tuple2`
*  ...
* 22nd element is `Tuple22`
* 23nd element is `NoSymbol`  // TODO update when we will have more tuples
* ...

### UnitClass
<pre><code class="language-scala" >def UnitClass: Symbol</pre></code>
The class symbol of primitive class `scala.Unit`.

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

