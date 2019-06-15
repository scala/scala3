scala.tasty.reflect.StandardDefinitions
# object definitions

<pre><code class="language-scala" >final object definitions extends StandardSymbols with StandardTypes with Serializable</pre></code>
A value containing all standard definitions in DefinitionsAPI

***Group*** Definitions

## Concrete Value Members:
### AnyClass
<pre><code class="language-scala" >def AnyClass: Symbol</pre></code>
The class symbol of core class `scala.Any`.

### AnyRefClass
<pre><code class="language-scala" >def AnyRefClass: Symbol</pre></code>
The type symbol of core class `scala.AnyRef`.

### AnyRefType
<pre><code class="language-scala" >def AnyRefType: Type</pre></code>
The type of core type `AnyRef`.

### AnyType
<pre><code class="language-scala" >def AnyType: Type</pre></code>
The type of core type `Any`.

### AnyValClass
<pre><code class="language-scala" >def AnyValClass: Symbol</pre></code>
The class symbol of core class `scala.AnyVal`.

### AnyValType
<pre><code class="language-scala" >def AnyValType: Type</pre></code>
The type of core type `AnyVal`.

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

### BooleanType
<pre><code class="language-scala" >def BooleanType: Type</pre></code>
The type of primitive type `Boolean`.

### ByteClass
<pre><code class="language-scala" >def ByteClass: Symbol</pre></code>
The class symbol of primitive class `scala.Byte`.

### ByteType
<pre><code class="language-scala" >def ByteType: Type</pre></code>
The type of primitive type `Byte`.

### CharClass
<pre><code class="language-scala" >def CharClass: Symbol</pre></code>
The class symbol of primitive class `scala.Char`.

### CharType
<pre><code class="language-scala" >def CharType: Type</pre></code>
The type of primitive type `Char`.

### ClassClass
<pre><code class="language-scala" >def ClassClass: Symbol</pre></code>
The class symbol of class `java.lang.Class`.

### DoubleClass
<pre><code class="language-scala" >def DoubleClass: Symbol</pre></code>
The class symbol of primitive class `scala.Double`.

### DoubleType
<pre><code class="language-scala" >def DoubleType: Type</pre></code>
The type of primitive type `Double`.

### EmptyPackageClass
<pre><code class="language-scala" >def EmptyPackageClass: Symbol</pre></code>
The class symbol of empty package `_root_._empty_`.

### FloatClass
<pre><code class="language-scala" >def FloatClass: Symbol</pre></code>
The class symbol of primitive class `scala.Float`.

### FloatType
<pre><code class="language-scala" >def FloatType: Type</pre></code>
The type of primitive type `Float`.

### FunctionClass
<pre><code class="language-scala" >def FunctionClass(arity: Int, isImplicit: Boolean, isErased: Boolean): Symbol</pre></code>
Function-like object that maps arity to symbols for classes `scala.FunctionX`.
*  0th element is `Function0`
*  1st element is `Function1`
*  ...
*  Nth element is `FunctionN`

### IntClass
<pre><code class="language-scala" >def IntClass: Symbol</pre></code>
The class symbol of primitive class `scala.Int`.

### IntType
<pre><code class="language-scala" >def IntType: Type</pre></code>
The type of primitive type `Int`.

### JavaLangPackage
<pre><code class="language-scala" >def JavaLangPackage: Symbol</pre></code>
The module symbol of package `java.lang`.

### LongClass
<pre><code class="language-scala" >def LongClass: Symbol</pre></code>
The class symbol of primitive class `scala.Long`.

### LongType
<pre><code class="language-scala" >def LongType: Type</pre></code>
The type of primitive type `Long`.

### NoneModule
<pre><code class="language-scala" >def NoneModule: Symbol</pre></code>
The module symbol of module `scala.None`.

### NothingClass
<pre><code class="language-scala" >def NothingClass: Symbol</pre></code>
The class symbol of core class `scala.Nothing`.

### NothingType
<pre><code class="language-scala" >def NothingType: Type</pre></code>
The type of core type `Nothing`.

### NullClass
<pre><code class="language-scala" >def NullClass: Symbol</pre></code>
The class symbol of core class `scala.Null`.

### NullType
<pre><code class="language-scala" >def NullType: Type</pre></code>
The type of core type `Null`.

### ObjectClass
<pre><code class="language-scala" >def ObjectClass: Symbol</pre></code>
The class symbol of core class `java.lang.Object`.

### ObjectType
<pre><code class="language-scala" >def ObjectType: Type</pre></code>
The type of core type `Object`.

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

### ShortType
<pre><code class="language-scala" >def ShortType: Type</pre></code>
The type of primitive type `Short`.

### SomeModule
<pre><code class="language-scala" >def SomeModule: Symbol</pre></code>
The module symbol of module `scala.Some`.

### StringClass
<pre><code class="language-scala" >def StringClass: Symbol</pre></code>
The class symbol of class `scala.String`.

### StringType
<pre><code class="language-scala" >def StringType: Type</pre></code>
The type for `scala.String`.

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

### UnitType
<pre><code class="language-scala" >def UnitType: Type</pre></code>
The type of primitive type `Unit`.

