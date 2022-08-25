<img src="./logo/package_twitter_if9bsyj4/color1/full/coverphoto/color1-white_logo_dark_background.png" alt="logo"/>

[![Maven](https://img.shields.io/maven-central/v/com.github.imrafaelmerino/json-scala-values_3/5.1.0)](https://search.maven.org/artifact/com.github.imrafaelmerino/json-scala-values_3/5.1.0/jar)

“_Talk is cheap. Show me the code._”
**Linus Torvalds**

- [Introduction](#introduction)
- [What to use _json-values_ for and when to use it](#whatfor)
- [Code wins arguments](#cwa)
  - [JSON creation](#jc)
  - [JSON validation](#jv)
  - [JSON parsing](#jp)
  - [JSON generation](#jg)
  - [JSON manipulation](#jm)
- [JsPath](#jspath)
- [JsValue](#jsvalue)
- [Creating Jsons](#creatingjson)
  - [Creating JsObj](#creatingjsonobj)
  - [Creating JsArray](#creatingjsonarray)
- [Putting data in and getting data out](#inout)
- [Filter, map and reduce](#filtermapreduce)
- [Flattening a Json](#flattening)
- [Specs](#specs)
- [Generators](#generators)
- [Installation](#installation)
- [Related projects](#rp)


## <a name="introduction"><a/> Introduction

One of the essential aspects of FP is immutable data structures,
better known in the FP jargon as values.
It is a fact that, when possible, working with values leads to more
readable code, easier to maintain, and fewer bugs. However, sometimes, it is 
at the cost of losing performance because the [copy-on-write](https://en.wikipedia.org/wiki/Copy-on-write)
approach is inefficient for significant data structures. Here is where 
persistent data structures come into play.

Why doesn't exist a persistent Json in Scala? It's the question I asked myself 
when I got into FP. Since I found no answer, I decided to implement one by myself.


## <a name="whatfor"><a/> What to use json-values for and when to use it

* You need to deal with Jsons, and you want to program following a functional style, **using just functions and values**.
* For those architectures that work with JSON end-to-end, it's safe and efficient to have a persistent Json.
  Think of actors sending JSON messages one to each other for example.
* You manipulate JSON all the time, and you'd like to do it with less ceremony. json-values is declarative and
  takes advantage of concepts from FP to define a powerful API.
* Generating JSON to do Property-Based-Testing is child's play with json-values.
* Generating specifications to validate JSON and parse strings or bytes very efficiently is a piece of cake.
* Simplicity matters, and I'd argue that json-values is simple.
* As _**Pat Helland**_ said, [Immutability Changes Everything!](http://cidrdb.org/cidr2015/Papers/CIDR15_Paper16.pdf)


## <a name="cwa"><a/> Code wins arguments

### <a name="jc"><a/> JSON creation

```scala    

JsObj("name" -> JsStr("Rafael"),
      "languages" -> JsArray("Java", "Scala", "Kotlin"),
      "age" -> JsInt(1),
      "address" -> JsObj("street" -> JsStr("Elm Street"),
                         "coordinates" -> JsArray(3.32, 40.4)
                        )
     )

```

or using conversions:

```scala     
import json.value.Conversions.given

JsObj("name" -> "Rafael",
      "languages" -> JsArray("Java", "Scala", "Kotlin"),
      "age" -> 1,
      "address" -> JsObj("street" -> "Elm Street",
                         "coordinates" -> JsArray(3.32, 40.4)
                        )
     )

```


### <a name="jv"><a/> JSON validation

```scala    

val spec = 
        JsObjSpec("name" ->  IsStr,
                  "languages" -> IsArrayOf(IsStr),
                  "age" -> IsInt,
                  "address" -> JsObjSpec("street" -> IsStr,
                                       "coordinates" ->  IsTuple(IsNumber,
                                                                 IsNumber
                                                                 )
                                      )
                 )
                 .withOptKeys("address")
    
```   

You can customize your specs with predicates and operators:

```scala     

val noneEmpty = IsStr(n => if n.nonEmpty then true else "empty name")
val ageSpec = IsInt(n => if n < 16 then "too young" else true)

val addressLocSpec = JsObjSpec("coordinates" ->  IsTuple(IsNumber,IsNumber))
val addressFullSpec =  
        JsObjSpec("street" -> noneEmpty,
                  "number" -> noneEmpty,
                  "zipcode" -> noneEmpty,
                  "country" -> noneEmpty
                 )     
val addressSpec = addressLocSpec or addressFullSpec

val spec = 
        JsObjSpec("name" ->  noneEmpty,
                  "languages" -> IsArrayOf(noneEmpty),
                  "age" -> ageSpec,
                  "address" -> addressSpec
                 )
                 .withOptKeys("address")
    
val errors:LazyList[(JsPath, Invalid)] = spec.validateAll(json)    

// Invalid:: (JsValue, SpecError)

```   

As you can see, the predicates can return a string instead of a boolean to customize
the error messages.


### <a name="jp"><a/>  JSON parsing

You can get a parser from a spec to parse a string or array of bytes into a Json.
Most of the json-schema implementations parse the whole Json and then validate it,
which is very inefficient. json-values validates each element of the Json as soon
as it is parsed.

On the other hand, it uses the library [jsoniter-scala](https://github.com/plokhotnyuk/jsoniter-scala) to do the parsing,
which is extremely fast and has a great API.

```scala      

val parser = spec.parser

val json:JsObj = parser.parse("{}")

```

### <a name="jg"><a/>  JSON generation

```scala    

val gen = 
      JsObjGen(name -> Gen.alphaStr.map(JsStr),
               languages -> JsArrayGen.of(Gen.oneOf("scala", "java", "kotlin")).distinct,
               age -> Arbitrary.arbitrary[Int].map(JsInt),
               address -> JsObjGen(street -> Gen.asciiStr.map(JsStr),
                                   coordinates -> TupleGen(Arbitrary.arbitrary[BigDecimal]
                                                                    .map(JsBigDec),
                                                           Arbitrary.arbitrary[BigDecimal]
                                                                    .map(JsBigDec)))
               )
        
                
```

or using conversions to avoid writing the map method:

```scala    
import json.value.gen.Conversions.given          

          
val gen = 
      JsObjGen(name -> Gen.alphaStr,
               languages -> JsArrayGen.of(Gen.oneOf("scala", "java", "kotlin")).distinct,
               age -> Arbitrary.arbitrary[Int],
               address -> JsObjGen(street -> Gen.asciiStr,
                                   coordinates -> TupleGen(Arbitrary.arbitrary[BigDecimal],
                                                           Arbitrary.arbitrary[BigDecimal]))
              )
        
                
```


When testing, it's important to generate valid and invalid data according
to your specifications. Generators and specs can be used for this purpose:

```scala    

val gen = 
      JsObjGen("name" -> Gen.alphaStr,
               "languages" -> JsArrayGen.of(Gen.oneOf("scala", "java", "kotlin")).distinct,
               "age" -> Arbitrary.arbitrary[Int],
               "address" -> JsObjGen("street" -> Gen.asciiStr,
                                     "coordinates" -> TupleGen(Arbitrary.arbitrary[BigDecimal],
                                                               Arbitrary.arbitrary[BigDecimal]
                                                               )
                                     )
                                     .withOptKeys("street","coordinates")
                                     .withNullValues("street","coordinates")
               )
               .withOptKeys("name","languages","age","address")
               .withNullValues("name","languages","age","address")
               
val (validGen, invalidGen) = gen.partition(spec)  

            
```


### <a name="jm"><a/> JSON manipulation

Crafting functions free of NullPointerException with optics is a piece of cake:

```scala   
import monocle.{Lens ,Optional}

val nameLens:Lens[JsObj,String] = JsObj.lens.str("name")

val ageLens:Lens[JsObj,Int] = JsObj.lens.int("age")

val cityOpt:Optional[JsObj,String] = JsObj.optional.str(root / "address" / "city")

val latitudeOpt:Optional[JsObj,Double] = JsObj.optional.double(root / "address" / "coordinates" / "latitude")

//let's craft a function using lenses and optionals

val fn:Function[JsObj,JsObj]  = 
    ageLens.modify(_ + 1)
           .andThen(nameLens.modify(_.trim))
           .andThen(cityOpt.set("Paris"))
           .andThen(latitudeLens.modify(lat => -lat))
           
         
val updated = fn(person)

```

No if-else conditions, no null checks, and I'd say it's pretty
expressive and concise. As you may notice, each field has defined an
associated optic, and we create functions, like _fn_
in the previous example, putting them together (composition is key
to handle complexity).

**Filter,map and reduce were never so easy!**

These functions **traverse the whole Json recursively**:

```scala    
          
json.mapKeys(_.toLowerCase)
    .map(JsStr.prism.modify(_.trim))
    .filter(_.noneNull)
    .filterKeys(!_.startsWith("$"))
                    
```

## <a name="jspath"><a/>JsPath

A _JsPath_ represents a location of a specific value within a Json. 
It's a sequence of _Position_, being a position either a _Key_ or an _Index_.

```scala  

val a:JsPath = JsPath.root / "a" / "b" / "c"
val b:JsPath = JsPath.root / 0 / 1

val ahead:Position = a.head

val atail:JsPath = a.tail
atail.head == Key("b")
atail.last == Key("c")

val bhead:Position = b.head

//appending paths
val c:JsPath = a / b
c.head == Key("a")
c.last == Index(1)

//prepending paths
val d:JsPath = a \ b
d.head == Index(0)
d.last == Key("c")

```


## <a name="jsvalue"><a/>JsValue

Every element in a Json is a subtype of _JsValue_. There is a specific type for each value described
in [json.org](https://www.json.org):

- String
- Number
- Null
- JSON object
- JSON array

There are five number specializations:

- Integer
- Long
- Double
- BigDecimal
- BigInteger

json-values adds support for the Instant type. Instants are serialized into 
their string representation according to ISO-8601.

When it comes to the _equals_ method, json-values is data-oriented. I mean, 
two JSON are equals if they represent the same piece of information. For 
example, the following JSONs xs and ys have values with different primitive 
types, and the keys don't follow the same order:

```java  

val xs = JsObj("a" -> JsInt(1000),
               "b" -> JsBigDec(BigDecimal.valueOf(100_000_000_000_000L)),
               "c" -> JsInstant(Instant.parse("2022-05-25T14:27:37.353Z"))
              )

val ys = JsObj("b" -> JsBigInt(BigInteger.valueOf(100_000_000_000_000L)),
               "a" -> JsLong(1000L),
               "c" -> JsStr("2022-05-25T14:27:37.353Z")
              ) 

```

Nevertheless, since both objects represent the same piece of information:

```json   

{
  "a": 1000,
  "b": 100000000000000,
  "c": "2022-05-25T14:27:37.353Z"
}

```

It makes sense that both of them are equal objects. Therefore, they have the same hashcode.
The best way of exploring JsValue is by applying an exhaustive pattern matching:

```scala   

val value: JsValue = ???

value match
  case primitive: JsPrimitive => primitive match
    case JsBool(b) => println("I'm a boolean")
    case JsNull => println("I'm null")
    case JsInstant(i) => println("I'm an instant")
    case JsStr(str) => println("I'm a string")
    case number: JsNumber => number match
      case JsInt(i) => println("I'm an integer")
      case JsDouble(d) => println("I'm a double")
      case JsLong(l) => println("I'm a long")
      case JsBigDec(bd) => println("I'm a big decimal")
      case JsBigInt(bi) => println("I'm a big integer")
  case json: Json[_] => json match
    case o: JsObj => println("I'm an object")
    case a: JsArray => println("I'm an array")
  case JsNothing => println("I'm a special type!")

```
The singleton _JsNothing_ represents nothing. **It's a convenient type that makes functions
that return a JsValue total on their arguments**. For example, the Json function

```scala   

Json :: apply(path:JsPath):JsValue

```

is total because it returns a JsValue for every JsPath. If there is no element located at the given path,
it returns _JsNothing_. On the other hand, inserting _JsNothing_ at a path in a Json is like removing the
element located at that path.


## <a name="creatingjson"><a/>Creating Jsons

There are several ways of creating Jsons:
* Using the apply methods of companion objects.
* Parsing an array of bytes, a string, or an input stream. 
When possible, it's always better to work on a byte level. 
On the other hand, if the schema of the Json is known, the 
fastest way is to define a spec.
* From an empty Json, and then using the API to 
insert new values.

### <a name="creatingjsonobj"><a/>Creating JsObjs
From a Map using the arrow notation:

```scala   
import json.value.JsObj
import json.value.JsArray
import json.value.Conversions.given

val person = JsObj("type" -> "Person",
                   "age" -> 37,
                   "name" -> "Rafael",
                   "gender" -> "MALE",
                   "address" -> JsObj("location" -> JsArray(40.416775,
                                                            -3.703790
                                                           )
                                     ),
                   "book_ids" -> JsArray("00001",
                                         "00002"
                                        )
                   )
```
From a sequence of path/value pairs:

```scala   
import json.value.Conversions.given

JsObj.pairs((root / "type","@Person"),
            (root / "age", 37),
            (root / "name", "Rafael"),
            (root / "gender", "MALE"),
            (root / "address" / "location" / 0, 40.416775),
            (root / "address" / "location" / 1, 40.416775),
            (root / "books_ids" / 0, "00001"),
            (root / "books_ids" / 1, "00002"),
           )
```


**Parsing a string or array of bytes, and the schema of the Json is unknown:**

```scala   

val str:String = ??? 
val bytes:Array[Byte] = ??? 

val a:JsObj= JsObj.parse(str)
val b:JsObj= JsObj.parse(bytes)

```

**Parsing a string or array of bytes, and the schema of the Json is known.**
We can create a spec to define the structure of the Json and then get a parser:

```scala    
val spec:JsObjSpec = JsObjSpec("a" -> IsInt,
                               "b" -> IsStr,
                               "c" -> IsBool,
                               "d" -> JsObjSpec("e" -> IsLong,
                                                "f" -> IsTuple(IsNumber,IsNumber)
                                               ),
                               "e" -> IsArrayOf(IsStr)
                              )

val parser = spec.parser //reuse this object

```

With the updated function:

```scala    

import json.value.Conversions.given

JsObj.empty.updated(root / "a" / "b" / 0, 1)
           .updated(root / "a" / "b" / 1, 2)
           .updated(root / "a" / "c", "hi")
```


### <a name="creatingjsonarray"><a/>Creating JsArrays

From a sequence of values:

```scala   
import json.value.Converisons.given

JsArray(1,2,3)

JsArray("a","b","c")

JsArray("a", 1, JsObj("a" -> 1, "b" -> 2), JsNull, JsArray(0,1))

```

From a sequence of path/value pairs:

```scala    
import json.value.Conversions.given

JsArray((root / 0, "a"),
        (root / 1, 1),
        (root / 2 / "a", 1),
        (root / 3, JsNull),
        (root / 4 / 0, 0),
        (root / 4 / 1, 1)
       )
```

Parsing a string or array of bytes, and the schema of the Json is unknown:

```scala    

val str:String = ??? 
val bytes:Array[Byte] = ??? 

val a = JsArray.parse(str)
val b = JsArray.parse(bytes)

```

Parsing a string or array of bytes, and the schema of the Json is known. 
We can create a spec to define the structure of the Json array:

```scala   
val spec = IsTuple(IsStr,
                   IsInt,
                   JsObjSpec("a" -> IsStr),
                   IsStr.nullable,
                   IsArrayOf(IsInt)
                   )

val parser = spec.parser //reuse this object

val str:String = ??? 
val bytes:Array[Byte] = ??? 

val a = parser.parse(str)
val b = parser.parse(bytes)

```

With the appended and prepended functions:

```scala    

JsArray.empty.appended("a")
             .appended("1")
             .appended(JsObj("a" -> 1, "b" -> true))
             .appended(JsNull)
             .appended(JsArray(0,1))
```

## <a name="inout"><a/>Putting data in and getting data out
There is one function to put data in a Json specifying a path and a value:

```scala   

JsObj::   updated(path:JsPath, value:JsValue, padWith:JsValue = JsNull):JsObj
JsArray:: updated(path:JsPath, value:JsValue, padWith:JsValue = JsNull):JsArray

```

**The _updated_ function always inserts the value at the specified path, creating
any needed container and padding arrays when necessary.**

```scala   

// always true: if you insert a value, you'll get it back
json.updated(path, value)(path) == value 

JsObj.empty.updated(root / "a", 1) == JsObj("a" -> 1)
JsObj.empty.updated(root / "a" / "b", 1) == JsObj("a" -> JsObj("b" -> 1))
JsObj.empty.updated(root / "a" / 2, "z", padWith="") == JsObj("a" -> JsArray("","","z"))

```

New elements can be appended and prepended to a JsArray:

```scala   

appended(value:JsValue):JsArray

prepended(value:JsValue):JsArray

appendedAll(xs:IterableOne[JsValue]):JsArray

prependedAll(xs:IterableOne[JsValue]):JsArray

```


## <a name="filtermapreduce"><a/>Filter,map and reduce

The functions _filter_, _filterKeys_, _map_, _mapKeys_, and _reduce_ 
**traverse the whole json recursively**.
All these functions are functors (don't change the structure of the Json).

```scala   

val toLowerCase:String => String = _.toLowerCase

json mapKeys toLowerCase

json map JsStr.prism.modify(_.trim)

val isNotNull:JsPrimitive => Boolean = _.noneNull

json filter isNotNull

 ```

## <a name="flattening"><a/>Flattening a Json

A Json can be seen as a set of (JsPath,JsValue) pairs. 
The flatten function returns a lazy list of pairs:

```scala    

Json:: flatten:LazyList[(JsPath,JsValue)]

```
Returning a lazy list decouples the consumers from the producer. 
No matter the number of pairs that will be consumed, the 
implementation doesn't change.

Let's put an example:

```scala    
val obj = JsObj("a" -> 1,
                "b" -> JsArray(1,"m", JsObj("c" -> true, "d" -> JsObj.empty))
               )

obj.flatten.foreach(println) // all the pairs are consumed

// (a, 1)
// (b / 0, 1)
// (b / 1, "m")
// (b / 2 / c, true)
// (b / 2 / d, {})
```

## <a name="specs"><a/>Specs

A Json spec defines the structure of a Json. Specs have attractive qualities like:
* Easy to write. You can define Specs in the same way you define a raw Json.
* Easy to compose. You glue specs together and create new ones easily.
* Easy to extend. There are predefined specs that cover the most common scenarios.
Nevertheless, you can create any imaginable spec from predicates.

Let's go straight to the point and put an example:


```scala    

import json.value.spec._

val personSpec = JsObjSpec("@type" -> IsCons("Person"),
                           "age" -> IsInt,
                           "name" -> IsStr,
                           "gender" -> IsEnum("MALE","FEMALE"),
                           "address" -> JsObjSpec("location" -> IsTuple(IsNumber,
                                                                        IsNumber
                                                                        )
                                                 ),
                           "books_id" -> IsArrayOf(IsStr)
                          ).lenient

```

I think it's self-explanatory and as it was mentioned, defining a spec is 
as simple as defining a Json.
It's declarative and concise, with no ceremony at all. 

There are a bunch of things we can do with a spec:
  - Validate a Json and get a stream with all the validation errors and their locations

```scala      
    
val json = JsObj("a" -> 1,
                 "b" -> "hi", 
                 "c" -> JsArray(JsObj("d" -> "bye", 
                                      "e" -> 1)
                                )
                )

val spec = JsObjSpec("a" -> IsStr, 
                     "b" -> IsInt, 
                     "c" -> IsArrayOf(JsObjSpec("d" -> IsInstant, 
                                                "e" -> IsBool)
                                                )
                                      )

val errors: LazyList[(JsPath, Invalid)] = spec validateAll json

errors foreach println

//output 

(a,Invalid(1,SpecError(STRING_EXPECTED)))
(b,Invalid(hi,SpecError(INT_EXPECTED)))
(c / 0 / d,Invalid(bye,SpecError(INSTANT_EXPECTED)))
(c / 0 / e,Invalid(1,SpecError(BOOLEAN_EXPECTED)))

```

  - Validate a Json to check whether it is valid or not (not interested in the detail about all the possible errors)

```scala      

val result: Result = spec.validate(json)
result match 
   case Valid => println("valid json!")
   case Invalid(value, error) => println(s"the value $value doesn conform the spec: $error")

```

  - Get a parser

  - Filter a generator


Reusing and composing specs is very straightforward. 
Spec composition is a good way of creating complex specs.
You define little blocks and glue them together. Let's put an example:

```scala    

val address = JsObjSpec("street" -> IsStr,
                        "number" -> IsInt,
                       )

val user = JsObjSpec("name" -> IsStr,
                     "id" -> IsStr
                    )

def userWithAddress = user concat JsObjSpec("address" -> address)

def userWithOptionalAddress = 
  (user concat JsObjSpec("address" -> address)).withOptKeys("addresss")

```

## <a name="generators"><a/>Generators

If you practice property-based testing and use [ScalaCheck](https://www.scalacheck.org), 
you'll be able to design composable Json generators very quickly and naturally, as if you were 
writing out a Json.

### <a name="customgens"><a/> Defining custom Json generators
Let's create a person generator:

```scala    
import json.value.JsObj
import json.value.gen.Conversions.given
import json.value.gen.*
import org.scalacheck.Gen

def typeGen: Gen[String] = ???
def nameGen: Gen[String] = ???
def birthDateGen: Gen[String] = ???
def latitudeGen: Gen[Double] = ???
def longitudeGen: Gen[Double] = ???
def emailGen: Gen[String] = ???
def countryGen: Gen[String] = ???

def personGen:Gen[JsObj] = JsObjGen("@type" -> typeGen,
                                    "name" -> nameGen,
                                    "birth_date" -> birthDateGen,
                                    "email" -> emailGen,
                                    "gender" -> Gen.oneOf("Male",
                                                          "Female"
                                                   ),
                                     "address" -> JsObjGen("country" -> countryGen,
                                                           "location" -> TupleGen(latitudeGen,
                                                                                  longitudeGen
                                                                                   )
                                                          )
                             )
```
You can also create Json generators from pairs of JsPath and their generators:


 ```scala    

JsObjGen.pairs((root / "@type" -> typeGen),
               (root / "name" -> nameGen),
               (root / "birth_date" -> birthDateGen),
               (root / "email" -> emailGen),
               (root / "gender" -> Gen.oneOf("Male","Female")),
               (root / "address" / "country" -> countryGen),
               (root / "address" / "location" / 0 -> latitudeGen),
               (root / "address" / "location" / 1 -> longitudeGen)
               )
```

A typical scenario is when we want some elements not to be always generated.

There are two possible solutions:

  - Use the withOptKeys function to create a new generator where the
specified keys are optional.

  - Using the the special value _JsNothing_, you can customize the probability 
an element will be generated with. Remember that inserting _JsNothing_ 
in a Json at a path is like removing the element located at that path. 
Taking that into account, let's create a generator that produces Jsons 
without the key _name_ with a probability of 50 percent:


```scala    
def nameGen: Gen[JsStr] = ???

def optNameGen: Gen[JsValue] = Gen.oneOf(JsNothing,nameGen)

JsObjGen("name" -> optNameGen)

```

And we can change that probability using the ScalaCheck function 
_Gen.frequencies_:

```scala    
def nameGen: Gen[JsStr] = ???

def optNameGen: Gen[JsValue] = Gen.frequency((10,JsNothing),
                                             (90,nameGen)
                                             )

JsObjGen("name" ->  optNameGen)

```

### <a name="composing"><a/> Composing Json generators

Composing Json generators is key to handle complexity and avoid repetition. 
There are two ways, inserting pairs into generators and joining generators:

```scala   
def addressGen:Gen[JsObj] = JsObjGen("street" -> streetGen,
                                     "city" -> cityGen,
                                     "zip_code" -> zipCodeGen
                                    )

def addressWithLocationGen:Gen[JsObj] = 
            addressGen updated ("location", TupleGen(latitudeGen,longitudeGen))

                                                         

def namesGen = JsObjGen("family_name" -> familyNameGen,
                        "given_name" -> givenNameGen)

def contactGen = JsObjGen("email" -> emailGen,
                          "phone" -> phoneGen,
                          "twitter_handle" -> handleGen
                         )

val clientGen = namesGen concat contactGen concat addressWithLocationGen

```


## <a name="installation"><a/> Installation
The library is compatible with Scala 3.1.3 or greater

```sbt     

libraryDependencies += "com.github.imrafaelmerino" %% "json-scala-values" % "5.1.0"

```

Disclaimer: I'm no longer maintain previous releases for Scala 2 and early versions of Dotty. 
Too much to handle for just one person... 

## <a name="rp"><a/> Related projects
json-values was first developed in [Java](https://github.com/imrafaelmerino/json-values).

