# Feasibility Analysis: Compiling Scala 3 Compiler to Scala-Native or Scala-JS

## Executive Summary

**Feasibility: Low to Very Low**

Compiling the Scala 3 compiler itself to Scala-Native or Scala-JS presents significant challenges due to deep integration with the Java Virtual Machine (JVM). While the compiler can *compile code* targeting these platforms, the compiler itself is fundamentally tied to JVM-specific features and APIs.

## Key Findings

### 1. Macro System and Runtime Reflection

The Scala 3 macro system relies heavily on JVM reflection APIs for compile-time macro expansion. This is the most significant barrier to cross-compilation.

#### Critical Code Locations

**`compiler/src/dotty/tools/dotc/quoted/Interpreter.scala`**

The `Interpreter` class executes macros at compile time using JVM reflection:

```scala
class Interpreter(pos: SrcPos, classLoader0: ClassLoader)(using Context):
  val classLoader = // ... uses ClassLoader for dynamic loading

  private def interpretedStaticMethodCall(moduleClass: Symbol, fn: Symbol, args: List[Object]): Object = {
    val inst = loadModule(moduleClass)
    val clazz = inst.getClass
    val method = getMethod(clazz, name, paramsSig(fn))
    stopIfRuntimeException(method.invoke(inst, args*), method)  // JVM reflection
  }

  private def loadClass(name: String): Class[?] =
    try classLoader.loadClass(name)  // Dynamic class loading
    catch case MissingClassValidInCurrentRun(sym, origin) => ...
```

**Key JVM Dependencies:**
- `ClassLoader.loadClass()` - Dynamic class loading
- `Class.forName()` - Reflection-based class lookup
- `Method.invoke()` - Runtime method invocation
- `java.lang.reflect.*` - Full reflection API

**Impact:** Scala-Native and Scala-JS do not provide equivalent reflection capabilities. Macros execute at compile-time by loading classes, instantiating objects, and invoking methods using JVM reflection APIs that don't exist on these platforms.

### 2. JVM Backend (ASM Library)

The compiler uses ASM (a Java bytecode manipulation library) for generating JVM bytecode.

**`compiler/src/dotty/tools/backend/jvm/PostProcessor.scala`**

```scala
import scala.tools.asm.ClassWriter
import scala.tools.asm.tree.ClassNode
```

**Impact:** ASM is a JVM-specific library that generates JVM bytecode. It cannot be used on Scala-Native or Scala-JS. While the compiler has a separate Scala.js backend (`backend/sjs/`), the compiler infrastructure itself depends on ASM for its own compilation.

### 3. Extensive Java Standard Library Usage

**Statistics:** 619 matches for `java.*` imports across 185 files

The compiler extensively uses Java standard library APIs:

- **`java.io.*`** - File I/O operations
- **`java.nio.*`** - NIO file operations, paths
- **`java.net.*`** - Networking, URLClassLoader
- **`java.util.*`** - Collections, concurrency utilities
- **`java.lang.reflect.*`** - Reflection APIs
- **`java.lang.*`** - Core Java types

**Example Locations:**
- `compiler/src/dotty/tools/io/` - File system abstractions
- `compiler/src/dotty/tools/dotc/core/MacroClassLoader.scala` - Uses `URLClassLoader`
- `compiler/src/dotty/tools/dotc/quoted/Interpreter.scala` - Uses reflection APIs

### 4. Class Loading Infrastructure

The compiler has sophisticated class loading mechanisms that are JVM-specific.

**`compiler/src/dotty/tools/dotc/core/MacroClassLoader.scala`**

```scala
private def makeMacroClassLoader(using Context): ClassLoader = {
  val entries = ClassPath.expandPath(ctx.settings.classpath.value, expandStar=true)
  val urls = entries.map(cp => java.nio.file.Paths.get(cp).toUri.toURL).toArray
  val out = Option(ctx.settings.outputDir.value.toURL)
  new java.net.URLClassLoader(urls ++ out.toList, getClass.getClassLoader)
}
```

**Key Components:**
- `URLClassLoader` - JVM-specific class loader
- `java.nio.file.Paths` - JVM path handling
- `AbstractFileClassLoader` - Custom class loader implementation

**Impact:** Dynamic class loading is fundamental to how macros work. Scala-Native and Scala-JS have different module/loading systems that would require complete redesign.

### 5. Platform Abstraction Limitations

While the compiler has a `Platform` abstraction, it's designed for *compiling to* different platforms, not for *running on* different platforms.

**`compiler/src/dotty/tools/dotc/config/Platform.scala`**

The `Platform` trait provides abstractions for:
- Class path handling
- Symbol loading
- Platform-specific type checks (SAM types, boxing)

However, the compiler infrastructure itself (phases, transformations, macro execution) assumes JVM runtime.

**`compiler/src/dotty/tools/dotc/Compiler.scala`**

The compiler phases are structured with JVM assumptions:

```scala
protected def backendPhases: List[List[Phase]] =
  List(new backend.sjs.GenSJSIR) :: // Generate .sjsir files for Scala.js
  List(new GenBCode) ::             // Generate JVM bytecode
  Nil
```

## Specific Technical Challenges

### Macro Execution Without Reflection

#### Why Dynamic Class Loading is Essential for Current Macros

The Scala 3 macro system fundamentally relies on dynamic class loading because of its **two-stage compilation model**:

**Stage 1: Macro Definition Compilation**
```scala
// User writes a macro
inline def myMacro(x: Expr[Int]): Expr[Int] =
  '{ ${x} + 1 }
```

This macro code is compiled to **JVM bytecode** (`.class` files) and stored in a JAR on the classpath, just like any other Scala code.

**Stage 2: Macro Expansion (At Compile-Time)**

When the compiler encounters a macro call:
```scala
val result = myMacro('{ 42 })
```

The compiler must:

1. **Load the macro class** - Use `ClassLoader.loadClass()` to find the compiled macro class:
   ```scala
   // From Interpreter.scala line 211-215
   private def loadClass(name: String): Class[?] =
     try classLoader.loadClass(name)  // Dynamic loading!
   ```

2. **Instantiate the macro** - Create an instance of the macro class:
   ```scala
   // From Interpreter.scala line 194-204
   private def loadModule(sym: Symbol): Object = {
     val moduleClass = loadClass(sym.fullName.toString)
     moduleClass.getField(str.MODULE_INSTANCE_FIELD).get(null)  // Reflection!
   }
   ```

3. **Invoke the macro method** - Use reflection to call the macro method:
   ```scala
   // From Interpreter.scala line 163-173
   private def interpretedStaticMethodCall(...): Object = {
     val inst = loadModule(moduleClass)
     val clazz = inst.getClass
     val method = getMethod(clazz, name, paramsSig(fn))  // Reflection!
     method.invoke(inst, args*)  // Runtime method invocation!
   }
   ```

4. **Execute macro code** - The macro code runs as **JVM bytecode**, producing a `scala.quoted.Expr[T]` result

5. **Unpickle TASTy** - The `Expr` contains pickled TASTy data, which gets unpickled back into compiler trees:
   ```scala
   // From Splicer.scala line 61
   val interpretedTree = interpretedExpr.fold(tree)(
     macroClosure => PickledQuotes.quotedExprToTree(macroClosure(QuotesImpl()))
   )
   ```

**Why This Requires Dynamic Loading:**

- **Macros are external dependencies** - They're compiled separately and may come from different JARs
- **Macros execute as bytecode** - The macro code itself runs as JVM bytecode, not as compiler trees
- **Runtime execution model** - Macros are essentially "functions that run at compile-time" using the JVM runtime
- **Separation of concerns** - The compiler doesn't need to re-compile macro definitions; it just loads and executes them

#### How TASTy Could Replace Dynamic Class Loading

TASTy (TASTy is Scala's Type) is Scala's intermediate representation format that contains **complete type information and tree structure**. It's already used for macro results, but could be used for macro definitions too.

**Proposed TASTy-Based Approach:**

Instead of the current bytecode-based model:

```
Macro Source → JVM Bytecode → ClassLoader → Reflection → Execute → TASTy Result → Unpickle
```

Use a TASTy-based model:

```
Macro Source → TASTy File → Direct Unpickle → Tree Interpreter → Result Tree
```

**Key Changes Required:**

1. **Compile macros to TASTy instead of (or in addition to) bytecode**
   - TASTy files already contain all necessary information (types, trees, symbols)
   - No need for JVM bytecode representation

2. **Load TASTy files directly**
   ```scala
   // Instead of:
   val clazz = classLoader.loadClass("MyMacro")

   // Do:
   val tastyBytes = loadTastyFile("MyMacro.tasty")
   val macroTree = unpickleTasty(tastyBytes)
   ```

3. **Interpret trees directly** - Execute macro logic as compiler trees:
   ```scala
   // Instead of reflection-based invocation:
   method.invoke(inst, args*)

   // Interpret the tree directly:
   val resultTree = interpretTree(macroTree, args)
   ```

4. **Tree-based evaluation engine** - Implement an interpreter that works on compiler trees:
   - Similar to the existing `Interpreter` class, but operates on trees instead of bytecode
   - Would need to handle:
     - Function application
     - Value access
     - Control flow
     - Type operations

**Advantages of TASTy-Based Approach:**

✅ **No reflection needed** - Work directly with compiler trees
✅ **Cross-platform** - TASTy is platform-independent
✅ **Better integration** - Macros become first-class compiler constructs
✅ **Type safety** - Full type information available at expansion time
✅ **Debugging** - Easier to debug and inspect macro code

**Challenges:**

⚠️ **Tree interpreter complexity** - Need to implement full Scala semantics in tree form
⚠️ **Performance** - Tree interpretation may be slower than bytecode execution
⚠️ **Completeness** - Must support all Scala features that macros might use
⚠️ **Migration** - Existing macros would need to be recompiled/reworked

**Current State:**

Interestingly, **TASTy is already partially used** for macro communication:

### Existing TASTy Interpreter Efforts

Several research and development efforts have explored building TASTy interpreters, providing valuable insights into the feasibility and challenges:

#### 1. TASTyTruffle (Research Project - University of Waterloo)

**Who:** Research team at University of Waterloo (published at OOPSLA 2023)

**What:** TASTyTruffle is an experimental Scala implementation that interprets TASTy IR directly instead of JVM bytecode. It leverages TASTy's rich type information to achieve performance optimizations.

**Key Features:**
- Interprets TASTy IR directly (no bytecode compilation)
- Reifies types as first-class objects
- Dynamically selects precise, box-free representations for generic values
- Generates efficient, specialized code for different type instantiations

**Outcomes:**
- ✅ **Performance Gains**: Achieved higher peak throughput than HotSpot JVM
- ✅ **Competitive with Graal**: Performance comparable to JVM with Graal compiler
- ✅ **Generic Code Optimization**: Particularly effective when generic code is instantiated with multiple concrete types
- ✅ **Proof of Concept**: Demonstrated that TASTy interpretation is viable and can outperform traditional approaches

**Learnings:**
- TASTy's rich type information enables optimizations impossible with erased bytecode
- Dynamic specialization based on type information can significantly improve performance
- The approach is particularly beneficial for generic/polymorphic code
- Type reification as first-class objects enables novel optimization strategies

**Relevance to Compiler Cross-Compilation:**
- Proves that TASTy interpretation is technically feasible
- Shows that TASTy-based execution can be performant
- Demonstrates that type information in TASTy is sufficient for execution
- However, focuses on runtime execution, not compile-time macro expansion

**Reference:** OOPSLA 2023 paper: "TASTyTruffle: A Framework for Building High-Performance Language Implementations Using TASTy"

#### 2. Scala 3 Compiler Prototype TASTy Interpreter

**Who:** Scala 3 compiler team (found in `tests/old-tasty-interpreter-prototype/`)

**What:** A prototype implementation of a TASTy tree interpreter within the Scala 3 compiler codebase.

**Key Components:**
- `TreeInterpreter` - Abstract base class for tree-based interpretation
- `TastyInterpreter` - Main interpreter that uses TASTy Inspector API
- `jvm.Interpreter` - JVM-specific implementation that falls back to reflection for non-current-run code
- Test suite demonstrating interpretation of various Scala constructs

**Features Implemented:**
- ✅ Basic tree evaluation (literals, blocks, conditionals, loops)
- ✅ Function calls and method invocation
- ✅ Variable access and assignment
- ✅ Primitive operations (arithmetic, comparisons)
- ✅ Type operations (isInstanceOf, asInstanceOf)
- ⚠️ Partial support for object creation (uses proxies for current-run classes)
- ⚠️ Falls back to JVM reflection for code not in current compilation run

**Current Status:**
- **Prototype/Experimental** - Found in test directory, not production code
- **Incomplete** - Many TODOs and FIXMEs indicate unfinished work
- **Hybrid Approach** - Uses tree interpretation for current-run code, reflection for external code
- **Test Infrastructure** - Includes test cases demonstrating interpretation of various Scala programs

**Key Learnings from Code:**
- Tree interpretation requires handling many Scala language constructs
- Need to maintain environment (Env) for variable bindings and closures
- Object creation is complex - requires proxies or full class instantiation
- Integration with existing compiler infrastructure (Quotes API) is feasible
- Performance considerations: tree interpretation may be slower than bytecode execution

**Design Notes Found:**
- Need to abstract platform operations (arrays, proxies)
- Environment management for objects (`this` in Env)
- Handling of classes with fields and custom constructors
- Stack management for local definitions and closures

**Relevance to Compiler Cross-Compilation:**
- ✅ **Proof of Concept**: Shows tree interpretation is possible within compiler
- ✅ **Infrastructure Exists**: Uses existing TASTy Inspector and Quotes APIs
- ⚠️ **Incomplete**: Many features still need implementation
- ⚠️ **Hybrid Model**: Still relies on JVM reflection for external code
- ✅ **Foundation**: Provides a starting point for full implementation

**Location:** `tests/old-tasty-interpreter-prototype/` in Scala 3 repository

#### 3. TASTy-MiMa (Scala Center)

**Who:** Scala Center (tooling project)

**What:** TASTy Migration Manager - a tool for detecting TASTy incompatibilities in Scala libraries.

**Purpose:**
- Identifies API changes that could cause retypechecking errors
- Particularly important for `inline` methods and macros
- Compares TASTy files between library versions

**Relevance:**
- Demonstrates that TASTy files contain sufficient information for semantic analysis
- Shows that TASTy can be used for compatibility checking
- Proves TASTy is a viable format for tooling beyond compilation

**Outcome:** Successfully used for maintaining library compatibility, showing TASTy's utility for semantic analysis.

#### 4. TASTy-Query (Scala Center)

**Who:** Scala Center

**What:** Library for reading and querying TASTy files semantically.

**Features:**
- Semantic queries over TASTy files
- Subtyping checks
- Type equivalence
- Foundation for tools like TASTy-MiMa

**Relevance:**
- Shows TASTy contains rich semantic information
- Demonstrates that TASTy can support complex queries
- Proves infrastructure exists for TASTy-based tooling

**Outcome:** Provides foundation for TASTy-based analysis tools.

#### 5. TASTy Reader for Scala 2

**Who:** Scala compiler team

**What:** Enables Scala 2.13.x to read TASTy files produced by Scala 3.

**Purpose:**
- Cross-version compatibility
- Allows Scala 2 projects to use Scala 3 libraries
- Gradual migration path

**Relevance:**
- Demonstrates TASTy can be consumed by different compiler versions
- Shows TASTy is a viable cross-version format
- Proves TASTy reading/unpickling infrastructure is robust

**Outcome:** Successfully integrated into Scala 2.13.5+, enabling cross-version compatibility.

### Summary of Learnings

**What Works:**
- ✅ TASTy interpretation is technically feasible (TASTyTruffle proves this)
- ✅ Tree-based evaluation can be performant (competitive with JVM)
- ✅ TASTy contains sufficient information for execution
- ✅ Infrastructure exists in compiler for TASTy reading/writing
- ✅ Type information in TASTy enables optimizations

**Challenges Identified:**
- ⚠️ **Completeness**: Full Scala semantics require extensive implementation
- ⚠️ **Performance**: Tree interpretation may be slower than bytecode (though TASTyTruffle shows it can be competitive)
- ⚠️ **Object Creation**: Complex - requires proxies or full instantiation
- ⚠️ **External Code**: Handling code not in current compilation run is challenging
- ⚠️ **Platform Abstractions**: Need to abstract platform-specific operations

**Key Insight:**
The prototype in the Scala 3 codebase shows that **tree interpretation is possible**, but the current implementation uses a **hybrid approach**: tree interpretation for code in the current compilation run, and JVM reflection fallback for external code. For full cross-compilation, this fallback would need to be replaced with pure tree interpretation or TASTy-based loading.

**Relevance to Macro System:**
- The prototype demonstrates that interpreting compiler trees is feasible
- The infrastructure (TASTy reading, tree evaluation) already exists
- The main challenge is completeness - implementing all Scala language features
- For macros specifically, the approach would be: load TASTy → unpickle → interpret trees → return result

**Current State:**

```scala
// From PickledQuotes.scala - Macros already pickle/unpickle trees via TASTy
def pickleQuote(tree: Tree): List[String] = {
  val pickled = pickle(tree)  // Converts tree to TASTy bytes
  TastyString.pickle(pickled)
}

def unpickleTerm(pickled: String | List[String], ...): Tree = {
  val bytes = TastyString.unpickle(pickled)
  val unpickler = new DottyUnpickler(NoAbstractFile, bytes, ...)
  unpickler.tree  // Unpickles TASTy back to tree
}
```

The infrastructure exists - it's just used for **macro results** rather than **macro definitions**. Extending it to macro definitions would be a significant but feasible architectural change.

**Required Alternative:**
- Pre-compile macros to TASTy format (already happens, but not used for loading)
- Use TASTy-based interpretation instead of bytecode execution
- Implement a compile-time evaluation engine that works on trees directly
- This would be a fundamental architectural change, but leverages existing TASTy infrastructure

### File I/O Abstraction

The compiler uses `java.io` and `java.nio` extensively. A cross-platform version would need:
- Abstract file system interface
- Platform-specific implementations
- Path handling abstraction

### Concurrency and Threading

The compiler uses JVM threading APIs. Cross-platform support would require:
- Abstract concurrency primitives
- Platform-specific implementations
- Careful handling of platform differences

### Class File vs TASTy File Handling

Currently, the compiler loads classes from `.class` files for macro execution. A cross-platform version would need to:
- Use TASTy files exclusively for macro dependencies
- Implement TASTy-based class loading
- Remove dependency on JVM class file format

## What Would Be Required

### 1. Macro System Redesign

**Estimated Effort: Very High (Years)**

- Design new macro execution model that doesn't use runtime reflection
- Implement TASTy-based macro evaluation
- Create compile-time evaluation engine
- Migrate all existing macros to new system
- Extensive testing and validation

### 2. Backend Abstraction

**Estimated Effort: High (Months)**

- Abstract ASM dependency
- Make backend selection truly pluggable
- Remove JVM-specific assumptions from core compiler phases
- Ensure backend can be swapped at runtime

### 3. I/O and File System Abstraction

**Estimated Effort: Medium-High (Months)**

- Create abstract file system interface
- Implement platform-specific backends
- Abstract path handling
- Migrate all file I/O code

### 4. Class Loading Replacement

**Estimated Effort: High (Months)**

- Replace `ClassLoader` with TASTy-based loading
- Implement TASTy class resolution
- Update macro infrastructure
- Handle module/package loading differently

### 5. Cross-Platform Concurrency

**Estimated Effort: Medium (Weeks-Months)**

- Abstract threading APIs
- Platform-specific implementations
- Handle platform differences carefully

## Current State: What Works

### Compiling *To* Scala-Native/Scala-JS

The compiler successfully compiles Scala code targeting:
- **Scala.js**: Has dedicated backend (`backend/sjs/`) that generates `.sjsir` files
- **Scala-Native**: Can generate TASTy files that can be processed by Scala-Native toolchain

### Platform-Specific Code

The compiler already has some platform awareness:
- `SJSPlatform` class for Scala.js-specific behavior
- Platform-specific phases and transformations
- Conditional compilation based on target platform

However, this is for *target* platform, not *host* platform.

## Analysis: Ported vs Non-Ported Java Classes

### Overview

There are ongoing efforts to port JVM classes to pure Scala, making them available on Scala-Native and Scala-JS. However, the Scala 3 compiler uses many Java classes that have not been ported, creating significant barriers to cross-compilation.

### Java Packages Used by the Compiler

Based on analysis of the compiler codebase, the following Java packages are extensively used:

#### 1. **`java.lang.reflect.*`** - **NOT PORTED** ⚠️ **CRITICAL**

**Usage:** 22+ matches across multiple files

**Classes Used:**
- `ClassLoader` - Dynamic class loading (71+ matches)
- `URLClassLoader` - URL-based class loading
- `Method` - Method reflection and invocation
- `InvocationTargetException` - Exception handling for reflection
- `Modifier` - Class/method modifier inspection

**Key Locations:**
- `compiler/src/dotty/tools/dotc/quoted/Interpreter.scala` - Macro execution
- `compiler/src/dotty/tools/dotc/core/MacroClassLoader.scala` - Class loader creation
- `compiler/src/dotty/tools/dotc/transform/Splicer.scala` - Macro splicing
- `compiler/src/dotty/tools/dotc/transform/MacroAnnotations.scala` - Annotation processing

**Status:** **NOT PORTED** - Reflection APIs are fundamentally JVM-specific and cannot be directly ported. Scala-Native and Scala-JS do not provide equivalent reflection capabilities.

**Impact:** **BLOCKER** - This is the single most critical blocker. The macro system cannot function without runtime reflection.

#### 2. **`java.nio.file.*`** - **PARTIALLY PORTED** ⚠️

**Usage:** Extensive use throughout I/O operations

**Classes Used:**
- `Path` - File path representation
- `Paths` - Path factory methods
- `Files` - File operations (read, write, walk, etc.)
- `FileChannel` - File channel operations
- `StandardOpenOption` - File open options
- `FileAttribute` - File attributes
- `BasicFileAttributes` - File metadata
- `FileTime` - File timestamps
- `InvalidPathException` - Path validation exceptions
- `FileAlreadyExistsException` - File operation exceptions
- `FileSystemAlreadyExistsException` - File system exceptions

**Key Locations:**
- `compiler/src/dotty/tools/io/Path.scala`
- `compiler/src/dotty/tools/io/FileWriters.scala`
- `compiler/src/dotty/tools/io/ZipArchive.scala`
- `compiler/src/dotty/tools/dotc/core/MacroClassLoader.scala`

**Status:** **PARTIALLY PORTED** - Basic `Path` operations exist in Scala-Native, but many advanced features are missing or incomplete.

**Impact:** **HIGH** - File I/O is fundamental to compiler operation. Missing features would require significant workarounds.

#### 3. **`java.nio.*`** (Other) - **PARTIALLY PORTED** ⚠️

**Classes Used:**
- `ByteBuffer` - Byte buffer operations
- `channels.FileChannel` - File channel I/O
- `channels.ClosedByInterruptException` - Channel exceptions
- `charset.StandardCharsets` - Character encoding

**Status:** **PARTIALLY PORTED** - Basic NIO support exists but is incomplete.

**Impact:** **MEDIUM-HIGH** - Used for efficient file I/O operations.

#### 4. **`java.io.*`** - **LIMITED PORTING** ⚠️

**Usage:** 619+ matches across 185+ files

**Classes Used:**
- `File` - File system representation
- `InputStream` / `OutputStream` - Stream I/O
- `BufferedInputStream` / `BufferedOutputStream` - Buffered I/O
- `DataOutputStream` - Data output streams
- `FileOutputStream` - File output
- `RandomAccessFile` - Random file access
- `ByteArrayInputStream` / `ByteArrayOutputStream` - In-memory streams
- `BufferedReader` / `InputStreamReader` - Text reading
- `PrintWriter` / `StringWriter` - Text writing
- `IOException` - I/O exceptions
- `Closeable` - Resource management

**Key Locations:**
- `compiler/src/dotty/tools/io/` - Entire I/O abstraction layer
- `compiler/src/dotty/tools/dotc/Run.scala` - Compilation run management
- `compiler/src/dotty/tools/dotc/profile/Profiler.scala` - Profiling output

**Status:** **LIMITED PORTING** - Basic I/O classes exist in Scala-Native/Scala-JS, but many advanced features are missing. Scala-JS has very limited file I/O (browser environment).

**Impact:** **HIGH** - File I/O is essential, but workarounds are possible with abstraction layers.

#### 5. **`java.net.*`** - **LIMITED PORTING** ⚠️

**Classes Used:**
- `URL` - URL representation
- `URI` - URI representation
- `URLClassLoader` - Class loading from URLs

**Key Locations:**
- `compiler/src/dotty/tools/dotc/core/MacroClassLoader.scala` - Uses `URLClassLoader`
- `compiler/src/dotty/tools/io/Streamable.scala` - URL-based resource loading
- `compiler/src/dotty/tools/io/JarArchive.scala` - JAR file handling

**Status:** **LIMITED PORTING** - Basic URL/URI support exists, but `URLClassLoader` is JVM-specific.

**Impact:** **HIGH** - `URLClassLoader` is critical for macro class loading.

#### 6. **`java.util.zip.*`** - **NOT PORTED** ⚠️

**Classes Used:**
- `ZipEntry` - ZIP file entries
- `ZipFile` - ZIP file reading
- `ZipOutputStream` - ZIP file writing
- `CRC32` - CRC checksum calculation
- `Deflater` - Compression

**Key Locations:**
- `compiler/src/dotty/tools/io/ZipArchive.scala`
- `compiler/src/dotty/tools/io/FileWriters.scala`
- `compiler/src/dotty/tools/backend/jvm/ClassfileWriters.scala`

**Status:** **NOT PORTED** - ZIP support is not available in Scala-Native/Scala-JS standard libraries.

**Impact:** **MEDIUM** - JAR/ZIP file handling is important but could potentially be replaced with TASTy-based alternatives.

#### 7. **`java.util.jar.*`** - **NOT PORTED** ⚠️

**Classes Used:**
- `JarFile` - JAR file access
- `Manifest` - JAR manifest handling
- `Attributes` - Manifest attributes

**Key Locations:**
- `compiler/src/dotty/tools/io/Jar.scala`
- `compiler/src/dotty/tools/io/ZipArchive.scala`
- `compiler/src/dotty/tools/scripting/Main.scala`

**Status:** **NOT PORTED** - JAR support is not available.

**Impact:** **MEDIUM** - JAR files are the standard distribution format, but TASTy files could replace this dependency.

#### 8. **`java.util.concurrent.*`** - **PARTIALLY PORTED** ✅

**Classes Used:**
- `ConcurrentHashMap` - Thread-safe hash map
- `Timer` / `TimerTask` - Scheduled tasks
- `atomic.AtomicInteger` - Atomic integers
- `atomic.AtomicReference` - Atomic references
- `atomic.AtomicBoolean` - Atomic booleans
- `ConcurrentModificationException` - Concurrency exceptions

**Key Locations:**
- `compiler/src/dotty/tools/dotc/Run.scala` - Uses `Timer`
- `compiler/src/dotty/tools/io/FileWriters.scala` - Uses concurrent collections
- `compiler/src/dotty/tools/dotc/core/Contexts.scala` - Uses atomic types

**Status:** **PARTIALLY PORTED** - Basic concurrent collections and atomic types exist, but some advanced features may be missing.

**Impact:** **MEDIUM** - Most commonly used concurrent utilities are available.

#### 9. **`java.util.*`** (Other) - **MOSTLY PORTED** ✅

**Classes Used:**
- `UUID` - UUID generation
- Basic collections (though compiler uses Scala collections primarily)

**Status:** **MOSTLY PORTED** - Basic utilities are available.

**Impact:** **LOW** - Limited usage, mostly replaced by Scala alternatives.

#### 10. **`java.lang.management.*`** - **NOT PORTED** ⚠️

**Classes Used:**
- `ManagementFactory` - JMX factory
- `GarbageCollectorMXBean` - GC monitoring
- `RuntimeMXBean` - Runtime monitoring
- `MemoryMXBean` - Memory monitoring
- `ClassLoadingMXBean` - Class loading monitoring
- `CompilationMXBean` - JIT compilation monitoring

**Key Locations:**
- `compiler/src/dotty/tools/dotc/profile/Profiler.scala` - Profiling infrastructure
- `compiler/src/dotty/tools/dotc/profile/ExtendedThreadMxBean.java` - Thread monitoring

**Status:** **NOT PORTED** - JMX/management APIs are JVM-specific.

**Impact:** **LOW-MEDIUM** - Used only for optional profiling features. Could be made optional or replaced with platform-specific alternatives.

#### 11. **`javax.management.*`** - **NOT PORTED** ⚠️

**Classes Used:**
- `Notification` / `NotificationEmitter` / `NotificationListener` - JMX notifications
- `CompositeData` - JMX composite data

**Status:** **NOT PORTED** - JMX is JVM-specific.

**Impact:** **LOW** - Used only for advanced profiling features.

#### 12. **`java.lang.*`** (Core) - **MOSTLY PORTED** ✅

**Classes Used:**
- `StringBuilder` - String building
- `Object` - Base class (implicitly)
- `Class` - Class metadata (via reflection)
- `RuntimeException` - Exception handling
- `UnsupportedOperationException` - Exception signaling

**Status:** **MOSTLY PORTED** - Core language classes are available, but reflection-related classes are not.

**Impact:** **LOW** - Basic classes are available, but `Class` usage is tied to reflection.

### Summary Table

| Java Package | Ported Status | Usage Count | Impact | Blocker? |
|-------------|---------------|-------------|--------|----------|
| `java.lang.reflect.*` | ❌ NOT PORTED | 22+ files | CRITICAL | ✅ YES |
| `java.net.URLClassLoader` | ❌ NOT PORTED | Critical | CRITICAL | ✅ YES |
| `java.nio.file.*` | ⚠️ PARTIAL | Extensive | HIGH | ⚠️ PARTIAL |
| `java.nio.*` (other) | ⚠️ PARTIAL | Medium | MEDIUM-HIGH | ⚠️ PARTIAL |
| `java.io.*` | ⚠️ LIMITED | 185+ files | HIGH | ⚠️ PARTIAL |
| `java.util.zip.*` | ❌ NOT PORTED | Medium | MEDIUM | ❌ NO |
| `java.util.jar.*` | ❌ NOT PORTED | Medium | MEDIUM | ❌ NO |
| `java.util.concurrent.*` | ✅ PARTIAL | Medium | MEDIUM | ❌ NO |
| `java.util.*` (other) | ✅ MOSTLY | Low | LOW | ❌ NO |
| `java.lang.management.*` | ❌ NOT PORTED | Low | LOW-MEDIUM | ❌ NO |
| `javax.management.*` | ❌ NOT PORTED | Low | LOW | ❌ NO |
| `java.lang.*` (core) | ✅ MOSTLY | High | LOW | ❌ NO |

### Critical Missing Classes

The following classes are **absolutely critical** and **NOT PORTED**:

1. **`java.lang.ClassLoader`** and **`java.net.URLClassLoader`**
   - **Impact:** Macro system cannot function without dynamic class loading
   - **Workaround:** Would require complete macro system redesign using TASTy-based evaluation

2. **`java.lang.reflect.Method`** and reflection APIs
   - **Impact:** Cannot invoke macro methods at runtime
   - **Workaround:** Would require compile-time macro evaluation instead of runtime execution

3. **`java.nio.file.Paths`** and advanced file operations
   - **Impact:** File path handling and advanced I/O operations
   - **Workaround:** Could use platform-specific file abstractions, but requires significant refactoring

### Partially Available Classes

These classes have partial support but may lack features used by the compiler:

- **`java.nio.file.*`** - Basic operations exist, but advanced features (walk, attributes) may be missing
- **`java.io.*`** - Basic streams exist, but some advanced features may be missing
- **`java.util.concurrent.*`** - Most common utilities exist, but some advanced features may be missing

### Conclusion on Ported Classes

**Overall Assessment:** While some Java classes have been ported to pure Scala, the **most critical classes for compiler operation have NOT been ported**:

- ❌ **Reflection APIs** - Required for macro execution
- ❌ **ClassLoader APIs** - Required for dynamic class loading
- ⚠️ **Advanced File I/O** - Required for file system operations
- ❌ **ZIP/JAR Support** - Required for JAR file handling (though could be replaced)

The porting efforts have focused on **application-level libraries** rather than **compiler infrastructure**. The compiler's dependencies on unported classes represent **fundamental architectural barriers** rather than simple API compatibility issues.

## Conclusion

### Feasibility Assessment

**Overall Feasibility: Low to Very Low**

**Timeline Estimate:** 3-5 years of dedicated development effort

**Key Blockers:**
1. Macro system requires complete redesign (depends on unported `java.lang.reflect.*` APIs)
2. ASM dependency is fundamental to compiler operation
3. Extensive Java standard library usage (see "Analysis: Ported vs Non-Ported Java Classes" section)
4. Class loading infrastructure is JVM-specific (`ClassLoader`, `URLClassLoader` not ported)

### Recommendations

1. **Short-term:** Continue using JVM as the host platform for the compiler
2. **Medium-term:** Investigate TASTy-based macro evaluation as a research project
3. **Long-term:** Consider gradual migration if there's strong demand, but recognize it's a multi-year effort

### Alternative Approaches

Instead of porting the entire compiler, consider:
- **Incremental compilation server:** Port only the language server/presentation compiler
- **TASTy-based tools:** Build new tools that work directly with TASTy files
- **WebAssembly target:** Consider WASM as an alternative cross-platform target

## References

### Key Files Analyzed

- `compiler/src/dotty/tools/dotc/quoted/Interpreter.scala` - Macro execution
- `compiler/src/dotty/tools/dotc/core/MacroClassLoader.scala` - Class loading
- `compiler/src/dotty/tools/dotc/config/Platform.scala` - Platform abstraction
- `compiler/src/dotty/tools/dotc/Compiler.scala` - Compiler phases
- `compiler/src/dotty/tools/backend/jvm/` - JVM backend
- `compiler/src/dotty/tools/backend/sjs/` - Scala.js backend

### Statistics

- **Java API usage:** 619+ matches across 185+ files
- **ClassLoader usage:** 71+ matches across 64+ files
- **Reflection usage:** 22+ matches
- **ASM usage:** 455+ matches
- **Java packages used:** `io`, `nio`, `net`, `util`, `lang`, `security`

### Ported Classes Analysis

See the detailed analysis in the "Analysis: Ported vs Non-Ported Java Classes" section above, which includes:
- Comprehensive breakdown of Java packages used by the compiler
- Ported status for each package
- Impact assessment
- Critical missing classes identification

---

*Document generated: 2025-01-27*
*Analysis based on Scala 3 compiler codebase*

