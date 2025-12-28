# Browser-Based Scala Compiler Design Document (Simple)
## Direct TASTy Execution - No Macros, No Backend

**Date:** 2025-11-30
**Status:** Design Document
**Target:** Execute simple Scala code directly in browser via TASTy interpretation

---

## Executive Summary

This document outlines a simplified browser-based Scala compiler that compiles Scala source code to TASTy format and then **directly executes** the TASTy using a tree interpreter. This approach:

- ✅ **Skips macro expansion** - No macro support needed
- ✅ **Skips backend code generation** - No bytecode or JS IR generation
- ✅ **Direct execution** - TASTy → Tree Interpreter → Execution
- ✅ **Simpler architecture** - Fewer components, easier to implement
- ✅ **Self-contained** - All dependencies bundled, no HTTP required

**Key Difference from Full Design:**

| Aspect | Full Design | Simple Design |
|--------|-------------|---------------|
| **Macros** | Supported via TASTy interpreter | Not supported |
| **Execution** | Generate TASTy → Expand macros → Generate final TASTy | Generate TASTy → Execute directly |
| **Backend** | TASTy generation only | TASTy generation + execution |
| **Use Case** | Compile Scala code | Compile AND execute Scala code |

---

## Architecture Overview

### High-Level Flow

```
User Scala Code (browser)
    ↓
Compiler Frontend (Parser → Typer → Pickler)
    ↓
TASTy Generation (in-memory VirtualFile)
    ↓
TASTy Unpickler (load TASTy back to trees)
    ↓
Tree Interpreter (PureTastyInterpreter)
    ↓
Direct Execution (interpret trees, produce results)
    ↓
Return Results to Browser
```

### Component Breakdown

| Component | Purpose | Location | Status |
|-----------|---------|----------|--------|
| **Compiler Frontend** | Parse, type-check, generate TASTy | `compiler/src/dotty/tools/dotc/` | ✅ Exists |
| **TASTy Generator** | Serialize trees to TASTy format | `compiler/src/dotty/tools/dotc/core/tasty/` | ✅ Exists |
| **TASTy Unpickler** | Load TASTy back to compiler trees | `compiler/src/dotty/tools/dotc/core/tasty/` | ✅ Exists |
| **Tree Interpreter** | Execute trees directly | `tests/old-tasty-interpreter-prototype/` | ⚠️ ~40% complete |
| **Stdlib TASTy Bundle** | Pre-compiled standard library | To be generated | 📝 To do |
| **Execution Engine** | Run interpreted code | To be implemented | 📝 To do |

---

## What Needs to Be Bundled

### 1. Scala 3 Compiler (Frontend Only)

**Required Components:**
- Parser (`compiler/src/dotty/tools/dotc/parsing/`)
- Typer (`compiler/src/dotty/tools/dotc/typer/`)
- Pickler (`compiler/src/dotty/tools/dotc/transform/Pickler.scala`)
- TASTy format (`compiler/src/dotty/tools/dotc/core/tasty/`)
- TASTy Unpickler (`compiler/src/dotty/tools/dotc/core/tasty/TreeUnpickler.scala`)
- VirtualFile system (`compiler/src/dotty/tools/io/VirtualFile.scala`)

**Not Required:**
- Macro expansion phases (`Inlining`, `Splicing`, `PickleQuotes`)
- JVM backend (`backend/jvm/`)
- Scala.js backend (`backend/sjs/`)
- ASM library
- ClassLoader infrastructure

**Estimated Size:** ~2-3 MB (compressed) for frontend phases only

### 2. Standard Library TASTy Files

**What to Bundle:**

Same as full design - stdlib TASTy files needed for:
- Type checking user code
- Executing stdlib methods during interpretation

| Module | Purpose | Criticality | Estimated Size |
|--------|---------|-------------|----------------|
| `scala-library` | Core types (`String`, `Int`, `Boolean`, etc.) | **Critical** | ~500 KB |
| `scala-library` (collections) | `List`, `Option`, `Seq`, `Map`, `Set` | **Critical** | ~800 KB |
| `scala-library` (other) | `Tuple`, `Function`, `Product`, etc. | **High** | ~300 KB |

**Total Estimated Size:** ~1.5-2 MB (compressed)

### 3. Tree Interpreter (Execution Engine)

**Required Components:**
- `PureTastyInterpreter.scala` - Core interpreter
- `TastyLoader.scala` - TASTy definition loader
- Intrinsics system - Platform bridges for stdlib types
- **Execution runtime** - New component to handle program execution

**Current Status:** ~40% complete (see `tests/old-tasty-interpreter-prototype/notes.md`)

**What's Working:**
- Control flow (if/else, while, blocks)
- Match expressions (literal patterns, guards, bindings)
- Closures/lambdas
- Try/catch/finally
- Throw/Return
- Type patterns and extractors

**What Needs to Be Added for Execution:**
- **Program entry point** - Find and execute `main` method or top-level code
- **Side effects** - Handle `println`, file I/O (redirected to browser console)
- **Return values** - Capture and return execution results
- **Object creation** - Create instances of classes (for stdlib types)
- **String operations** - Concatenation, interpolation
- **Collection operations** - `map`, `flatMap`, `filter`, etc.

---

## Technical Implementation

### 1. Execution Flow

#### Step 1: Compile to TASTy

```scala
// In browser compiler
class BrowserScalaCompiler {
  def compile(sourceCode: String): Array[Byte] = {
    val virtualSource = new VirtualFile("Main.scala", sourceCode.getBytes())
    val virtualOutput = new VirtualDirectory("output")

    val ctx = initialContext
      .setSetting(ctx.settings.sources, List(virtualSource))
      .setSetting(ctx.settings.outputDir, virtualOutput)
      .setSetting(ctx.settings.classpath, stdlibTastyDir)

    val compiler = new Compiler()
    val run = compiler.newRun(ctx)
    run.compileUnits(List(new CompilationUnit(virtualSource)))

    // Extract generated TASTy
    virtualOutput.iterator
      .find(_.name.endsWith(".tasty"))
      .map(_.toByteArray)
      .getOrElse(throw new Exception("No TASTy generated"))
  }
}
```

#### Step 2: Unpickle TASTy

```scala
def unpickleTasty(tastyBytes: Array[Byte]): Tree = {
  val virtualFile = new VirtualFile("Main.tasty", tastyBytes)
  val unpickler = new DottyUnpickler(virtualFile, tastyBytes)

  // Find the main class/module
  val roots = unpickler.readTopLevel()
  roots.find(_.name == "Main").map(_.tree).getOrElse(
    throw new Exception("No Main class found")
  )
}
```

#### Step 3: Execute Tree

```scala
class ExecutionEngine(interpreter: PureTastyInterpreter) {
  def execute(tree: Tree): ExecutionResult = {
    tree match {
      case PackageDef(_, stats) =>
        // Execute top-level statements
        stats.foreach(executeStatement)

      case ClassDef(name, _, _, body) if name == "Main" =>
        // Find main method or execute object body
        findMainMethod(body).map(executeMethod)
          .getOrElse(executeObjectBody(body))

      case _ =>
        throw new Exception("Unexpected top-level structure")
    }
  }

  private def findMainMethod(body: List[Tree]): Option[DefDef] = {
    body.collectFirst {
      case ddef: DefDef if ddef.name == "main" => ddef
    }
  }

  private def executeMethod(mainDef: DefDef): ExecutionResult = {
    val args = Array[String]() // Empty args for now
    interpreter.interpretMethodCall(null, mainDef.symbol, List(args))
  }
}
```

### 2. Browser Integration

```html
<!DOCTYPE html>
<html>
<head>
  <script type="module">
    import { BrowserScalaCompiler } from './browser-compiler.js';

    const compiler = new BrowserScalaCompiler();

    window.runScala = async (sourceCode) => {
      try {
        // 1. Compile to TASTy
        const tastyBytes = await compiler.compile(sourceCode);

        // 2. Execute TASTy
        const result = await compiler.execute(tastyBytes);

        return {
          success: true,
          output: result.output,
          value: result.returnValue
        };
      } catch (error) {
        return {
          success: false,
          error: error.message,
          errors: error.compilerErrors || []
        };
      }
    };
  </script>
</head>
<body>
  <textarea id="scala-code" rows="20" cols="80">
object Main {
  def main(args: Array[String]): Unit = {
    println("Hello from Scala!")
    val list = List(1, 2, 3)
    val doubled = list.map(_ * 2)
    println(s"Doubled: $doubled")
  }
}
  </textarea>
  <button onclick="run()">Run</button>
  <pre id="output"></pre>

  <script>
    async function run() {
      const code = document.getElementById('scala-code').value;
      const result = await window.runScala(code);

      const output = document.getElementById('output');
      if (result.success) {
        output.textContent = result.output || 'Execution completed';
        if (result.value !== undefined) {
          output.textContent += '\nReturn value: ' + JSON.stringify(result.value);
        }
      } else {
        output.textContent = 'Error: ' + result.error;
        if (result.errors) {
          output.textContent += '\n\nCompiler errors:\n' +
            result.errors.map(e => e.message).join('\n');
        }
      }
    }
  </script>
</body>
</html>
```

### 3. Execution Runtime

```scala
class ExecutionRuntime {
  private val outputBuffer = new StringBuilder()
  private var returnValue: Any = null

  def println(s: String): Unit = {
    outputBuffer.append(s).append("\n")
    // Also log to browser console
    js.Dynamic.global.console.log(s)
  }

  def getOutput: String = outputBuffer.toString()

  def setReturnValue(value: Any): Unit = {
    returnValue = value
  }

  def getReturnValue: Any = returnValue
}

// Intrinsics for browser execution
object BrowserIntrinsics {
  def register(interpreter: PureTastyInterpreter): Unit = {
    // Override println to use browser console
    interpreter.registerIntrinsic("scala.Console.println",
      (args: List[Any]) => {
        val s = args.head.asInstanceOf[String]
        js.Dynamic.global.console.log(s)
        ()
      }
    )

    interpreter.registerIntrinsic("scala.Predef.println",
      (args: List[Any]) => {
        val s = args.head.asInstanceOf[String]
        js.Dynamic.global.console.log(s)
        ()
      }
    )
  }
}
```

### 4. Virtual File System Setup

```scala
// In browser compiler initialization
class BrowserScalaCompiler {
  private val virtualStdlibDir = new VirtualDirectory("stdlib")
  private val virtualOutputDir = new VirtualDirectory("output")
  private val executionRuntime = new ExecutionRuntime()

  def initialize(stdlibTastyBundle: Map[String, Array[Byte]]): Unit = {
    // Load stdlib TASTy files into virtual filesystem
    for ((path, bytes) <- stdlibTastyBundle) {
      val file = new VirtualFile(path, bytes)
      virtualStdlibDir.addFile(file)
    }

    // Set up classpath
    val ctx = initialContext
      .setSetting(ctx.settings.classpath, virtualStdlibDir.path)
      .setSetting(ctx.settings.outputDir, virtualOutputDir)

    // Register browser intrinsics
    BrowserIntrinsics.register(interpreter)
  }

  def execute(tastyBytes: Array[Byte]): ExecutionResult = {
    // Unpickle TASTy
    val tree = unpickleTasty(tastyBytes)

    // Execute
    val engine = new ExecutionEngine(interpreter, executionRuntime)
    engine.execute(tree)

    ExecutionResult(
      output = executionRuntime.getOutput,
      returnValue = executionRuntime.getReturnValue
    )
  }
}
```

---

## Supported Scala Subset

### ✅ Fully Supported

**Language Features:**
- Variables: `val`, `var`, `lazy val`
- Control flow: `if`/`else`, `while`, `match` expressions
- Functions: method definitions, lambdas/closures
- Pattern matching: literal patterns, guards, bindings, type patterns
- Exception handling: `try`/`catch`/`finally`, `throw`
- Blocks and scoping
- Top-level definitions: `object`, `class` (for structure, not instantiation)

**Standard Library:**
- Primitives: `Int`, `Long`, `Double`, `Boolean`, `String`, `Char`
- Collections: `List`, `Option`, `Seq`, `Map`, `Set` (basic operations)
- Tuples: `Tuple2` through `Tuple5`
- Functions: `Function1` through `Function22`
- Console output: `println` (redirected to browser console)

**Execution Model:**
- `object Main { def main(args: Array[String]): Unit = ... }`
- Top-level statements (if supported by compiler)
- Expression evaluation with side effects

### ⚠️ Partially Supported

**Language Features:**
- String interpolation - needs implementation
- For-comprehensions - desugared, but needs `map`/`flatMap` support
- Class instantiation - only stdlib types initially

**Standard Library:**
- Advanced collection operations - some may need interpretation
- String operations - basic ones work, advanced need implementation
- File I/O - redirected to virtual filesystem or browser storage

### ❌ Not Supported (Initial Version)

**Language Features:**
- Macros - explicitly not supported
- User-defined class instantiation
- Trait mixins and inheritance
- Nested classes
- Implicit conversions (beyond what compiler handles)

**Standard Library:**
- `java.*` packages (except basic types)
- Advanced collections (`LazyList`, parallel collections)
- Reflection APIs
- File I/O (real filesystem access)

**Execution:**
- Multi-threaded execution
- Native method calls
- JVM-specific features

---

## Execution Model

### Entry Points

The execution engine supports multiple entry point styles:

#### Style 1: Traditional Main Method

```scala
object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, World!")
  }
}
```

**Execution:** Find `main` method, call with empty args array.

#### Style 2: Top-Level Code (Future)

```scala
// Top-level code (if compiler supports it)
println("Hello, World!")
val x = 42
println(s"x = $x")
```

**Execution:** Execute top-level statements in order.

#### Style 3: Expression Evaluation

```scala
object Main {
  def main(args: Array[String]): Unit = {
    val result = computeSomething()
    println(result)
  }

  def computeSomething(): Int = {
    1 + 2 + 3
  }
}
```

**Execution:** Execute `main`, capture return value if non-Unit.

### Side Effects

All side effects are captured and redirected:

| Side Effect | Browser Behavior |
|-------------|------------------|
| `println(s)` | Append to output buffer, log to `console.log` |
| `print(s)` | Append to output buffer (no newline) |
| Exceptions | Capture and include in execution result |
| Return values | Capture and return (if non-Unit) |

### Return Values

```scala
object Main {
  def main(args: Array[String]): Int = {
    val sum = List(1, 2, 3).sum
    println(s"Sum: $sum")
    sum  // Return value captured
  }
}
```

**Result:**
```json
{
  "success": true,
  "output": "Sum: 6\n",
  "returnValue": 6
}
```

---

## File Structure

```
browser-compiler-simple/
├── compiler-frontend.js          # Compiled compiler frontend (JS/WASM)
├── tasty-interpreter.js          # TASTy interpreter (JS/WASM)
├── execution-engine.js           # Execution runtime (NEW)
├── stdlib-tasty.js              # Bundled stdlib TASTy files
├── browser-compiler.js           # Main entry point
├── index.html                   # Demo page
└── build/
    ├── compiler/                 # Source: compiler frontend
    ├── interpreter/              # Source: TASTy interpreter
    ├── execution/                # Source: execution engine (NEW)
    └── stdlib-bundle/            # Scripts to generate stdlib bundle
```

---

## Build Steps

### 1. Prepare Compiler Frontend

```bash
# In Scala 3 repository
cd compiler
sbt "project scala3-compiler-bootstrapped-new"

# Modify Compiler.scala to exclude:
# - Macro phases (Inlining, Splicing, PickleQuotes)
# - Backend phases (GenBCode, GenSJSIR)
# Keep only: Parser → Typer → Pickler
```

### 2. Compile to JavaScript

```bash
# Use Scala.js to compile compiler frontend
sbt "project scala3-compiler-bootstrapped-new"
sbt "set scalaJSUseMainModuleInitializer := true"
sbt "scalaJS/fastOptJS"  # or fullOptJS for production
```

### 3. Generate Stdlib TASTy Bundle

```bash
# Same as full design
sbt "project scala3-library-bootstrapped-new"
sbt "compile"

# Extract and bundle TASTy files
find library/target/scala-library/classes -name "*.tasty" \
  -exec echo {} \; > tasty-list.txt

node scripts/bundle-tasty.js tasty-list.txt > stdlib-tasty.js
```

### 4. Bundle TASTy Interpreter

```bash
# Compile interpreter to JavaScript
cd tests/old-tasty-interpreter-prototype
sbt "scalaJS/fastOptJS"
cp target/scala-3.x/scalajs-bundler/main/browser-compiler-fastopt.js \
   ../../browser-compiler-simple/tasty-interpreter.js
```

### 5. Implement Execution Engine

```scala
// New file: execution/ExecutionEngine.scala
package scala.tasty.browser

class ExecutionEngine(interpreter: PureTastyInterpreter) {
  def execute(tree: Tree): ExecutionResult = {
    // Implementation as described above
  }
}
```

### 6. Create Main Bundle

```javascript
// browser-compiler.js (final bundle)
import { Compiler } from './compiler-frontend.js';
import { PureTastyInterpreter } from './tasty-interpreter.js';
import { ExecutionEngine } from './execution-engine.js';
import { stdlibTasty } from './stdlib-tasty.js';

class BrowserScalaCompiler {
  constructor() {
    this.compiler = new Compiler();
    this.interpreter = new PureTastyInterpreter();
    this.executionEngine = new ExecutionEngine(this.interpreter);
    this.tastyLoader = new TastyLoader(stdlibTasty);
  }

  async compile(sourceCode) {
    // 1. Parse and type-check
    // 2. Generate TASTy
    // 3. Return TASTy bytes
  }

  async execute(tastyBytes) {
    // 1. Unpickle TASTy
    // 2. Execute using execution engine
    // 3. Return results
  }

  async compileAndExecute(sourceCode) {
    const tastyBytes = await this.compile(sourceCode);
    return await this.execute(tastyBytes);
  }
}
```

---

## Limitations and Constraints

### 1. Bundle Size

**Estimated Total Size:**
- Compiler frontend: ~2-3 MB (compressed)
- Stdlib TASTy: ~1.5-2 MB (compressed)
- TASTy interpreter: ~500 KB (compressed)
- Execution engine: ~200 KB (compressed)
- **Total: ~4.2-5.7 MB (compressed)**

**Mitigation:**
- Use WebAssembly for better compression
- Tree-shaking to exclude unused compiler phases
- Lazy loading of stdlib TASTy files
- Compression (gzip/brotli)

### 2. Performance

**Expected Performance:**
- Compilation: ~100-500ms for simple code
- Execution: ~10-100x slower than native (acceptable for simple code)
- Memory: ~50-100 MB for compiler + stdlib + execution

**Optimization Opportunities:**
- Use WebAssembly for better performance
- Cache compiled TASTy files
- Optimize interpreter hot paths

### 3. Feature Limitations

**What Won't Work:**
- Macros (by design)
- External library dependencies
- User-defined classes (object model incomplete)
- Advanced stdlib features
- Real file I/O
- Multi-threading

**Workarounds:**
- Focus on simple Scala code initially
- Use only stdlib collections
- Document limitations clearly
- Provide clear error messages

### 4. Browser Compatibility

**Requirements:**
- ES2020+ support
- WebAssembly support (if using WASM)
- Modern browser (Chrome 90+, Firefox 88+, Safari 14+)

---

## Comparison: Simple vs Full Design

| Aspect | Simple Design | Full Design |
|--------|---------------|-------------|
| **Purpose** | Execute Scala code | Compile Scala code |
| **Macros** | Not supported | Supported |
| **Output** | Execution results | TASTy files |
| **Complexity** | Lower | Higher |
| **Use Case** | REPL, playground, education | Full compiler |
| **Bundle Size** | ~4.2-5.7 MB | ~4-5.5 MB |
| **Implementation** | Easier | More complex |

---

## Future Enhancements

### Phase 2: Enhanced Execution

- Top-level code execution
- Better error reporting with source positions
- Debugging support (step through execution)
- Performance profiling

### Phase 3: More Language Features

- User-defined classes
- Trait inheritance
- Advanced pattern matching
- String interpolation

### Phase 4: Interactive Features

- REPL mode (incremental execution)
- Variable inspection
- Breakpoints
- Execution visualization

### Phase 5: Advanced Stdlib

- Full collection API
- More stdlib modules
- Better performance for collections

---

## Testing Strategy

### Unit Tests

```scala
// Test execution engine
test("execute simple main method") {
  val code = """
    object Main {
      def main(args: Array[String]): Unit = {
        println("Hello")
      }
    }
  """
  val compiler = new BrowserScalaCompiler()
  val result = compiler.compileAndExecute(code)
  assert(result.success)
  assert(result.output.contains("Hello"))
}
```

### Integration Tests

```scala
// Test full execution pipeline
test("execute with collections") {
  val code = """
    object Main {
      def main(args: Array[String]): Unit = {
        val list = List(1, 2, 3)
        val doubled = list.map(_ * 2)
        println(doubled.mkString(", "))
      }
    }
  """
  val result = compiler.compileAndExecute(code)
  assert(result.success)
  assert(result.output.contains("2, 4, 6"))
}
```

### Browser Tests

```javascript
// Test in actual browser environment
describe('Browser Compiler Execution', () => {
  it('executes simple Scala code', async () => {
    const compiler = new BrowserScalaCompiler();
    const code = `
      object Main {
        def main(args: Array[String]): Unit = {
          println("Hello from browser!")
        }
      }
    `;
    const result = await compiler.compileAndExecute(code);
    expect(result.success).toBe(true);
    expect(result.output).toContain("Hello from browser!");
  });
});
```

---

## Success Criteria

### MVP (Minimum Viable Product)

✅ Compile simple Scala code using only stdlib
✅ Generate TASTy files in-memory
✅ Execute TASTy directly using tree interpreter
✅ Capture and return execution output
✅ No HTTP requests required
✅ Bundle size < 6 MB compressed
✅ Execution time < 2 seconds for simple code

### Production Ready

✅ Support all stdlib collections
✅ Complete string operations
✅ Error messages with source positions
✅ Return value capture
✅ Better performance (< 1 second execution)
✅ Bundle size < 5 MB compressed

---

## Example Use Cases

### 1. Educational Playground

```scala
// User writes code in browser
object Main {
  def main(args: Array[String]): Unit = {
    val numbers = List(1, 2, 3, 4, 5)
    val evens = numbers.filter(_ % 2 == 0)
    val doubled = evens.map(_ * 2)
    println(s"Result: $doubled")
  }
}

// Result:
// Output: "Result: List(4, 8)\n"
// Success: true
```

### 2. Algorithm Visualization

```scala
object Main {
  def main(args: Array[String]): Unit = {
    def factorial(n: Int): Int = {
      if (n <= 1) 1
      else n * factorial(n - 1)
    }

    val result = factorial(5)
    println(s"5! = $result")
  }
}

// Result:
// Output: "5! = 120\n"
// Success: true
```

### 3. Data Processing

```scala
object Main {
  def main(args: Array[String]): Unit = {
    val data = List("apple", "banana", "cherry")
    val lengths = data.map(_.length)
    val total = lengths.sum
    println(s"Total characters: $total")
  }
}

// Result:
// Output: "Total characters: 18\n"
// Success: true
```

---

## Related Documents

- `BROWSER_COMPILER_DESIGN.md` - Full design with macro support
- `CROSS_COMPILATION_FEASIBILITY.md` - Overall feasibility analysis
- `tests/old-tasty-interpreter-prototype/notes.md` - TASTy interpreter status
- `tests/old-tasty-interpreter-prototype/PRE_IMPLEMENTATION_ANALYSIS.md` - Pre-implementation analysis

---

## Open Questions

1. **Top-Level Code Support?**
   - Does Scala 3 compiler support top-level code?
   - If not, require `object Main { def main(...) }` format?
   - **Recommendation:** Start with `main` method, add top-level later if possible

2. **Return Value Handling?**
   - How to handle `main` methods that return non-Unit?
   - Should we support expression evaluation?
   - **Recommendation:** Support both Unit and non-Unit returns

3. **Error Reporting?**
   - How to display execution errors in browser?
   - Stack traces for interpreted code?
   - **Recommendation:** Return structured error objects with positions

4. **Performance Targets?**
   - What execution speed is acceptable?
   - Should we optimize interpreter or accept slower execution?
   - **Recommendation:** Accept slower execution initially, optimize hot paths later

5. **Stdlib Completeness?**
   - How much of stdlib needs to be executable?
   - Can we start with basic collections?
   - **Recommendation:** Start with core collections, expand incrementally

---

*Last updated: 2025-11-30*

