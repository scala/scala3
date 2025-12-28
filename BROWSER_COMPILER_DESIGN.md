# Browser-Based Scala Compiler Design Document
## Self-Contained, No HTTP Loading Required

**Date:** 2025-11-30
**Status:** Design Document
**Target:** Simple Scala code execution in browser without external dependencies

---

## Executive Summary

This document outlines the design for a browser-based Scala compiler that bundles all necessary components (compiler, standard library TASTy files, TASTy interpreter) to enable compilation and execution of simple Scala code directly in the browser without requiring HTTP requests or external dependencies.

**Key Characteristics:**
- ✅ **Self-contained** - All dependencies bundled at build time
- ✅ **No HTTP loading** - Everything loaded from memory/embedded resources
- ✅ **Simple Scala subset** - Supports code using only standard library
- ✅ **TASTy-based execution** - Uses TASTy interpreter for macro execution

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
[If macros present]
    ↓
TASTy Interpreter (PureTastyInterpreter)
    ↓
Macro Expansion (tree interpretation)
    ↓
Final TASTy Output (in-memory)
```

### Component Breakdown

| Component | Purpose | Location | Status |
|-----------|---------|----------|--------|
| **Compiler Frontend** | Parse, type-check, generate TASTy | `compiler/src/dotty/tools/dotc/` | ✅ Exists |
| **TASTy Generator** | Serialize trees to TASTy format | `compiler/src/dotty/tools/dotc/core/tasty/` | ✅ Exists |
| **VirtualFile System** | In-memory file abstraction | `compiler/src/dotty/tools/io/VirtualFile.scala` | ✅ Exists |
| **TASTy Interpreter** | Execute macros from TASTy | `tests/old-tasty-interpreter-prototype/` | ⚠️ ~40% complete |
| **Stdlib TASTy Bundle** | Pre-compiled standard library | To be generated | 📝 To do |
| **Browser Build Target** | Compile compiler to JS/WASM | Build configuration | 📝 To do |

---

## What Needs to Be Bundled

### 1. Scala 3 Compiler (Frontend Only)

**Required Components:**
- Parser (`compiler/src/dotty/tools/dotc/parsing/`)
- Typer (`compiler/src/dotty/tools/dotc/typer/`)
- Pickler (`compiler/src/dotty/tools/dotc/transform/Pickler.scala`)
- TASTy format (`compiler/src/dotty/tools/dotc/core/tasty/`)
- VirtualFile system (`compiler/src/dotty/tools/io/VirtualFile.scala`)

**Not Required:**
- JVM backend (`backend/jvm/`) - We're not generating bytecode
- Scala.js backend (`backend/sjs/`) - We're not generating JS IR
- ASM library - Not needed for TASTy generation
- ClassLoader infrastructure - Using TASTy-based loading

**Estimated Size:** ~2-3 MB (compressed) for frontend phases only

### 2. Standard Library TASTy Files

**What to Bundle:**

The standard library consists of multiple modules. For simple Scala code, we need:

| Module | Purpose | Criticality | Estimated Size |
|--------|---------|-------------|----------------|
| `scala-library` | Core types (`String`, `Int`, `Boolean`, etc.) | **Critical** | ~500 KB |
| `scala-library` (collections) | `List`, `Option`, `Seq`, `Map`, `Set` | **Critical** | ~800 KB |
| `scala-library` (other) | `Tuple`, `Function`, `Product`, etc. | **High** | ~300 KB |

**Total Estimated Size:** ~1.5-2 MB (compressed)

**What These Contain:**
- Type signatures for type checking
- Method signatures for overload resolution
- Method bodies for TASTy interpreter execution
- All necessary to compile and execute simple Scala code

### 3. TASTy Interpreter

**Required Components:**
- `PureTastyInterpreter.scala` - Core interpreter
- `TastyLoader.scala` - TASTy definition loader
- Intrinsics system - Platform bridges for stdlib types

**Current Status:** ~40% complete (see `tests/old-tasty-interpreter-prototype/notes.md`)

**What's Working:**
- Control flow (if/else, while, blocks)
- Match expressions (literal patterns, guards, bindings)
- Closures/lambdas
- Try/catch/finally
- Throw/Return
- Type patterns and extractors

**What's Missing (but not critical for simple code):**
- String operations (concatenation, interpolation)
- Full object model (class instantiation, constructors)
- For-comprehensions (desugared, so lower priority)

### 4. Intrinsics System

For stdlib types that cannot be interpreted from TASTy (JVM primitives), we need intrinsics:

**Already Implemented:**
- `scala.Console.println` → delegates to browser console
- `scala.Predef.println` → delegates to browser console
- `scala.math.max/min/abs` → JavaScript Math operations
- `Some/None` extractors → Case class handling
- Exception types → JavaScript Error objects

**To Add:**
- String operations (`+`, `length`, `substring`, etc.)
- Collection operations (`map`, `flatMap`, `filter`, etc.) - or interpret from TASTy
- Array operations

---

## Technical Implementation

### 1. Build Process

#### Step 1: Compile Compiler to JavaScript/WebAssembly

```scala
// Build configuration (conceptual)
lazy val compilerJS = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.x.x",
    scalaJSUseMainModuleInitializer := true,
    // Exclude JVM-specific code
    libraryDependencies := libraryDependencies.value.filterNot(_.name == "asm"),
    // Include only frontend phases
    scalacOptions ++= Seq(
      "-Ybackend:JS",  // Compile compiler itself to JS
      "-Yexclude-backend:jvm"  // Exclude JVM backend code
    )
  )
```

**Challenges:**
- File I/O abstraction - Replace `java.nio.file.*` with `AbstractFile` API
- Float/Double bit manipulation - Use JavaScript `Float32Array`/`Float64Array`
- Concurrency - Use JavaScript `Promise`/`async-await` instead of threads

#### Step 2: Generate Stdlib TASTy Bundle

```bash
# Compile stdlib with TASTy output
sbt "scala3-library/compile"

# Extract TASTy files
find library/target/scala-library/classes -name "*.tasty" \
  | tar -czf stdlib-tasty.tar.gz -T -

# Or bundle as JavaScript module
# Convert TASTy files to base64-encoded strings in a JS module
```

**Bundle Format Options:**

**Option A: JavaScript Module (Recommended)**
```javascript
// stdlib-tasty.js
export const stdlibTasty = {
  "scala/String.tasty": new Uint8Array([/* base64 decoded bytes */]),
  "scala/collection/immutable/List.tasty": new Uint8Array([/* ... */]),
  // ... all stdlib TASTy files
};
```

**Option B: WebAssembly Memory**
```javascript
// Load TASTy files into WASM linear memory
// More efficient but more complex
```

**Option C: Embedded in Compiler Bundle**
```scala
// Embed as resources in compiler JS bundle
// Access via `getClass.getResourceAsStream`
```

#### Step 3: Bundle Everything

```javascript
// browser-compiler.js (final bundle)
import { Compiler } from './compiler-frontend.js';
import { PureTastyInterpreter } from './tasty-interpreter.js';
import { stdlibTasty } from './stdlib-tasty.js';

class BrowserScalaCompiler {
  constructor() {
    this.compiler = new Compiler();
    this.interpreter = new PureTastyInterpreter();
    this.tastyLoader = new TastyLoader(stdlibTasty);
  }

  compile(sourceCode) {
    // 1. Parse and type-check
    // 2. Generate TASTy
    // 3. Execute macros if present
    // 4. Return TASTy or errors
  }
}
```

### 2. Virtual File System Setup

```scala
// In browser compiler initialization
val virtualOutputDir = new VirtualDirectory("output")
val virtualStdlibDir = new VirtualDirectory("stdlib")

// Load stdlib TASTy files into virtual filesystem
for ((path, bytes) <- stdlibTastyBundle) {
  val file = new VirtualFile(path, bytes)
  virtualStdlibDir.addFile(file)
}

// Set up classpath
ctx.setSetting(ctx.settings.classpath, virtualStdlibDir.path)
ctx.setSetting(ctx.settings.outputDir, virtualOutputDir)
```

### 3. TASTy Loading from Bundle

```scala
class BundledTastyLoader(tastyBundle: Map[String, Array[Byte]])
    extends TastyLoader {

  override def loadClass(fullName: String): Option[ClassDef] = {
    val tastyPath = fullName.replace('.', '/') + ".tasty"
    tastyBundle.get(tastyPath).flatMap { bytes =>
      val virtualFile = new VirtualFile(tastyPath, bytes)
      val unpickler = new DottyUnpickler(virtualFile, bytes)
      // ... unpickle and return ClassDef
    }
  }
}
```

### 4. Browser Integration

```html
<!DOCTYPE html>
<html>
<head>
  <script type="module">
    import { BrowserScalaCompiler } from './browser-compiler.js';

    const compiler = new BrowserScalaCompiler();

    window.compileScala = async (sourceCode) => {
      try {
        const result = await compiler.compile(sourceCode);
        return { success: true, tasty: result };
      } catch (error) {
        return { success: false, error: error.message };
      }
    };
  </script>
</head>
<body>
  <textarea id="scala-code"></textarea>
  <button onclick="run()">Compile</button>

  <script>
    async function run() {
      const code = document.getElementById('scala-code').value;
      const result = await window.compileScala(code);
      console.log(result);
    }
  </script>
</body>
</html>
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

**Standard Library:**
- Primitives: `Int`, `Long`, `Double`, `Boolean`, `String`, `Char`
- Collections: `List`, `Option`, `Seq`, `Map`, `Set` (basic operations)
- Tuples: `Tuple2` through `Tuple5`
- Functions: `Function1` through `Function22`

**Macros:**
- Simple inline macros (using TASTy interpreter)
- Macros that use stdlib collections
- Macros with pattern matching

### ⚠️ Partially Supported

**Language Features:**
- String interpolation - needs implementation
- For-comprehensions - desugared, but needs `map`/`flatMap` support
- By-name parameters - needs thunk implementation

**Standard Library:**
- Advanced collection operations - some may need interpretation
- String operations - basic ones work, advanced need implementation

### ❌ Not Supported (Initial Version)

**Language Features:**
- Class instantiation (user-defined classes)
- Trait mixins and inheritance
- Nested classes
- Implicit conversions (beyond what compiler handles)

**Standard Library:**
- `java.*` packages (except basic types)
- Advanced collections (`LazyList`, parallel collections)
- Reflection APIs

**Macros:**
- Macros that require external libraries
- Macros that instantiate user-defined classes
- Complex macro libraries (circe, shapeless, etc.)

---

## File Structure

```
browser-compiler/
├── compiler-frontend.js          # Compiled compiler frontend (JS/WASM)
├── tasty-interpreter.js          # TASTy interpreter (JS/WASM)
├── stdlib-tasty.js              # Bundled stdlib TASTy files
├── browser-compiler.js           # Main entry point
├── index.html                   # Demo page
└── build/
    ├── compiler/                 # Source: compiler frontend
    ├── interpreter/              # Source: TASTy interpreter
    └── stdlib-bundle/            # Scripts to generate stdlib bundle
```

---

## Build Steps

### 1. Prepare Compiler Frontend

```bash
# In Scala 3 repository
cd compiler
sbt "project scala3-compiler-bootstrapped-new"
sbt "compile"

# Exclude backend phases, keep only frontend
# Modify Compiler.scala to exclude backendPhases
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
# Compile stdlib with TASTy output
sbt "project scala3-library-bootstrapped-new"
sbt "compile"

# Extract TASTy files
find library/target/scala-library/classes -name "*.tasty" \
  -exec echo {} \; > tasty-list.txt

# Bundle as JavaScript module
node scripts/bundle-tasty.js tasty-list.txt > stdlib-tasty.js
```

### 4. Bundle TASTy Interpreter

```bash
# Compile interpreter to JavaScript
cd tests/old-tasty-interpreter-prototype
sbt "scalaJS/fastOptJS"
cp target/scala-3.x/scalajs-bundler/main/browser-compiler-fastopt.js \
   ../../browser-compiler/tasty-interpreter.js
```

### 5. Create Main Bundle

```javascript
// browser-compiler.js
import { Compiler } from './compiler-frontend.js';
import { PureTastyInterpreter } from './tasty-interpreter.js';
import { stdlibTasty } from './stdlib-tasty.js';

// ... implementation
```

---

## Limitations and Constraints

### 1. Bundle Size

**Estimated Total Size:**
- Compiler frontend: ~2-3 MB (compressed)
- Stdlib TASTy: ~1.5-2 MB (compressed)
- TASTy interpreter: ~500 KB (compressed)
- **Total: ~4-5.5 MB (compressed)**

**Mitigation:**
- Use WebAssembly for better compression
- Tree-shaking to exclude unused compiler phases
- Lazy loading of stdlib TASTy files (load on demand)
- Compression (gzip/brotli)

### 2. Performance

**Expected Performance:**
- Compilation: ~100-500ms for simple code (vs ~50ms on JVM)
- Macro execution: ~10-50x slower than JVM (acceptable for macros)
- Memory: ~50-100 MB for compiler + stdlib

**Optimization Opportunities:**
- Use WebAssembly for better performance
- Cache compiled TASTy files
- Incremental compilation (reuse previous results)

### 3. Feature Limitations

**What Won't Work:**
- External library dependencies (no HTTP loading)
- User-defined classes (object model incomplete)
- Advanced stdlib features (some collections, reflection)
- Complex macros (shapeless, circe, etc.)

**Workarounds:**
- Focus on simple Scala code initially
- Gradually expand supported features
- Document limitations clearly

### 4. Browser Compatibility

**Requirements:**
- ES2020+ support (for async/await, optional chaining)
- WebAssembly support (if using WASM)
- Modern browser (Chrome 90+, Firefox 88+, Safari 14+)

---

## Future Enhancements

### Phase 2: Enhanced Stdlib Support

- Complete string operations
- Full collection API support
- More stdlib modules

### Phase 3: External Library Support

- HTTP-based TASTy loading
- CDN for common libraries
- Dependency resolution

### Phase 4: Full Language Support

- User-defined classes
- Trait inheritance
- Advanced language features

### Phase 5: Execution Engine

- Execute compiled TASTy in browser
- REPL functionality
- Interactive playground

---

## Testing Strategy

### Unit Tests

```scala
// Test compiler frontend in isolation
test("parse simple code") {
  val compiler = new BrowserScalaCompiler()
  val result = compiler.compile("val x = 42")
  assert(result.success)
}
```

### Integration Tests

```scala
// Test full compilation pipeline
test("compile with stdlib") {
  val code = """
    val list = List(1, 2, 3)
    val doubled = list.map(_ * 2)
    println(doubled.mkString(", "))
  """
  val result = compiler.compile(code)
  assert(result.success)
  assert(result.tasty.nonEmpty)
}
```

### Browser Tests

```javascript
// Test in actual browser environment
describe('Browser Compiler', () => {
  it('compiles simple Scala code', async () => {
    const compiler = new BrowserScalaCompiler();
    const result = await compiler.compile('val x = 42');
    expect(result.success).toBe(true);
  });
});
```

---

## Success Criteria

### MVP (Minimum Viable Product)

✅ Compile simple Scala code using only stdlib
✅ Generate TASTy files in-memory
✅ Execute simple macros using TASTy interpreter
✅ No HTTP requests required
✅ Bundle size < 6 MB compressed
✅ Compilation time < 1 second for simple code

### Production Ready

✅ Support all stdlib collections
✅ Complete string operations
✅ Error messages with source positions
✅ Incremental compilation
✅ Bundle size < 4 MB compressed
✅ Compilation time < 500ms for simple code

---

## Related Documents

- `CROSS_COMPILATION_FEASIBILITY.md` - Overall feasibility analysis
- `tests/old-tasty-interpreter-prototype/notes.md` - TASTy interpreter status
- `tests/old-tasty-interpreter-prototype/PRE_IMPLEMENTATION_ANALYSIS.md` - Pre-implementation analysis

---

## Open Questions

1. **WebAssembly vs JavaScript?**
   - WASM: Better performance, smaller size, but more complex toolchain
   - JS: Easier development, better debugging, but slower
   - **Recommendation:** Start with JS, migrate to WASM if needed

2. **Stdlib Bundle Strategy?**
   - Bundle all stdlib TASTy files upfront?
   - Lazy load on demand?
   - **Recommendation:** Bundle core stdlib upfront, lazy load advanced modules

3. **Error Reporting?**
   - How to display compiler errors in browser?
   - Source map support?
   - **Recommendation:** Return structured error objects, let UI handle display

4. **TASTy Interpreter Completeness?**
   - How much of the interpreter needs to be complete?
   - Can we start with basic macros only?
   - **Recommendation:** Support basic macros first, expand incrementally

---

*Last updated: 2025-01-27*

