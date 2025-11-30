# Scala Browser Interpreter

A proof-of-concept TASTy-based interpreter that runs Scala code in the browser.

## Quick Start

### Run the Demo

Simply open `demo.html` in your web browser:

```bash
open demo.html
```

The demo includes several built-in examples:
- Hello World - Basic println
- Arithmetic - Math operations
- Fibonacci - Recursive functions
- Pattern Matching - Match expressions with guards
- List Operations - map, filter, foldLeft
- Option Handling - Some/None
- Try/Catch - Exception handling
- Higher-Order Functions - Functions as values
- Factorial - Recursion
- Closures - Captured variables

### Convert Real Scala Code

1. **Compile Scala to TASTy:**
   ```bash
   scalac -Yretain-trees examples/HelloWorld.scala -d out
   ```

2. **Convert TASTy to JSON:**
   ```bash
   sbt "browserInterpreterJvm/run out/HelloWorld.tasty"
   ```

3. **Paste the JSON output into the demo and run!**

## Project Structure

```
browser-interpreter/
├── demo.html              # Self-contained browser demo
├── build.sbt              # SBT build configuration
├── jvm/                   # JVM tools
│   └── src/main/scala/browser/
│       ├── TastyToJsonConverter.scala  # TASTy → JSON converter
│       └── AstSerializer.scala         # Tree serializer
├── js/                    # Scala.js browser module
│   └── src/main/scala/browser/
│       └── BrowserInterpreter.scala    # Scala.js interpreter
└── examples/              # Example Scala programs
    ├── HelloWorld.scala
    ├── Fibonacci.scala
    ├── PatternMatching.scala
    └── ListOperations.scala
```

## JSON AST Format

The interpreter uses a simple JSON AST format:

```json
// Literals
{"tag": "Literal", "type": "Int", "value": 42}
{"tag": "Literal", "type": "String", "value": "hello"}

// Variables
{"tag": "Ident", "name": "x"}

// Binary operations
{"tag": "BinaryOp", "op": "+", "lhs": {...}, "rhs": {...}}

// Blocks
{"tag": "Block", "stats": [...], "expr": {...}}

// Conditionals
{"tag": "If", "cond": {...}, "thenp": {...}, "elsep": {...}}

// Loops
{"tag": "While", "cond": {...}, "body": {...}}

// Function definitions
{"tag": "DefDef", "name": "add", "params": ["a", "b"], "body": {...}}

// Function calls
{"tag": "Apply", "fn": {...}, "args": [...]}

// Lambdas
{"tag": "Lambda", "params": ["x"], "body": {...}}

// Pattern matching
{"tag": "Match", "selector": {...}, "cases": [
  {"pattern": {...}, "guard": {...}, "body": {...}}
]}

// Exceptions
{"tag": "Try", "block": {...}, "catches": [...], "finalizer": {...}}
{"tag": "Throw", "expr": {...}}
```

## Supported Features

| Feature | Status |
|---------|--------|
| Literals (Int, String, Boolean, etc.) | ✅ |
| Variables (val, var) | ✅ |
| Arithmetic (+, -, *, /, %) | ✅ |
| Comparisons (<, >, <=, >=, ==, !=) | ✅ |
| Boolean operators (&&, \|\|, !) | ✅ |
| Conditionals (if/else) | ✅ |
| Loops (while) | ✅ |
| Blocks with local definitions | ✅ |
| Functions (def) | ✅ |
| Recursion | ✅ |
| Lambdas | ✅ |
| Closures with captured variables | ✅ |
| Pattern matching | ✅ |
| Guards in pattern matching | ✅ |
| Option (Some/None) | ✅ |
| List operations (map, filter, fold, etc.) | ✅ |
| String operations | ✅ |
| Tuple operations | ✅ |
| Try/Catch/Finally | ✅ |
| Throw | ✅ |
| For comprehensions | ⚠️ Partial |
| Classes | ❌ |
| Traits | ❌ |
| Imports | N/A (compile-time) |

## Building

### Prerequisites
- SBT 1.10+
- Scala 3.7.0+
- Node.js (for Scala.js testing)

### Build Commands

```bash
# Compile everything
sbt compile

# Build JVM tools
sbt browserInterpreterJvm/compile

# Build Scala.js module
sbt browserInterpreterJs/fastLinkJS

# Run TASTy converter
sbt "browserInterpreterJvm/run path/to/file.tasty"
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Compilation Pipeline                      │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  Scala Source → scalac → TASTy → TastyToJson → JSON AST     │
│                                                              │
├─────────────────────────────────────────────────────────────┤
│                    Browser Execution                         │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  JSON AST → BrowserInterpreter.interpret() → Output          │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## Roadmap

- [x] Basic interpreter with control flow
- [x] Pattern matching support
- [x] Exception handling
- [x] Collection operations
- [x] TASTy-to-JSON converter
- [ ] Cross-compile full interpreter to Scala.js
- [ ] Bundle stdlib TASTy for type-checking
- [ ] Full browser-based compilation

## License

Apache 2.0 (same as Scala 3)

