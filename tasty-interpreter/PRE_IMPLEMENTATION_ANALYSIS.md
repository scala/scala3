# Pre-Implementation Analysis for TASTy Interpreter

This document provides the results of pre-implementation data gathering and analysis.

**Date:** 2025-11-30
**Status:** Analysis complete. See `notes.md` for current implementation status.

---

## 1. Standard Library Usage in Macros

### 1.1 Analysis Results

Analyzed stdlib usage patterns in `tests/run-macros/` and `tests/pos-macros/`.

| Class | Criticality | Notes |
|-------|-------------|-------|
| `List` | **Critical** | Found in majority of macros |
| `Option` | **Critical** | High usage |
| `String` | **Critical** | Very high usage |
| `Seq` | High | Medium-High usage |
| `Map`, `Set` | Medium | Moderate usage |

### 1.2 Most Used Methods

| Method | Notes |
|--------|-------|
| `.map`, `.flatMap`, `.filter` | Primary transformations |
| `.foreach` | Side effects |
| `.mkString`, `.fold`/`.foldLeft` | String building, aggregation |
| `.head`, `.tail`, `.isEmpty` | List decomposition |

### 1.3 Key Insight

Macros use a **LIMITED subset** of stdlib:
- **Tree manipulation** is the primary activity (not general computation)
- Collection operations are mainly for processing AST children
- String operations for error messages and code generation

This means **interpreting stdlib from TASTy is viable** — the hot path is tree manipulation, not collection operations.

---

## 2. Integration Design

> **Decision:** Development is **isolated** in `tasty-interpreter/`. Integration uses the Adapter pattern when ready.

### 2.1 JVM Dependencies to Replace

| Production Method | JVM API | Replacement |
|-------------------|---------|-------------|
| `loadClass` | `ClassLoader.loadClass` | Load from TASTy |
| `loadModule` | `Class.getField(MODULE$)` | Interpret singleton |
| `interpretedStaticMethodCall` | `Method.invoke` | Tree interpretation |
| `interpretNew` | `Constructor.newInstance` | Tree interpretation |

### 2.2 Integration Strategy

**Adapter pattern with gradual migration:**

```
MacroInterpreterBackend (trait)
    ├── JVMReflectionBackend    (existing, for fallback)
    ├── TreeInterpretationBackend (new, pure TASTy)
    └── HybridBackend           (transition period)
```

The `HybridBackend` tries TASTy interpretation first, falls back to JVM for deps without TASTy.

---

## 3. Validation Targets

### 3.1 MVP Macro Tests

| Phase | Tests | Features |
|-------|-------|----------|
| **1** | `xml-interpolation-1`, `tasty-definitions-1` | Basic quotes, symbol queries |
| **2** | `inline-tuples-1`, `flops-rewrite` | Collections, transformations |
| **3** | `annot-mod-class-data`, `newClassExtends` | Annotations, dynamic classes |

### 3.2 Success Criteria

1. ✅ Macro compiles without JVM reflection calls
2. ✅ Test output matches expected `.check` file
3. ✅ No runtime exceptions during macro expansion
4. ✅ Generated code type-checks correctly

---

## 4. Summary

### Pre-Implementation Analysis Complete ✅

| Action | Status | Key Findings |
|--------|--------|--------------|
| **Data gathering** | ✅ | `List`, `Option`, `String` critical; limited stdlib subset needed |
| **Design document** | ✅ | Map-based object representation; `this` via env binding |
| **Prototype spike** | ✅ | All features implemented and tested |
| **Integration design** | ✅ | Adapter pattern for gradual migration |
| **Test plan** | ✅ | 10 representative tests identified |

### Decisions Made

1. **Stdlib Strategy**: Hybrid — interpret from TASTy with intrinsics for truly native operations
2. **Migration Path**: Isolated development in `tasty-interpreter/`
3. **MVP Scope**: Phase 1-2 tests prioritized

---

## 5. Current Status

**See `notes.md` for:**
- Implementation progress log
- Current test results (11/11 passing)
- Intrinsics vs TASTy interpretation architecture
- Detailed task lists

**Run tests:**
```bash
sbt 'scala3-tasty-interpreter-new/test:run'
```

---

*Last updated: 2025-11-30*
