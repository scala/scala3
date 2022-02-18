---
layout: doc-page
title: "Scala 3 Reference"
titlegraphic: ../out/pandoc/images/scala-spiral.png
author: Martin Odersky et al.
institute: EPFL, Switzerland
abstract-title: WILL_BE_IGNORED
abstract: |
  Latest HTML version available at
  [`https://docs.scala-lang.org/scala3/reference/`](https://docs.scala-lang.org/scala3/reference/overview.html)
---

\newpage
# Overview [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/overview.html) {#overview}
```{.include}
../out/pandoc/src_managed/reference/overview.md
```

## Soft Keywords [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/soft-modifier.html) {#soft-modifier}
```{.include}
../out/pandoc/src_managed/reference/soft-modifier.md
```

[new-types]: #################################################################

\newpage
# New Types [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/new-types.html) {#new-types}

## Intersection types [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/new-types/intersection-types.html) {#intersection-types}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/new-types/intersection-types.md
```

## Union types [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/new-types/union-types.html) {#union-types}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/new-types/union-types.md
```

## Type lambdas [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/new-types/type-lambdas.html) {#type-lambdas}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/new-types/type-lambdas.md
```

## Match Types [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/new-types/match-types.html) {#match-types}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/new-types/match-types.md
```

## Dependent Function Types [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/new-types/dependent-function-types.html) {#dependent-function-types}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/new-types/dependent-function-types.md
```

## Polymorphic Function Types [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/new-types/polymorphic-function-types.html) {#polymorphic-function-types}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/new-types/polymorphic-function-types.md
```

[enums]: #####################################################################

\newpage
# Enums [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/enums.html) {#enums-2}

## Enumerations [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/enums/enums.html) {#enums}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/enums/enums.md
```

## Algebraic Data Types [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/enums/adts.html) {#adts}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/enums/adts.md
```

## Translation of Enums and ADTs [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/enums/desugarEnums.html) {#desugarEnums}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/enums/desugarEnums.md
```

[contextual-abstractions]: ###################################################

\newpage
# Contextual Abstractions [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/contextual.html) {#motivation}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/contextual.md
```

## Given Instances [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/givens.html) {#givens}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/givens.md
```

## Using Clauses [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/using-clauses.html) {#using-clauses}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/using-clauses.md
```

## Context Bounds [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/context-bounds.html) {#context-bounds}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/context-bounds.md
```

## Importing Givens [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/given-imports.html) {#given-imports}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/given-imports.md
```

## Extension Methods [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/extension-methods.html) {#extension-methods}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/extension-methods.md
```

## Implementing Type classes [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/type-classes.html) {#type-classes}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/type-classes.md
```

## Type Class Derivation [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/derivation.html) {#derivation}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/derivation.md
```

## Multiversal Equality [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/multiversal-equality.html) {#multiversal-equality}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/multiversal-equality.md
```

## Context Functions [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/context-functions.html) {#context-functions}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/context-functions.md
```

## Implicit Conversions [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/conversions.html) {#conversions}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/conversions.md
```

## By-Name Context Parameters [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/by-name-context-parameters.html) {#by-name-context-parameters}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/by-name-context-parameters.md
```

## Relationship with Scala 2 Implicits [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/relationship-implicits.html) {#relationship-implicits}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/relationship-implicits.md
```

## How to write a type class `derived` method using macros {#derivation-macro}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/derivation-macro.md
``` 

[metaprogramming]: ###########################################################

\newpage
# Metaprogramming [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/metaprogramming.html) {#metaprogramming}

```{.include}
../out/pandoc/src_managed/reference/metaprogramming/metaprogramming.md
```

## Inline [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/metaprogramming/inline.html) {#inline}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/metaprogramming/inline.md
```

## Macros [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/metaprogramming/macros.html) {#macros}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/metaprogramming/macros.md
```

## Multi-Stage Programming [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/metaprogramming/staging.html) {#staging}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/metaprogramming/staging.md
```

## Reflection [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/metaprogramming/reflection.html) {#reflection}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/metaprogramming/reflection.md
```

## TASTy Inspection [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/metaprogramming/tasty-inspect.html) {#tasty-inspect}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/metaprogramming/tasty-inspect.md
```

## The Meta-theory of Symmetric Metaprogramming [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/metaprogramming/simple-smp.html) {#simple-smp}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/metaprogramming/simple-smp.md
```

[other-new-features]: ########################################################

\newpage
# Other New Features [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features.html) {#other-new-features}

```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/other-new-types.md
```

## Trait Parameters [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/trait-parameters.html) {#trait-parameters}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/trait-parameters.md
```

## Transparent Traits [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/transparent-traits.html) {#transparent-traits}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/transparent-traits.md
```

## Universal Apply Methods [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/creator-applications.html) {#creator-applications}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/creator-applications.md
```

## Export clauses [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/export.html) {#export}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/export.md
```

## Opaque Type Aliases [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/opaques.html) {#opaques}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/opaques.md
```

## Open Classes [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/open-classes.html) {#open-classes}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/open-classes.md
```

## Parameter Untupling [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/parameter-untupling.html) {#parameter-untupling}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/parameter-untupling.md
```

## Kind Polymorphism [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/kind-polymorphism.html) {#kind-polymorphism}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/kind-polymorphism.md
```

## The Matchable Trait [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/matchable.html) {#matchable}
```{.include}
../out/pandoc/src_managed/reference/other-new-features/matchable.md
```

## `@threadUnsafe` Annotation [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/threadUnsafe-annotation.html) {#threadUnsafe-annotation}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/threadUnsafe-annotation.md
```

## `@targetName` Annotations [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/targetName.html) {#targetName}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/targetName.md
```

## New Control Syntax [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/control-syntax.html) {#control-syntax}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/control-syntax.md
```

## Optional Braces [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/indentation.html) {#indentation}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/indentation.md
```

## Fewer Braces [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/indentation-experimental.html) {#indentation-experimental}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/indentation-experimental.md
```

## Safe Initialization [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/safe-initialization.html) {#safe-initialization}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/safe-initialization.md
```

## TypeTest [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/type-test.html) {#type-test}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/type-test.md
```

## Experimental Definitions [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/experimental-defs.html) {#type-test}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/experimental-defs.md
```

[changed-features]: ##########################################################

\newpage
# Other Changed Features [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features.html) {#other-changed-features}

## Numeric Literals [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/numeric-literals.html) {#numeric-literals}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/numeric-literals.md
```

## Programmatic Structural Types [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/structural-types.html) {#structural-types}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/structural-types.md
```

## Rules for Operators [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/operators.html) {#operators}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/operators.md
```

## Wildcard Arguments in Types [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/wildcards.html) {#wildcards}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/wildcards.md
```

## Imports [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/imports.html) {#wildcards}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/imports.md
```

## Changes in Type Checking [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/type-checking.html) {#type-checking}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/type-checking.md
```

## Changes in Type Inference [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/type-inference.html) {#type-inference}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/type-inference.md
```

## Changes in Implicit Resolution [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/implicit-resolution.html) {#implicit-resolution}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/implicit-resolution.md
```

## Implicit Conversions [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/implicit-conversions.html) {#implicit-conversions}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/implicit-conversions.md
```

## Changes in Overload Resolution [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/overload-resolution.html) {#overload-resolution}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/overload-resolution.md
```

## Match Expressions [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/match-syntax.html) {#match-syntax}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/match-syntax.md
```

## Vararg Splices [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/vararg-splices.html) {#vararg-splices}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/vararg-splices.md
```

## Pattern Bindings [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/pattern-bindings.html) {#pattern-bindings}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/pattern-bindings.md
```

## Option-less pattern matching [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/pattern-matching.html) {#pattern-matching}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/pattern-matching.md
```

## Automatic Eta Expansion [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/eta-expansion.html) {#eta-expansion}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/eta-expansion.md
```

## Changes in Compiler Plugins [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/compiler-plugins.html) {#compiler-plugins}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/compiler-plugins.md
```

## Lazy Vals Initialization [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/lazy-vals-init.html) {#lazy-vals-init}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/lazy-vals-init.md
```

## Main Methods [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/main-functions.html) {#main-functions}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/main-functions.md
```

## Escapes in interpolations [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/interpolation-escapes.html) {#interpolation-escapes}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/interpolation-escapes.md
```

[dropped-features]: ##########################################################

\newpage
# Dropped Features [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features.html) {#dropped-features}

## Dropped: DelayedInit [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/delayed-init.html) {#delayed-init}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/delayed-init.md
```

## Dropped: Scala 2 Macros [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/macros.html) {#macros2}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/macros.md
```

## Dropped: Existential types [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/existential-types.html) {#existential-types}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/existential-types.md
```

## Dropped: General Type Projection [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/type-projection.html) {#type-projection}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/type-projection.md
```

## Dropped: Do-While [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/do-while.html) {#do-while}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/do-while.md
```

## Dropped: Procedure Syntax [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/procedure-syntax.html) {#procedure-syntax}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/procedure-syntax.md
```

## Dropped: Package Objects [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/package-objects.html) {#package-objects}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/package-objects.md
```

## Dropped: Early Initializers [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/early-initializers.html) {#early-initializers}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/early-initializers.md
```

## Dropped: Class shadowing [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/class-shadowing.html) {#class-shadowing}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/class-shadowing.md
```

## Dropped: Limit 22 [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/limit22.html) {#limit22}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/limit22.md
```

## Dropped: XML literals [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/xml.html) {#xml}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/xml.md
```

## Dropped: Symbol literals [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/symlits.html) {#symlits}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/symlits.md
```

## Dropped: Auto-Application [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/auto-apply.html) {#auto-apply}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/auto-apply.md
```

## Dropped: Weak conformance [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/weak-conformance.html) {#weak-conformance}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/weak-conformance.md
```

## Deprecated: Nonlocal Returns [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/nonlocal-returns.html) {#nonlocal-returns}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/nonlocal-returns.md
```

## Dropped: `private[this]` and `protected[this]` [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/this-qualifier.html) {#this-qualifier}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/this-qualifier.md
```

## Dropped: Wildcard Initializer [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/wildcard-init.html) {#wildcard-init}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/wildcard-init.md
```

[experimental]: ##############################################################

\newpage
# Experimental Features [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/experimental/overview.html) {#experimental-features}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/experimental/overview.md
```

## CanThrow Capabilities [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/experimental/canthrow.html) {#canthrow}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/experimental/canthrow.md
```

## Capture Checking [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/experimental/cc.html) {#capture-checking}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/experimental/cc.md
```

## Erased Definitions [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/experimental/erased-defs.html) {#erased-defs}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/experimental/erased-defs.md
```

## Explicit Nulls [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/experimental/explicit-nulls.html) {#explicit-nulls}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/experimental/explicit-nulls.md
```

## Named Type Arguments [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/experimental/named-typeargs.html) {#named-typeargs}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/experimental/named-typeargs.md
```

## Named Numeric Literals [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/experimental/numeric-literals.html) {#numeric-literals}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/experimental/numeric-literals.md
```

[syntax]: ####################################################################

\newpage
# Scala 3 Syntax Summary [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/syntax.html) {#syntax}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/syntax.md
```

[language-versions]: #########################################################

\newpage
# Language Versions [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/language-versions.html) {#language-versions}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/language-versions/language-versions.md
```

## Source Compatibility [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/language-versions/source-compatibility.html) {#source-compatibility}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/language-versions/source-compatibility.md
```

## Binary Compatibility [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/language-versions/binary-compatibility.html) {#binary-compatibility}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/language-versions/binary-compatibility.md
```

[appendix]: ##################################################################

\newpage
# Appendix {#appendix}

## Context Functions - More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/contextual/context-functions-spec.html) {#context-functions-spec}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/contextual/context-functions-spec.md
```

## Opaque Type Aliases: More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/opaques-details.html) {#opaques-details}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/opaques-details.md
```

## Parameter Untupling - More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/other-new-features/parameter-untupling-spec.html) {#parameter-untupling-spec}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/other-new-features/parameter-untupling-spec.md
```

## Dropped: Class Shadowing - More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/class-shadowing-spec.html) {#class-shadowing-spec}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/class-shadowing-spec.md
```

## Dropped: Weak Conformance - More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/dropped-features/weak-conformance-spec.html) {#weak-conformance-spec}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/dropped-features/weak-conformance-spec.md
```

## Automatic Eta Expansion - More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/eta-expansion-spec.html) {#eta-expansion-spec}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/eta-expansion-spec.md
```

## Implicit Conversions - More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/implicit-conversions-spec.html) {#implicit-conversions-spec}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/implicit-conversions-spec.md
```

## Programmatic Structural Types - More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/changed-features/structural-types-spec.html) {#structural-types-spec}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/changed-features/structural-types-spec.md
```

## Dependent Function Types - More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/new-types/dependent-function-types-spec.html) {#dependent-function-types-spec}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/new-types/dependent-function-types-spec.md
```

## Intersection Types - More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/new-types/intersection-types-spec.html) {#intersection-types-spec.}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/new-types/intersection-types-spec.md
```

## Type Lambdas - More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/new-types/type-lambdas-spec.html) {#type-lambdas-spec}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/new-types/type-lambdas-spec.md
```

## Union Types - More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/new-types/union-types-spec.html) {#union-types-spec}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/new-types/union-types-spec.md
```

## Macros Spec [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/metaprogramming/macros-spec.html) {#macros-spec}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/metaprogramming/macros-spec.md
```

## Erased Definitions: More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/experimental/erased-defs-spec.html) {#erased-defs-spec}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/experimental/erased-defs-spec.md
```

## Named Type Arguments - More Details [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/experimental/named-typeargs-spec.html) {#named-typeargs-spec}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/experimental/named-typeargs-spec.md
```

[features-classification]: # Referenced in overview.md #######################

\newpage
## A Classification of Proposed Language Features [![](../out/pandoc/images/external.png){width=64px}&nbsp;](https://docs.scala-lang.org/scala3/reference/features-classification.html) {#features-classification}
```{.include shift-heading-level-by=1}
../out/pandoc/src_managed/reference/features-classification.md
```
