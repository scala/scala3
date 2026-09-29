# Todo List
Differences between scalameta implementation and dotc.

- Emit for-comprehension desugaring (map|flatMap|withFilter|foreach) in the Synthetics section.
  These calls are currently skipped (see `SyntheticsExtractor.isForSynthetic`); Scala 2 emits the
  full desugared tree as a synthetic.

## Completed

- [x] Recognise named arguments and traverse their bodies.
- [x] Recognise pattern val definitions.
- [x] Recognise anonymous functions.
- [x] Recognise specialised constant enum values.
- [x] Use setter symbol when assigning to a var.
- [x] Substitute constructor type params for the class parameters.
- [x] Skip the synthetic import statement in an Enum class.
- [x] Do not traverse RHS of synthetic val|var|def unless anonymous.
- [x] Avoid symbols with volatile names. - [$1$, $2$, etc].
- [x] Skip module val
- [x] Add metac printer.
- [x] Emit a definition occurrence for primary-constructor parameters, so the accessor and the
      constructor parameter share the parameter's name range (parity with scalameta#4650; closes the
      dotc side of scalameta#1327).
- [x] Emit synthetics for pattern val definitions (unapply|unapplySeq, zero-length).
- [x] Emit synthetics for implicit conversions.
- [x] Emit synthetics for implicit|given arguments.
- [x] Record signature information in Symbols section.
- [x] Record access modifier information in Symbols section.
