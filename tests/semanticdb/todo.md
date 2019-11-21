# Todo List
Differences between scalameta implementation and dotc.

- Generally put all zero length method calls or arguments in Synthetics section
  - Pattern val defs -- [unapply|unapplySeq is zero-length]
  - For comprehensions -- [map|flatMap|withFilter|foreach etc is zero-length].
  - Implicit conversions -- [span of Apply node is same as its single argument (which has a length)].
  - Implicit arguments -- [span of argument is zero length].
- Record signature information in Symbols section.
- Record access modifier information in Symbols section.

## Completed

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
