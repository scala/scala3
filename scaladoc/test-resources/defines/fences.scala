package pkg

/**
 * These should not be escaped: `x < y`, `a > b`, `i <not-a-tag> j`
 */
class B

/**
 * This is OK: `$a` `${b}`
 * And so is this:
 * ```
 * $c
 * ```
 * And that:
 * ```
 * ${d} `hello` $e
 * ```
 *
 * Fenced blocks should be finished the same way they started:
 * ```
 * this block does not end here: ` so this should not be escaped: i < j
 * ```
 */
class C

/**
 * Oopsie, this one is unfinished: `hello
 */
class D

/**
 * And so is this one, too bad:
 * ```
 * hello
 */
class E