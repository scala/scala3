package pkg

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