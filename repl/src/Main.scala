package dotty.tools
package dotc
package repl

/* This REPL was adapted from an old (2008-ish) version of the Scala
 * REPL. The original version from which the adaptation was done is found in:
 *
 *      https://github.com/odersky/legacy-svn-scala/tree/spoon
 *
 * The reason this version was picked instead of a more current one is that
 * the older version is much smaller, therefore easier to port. It is also
 * considerably less intertwined with nsc than later versions.
 *
 * There are a number of TODOs:
 *
 *  - figure out why we can launch REPL only with `java`, not with `scala`.
 *  - make a doti command (urgent, easy)
 *  - create or port REPL tests (urgent, intermediate)
 *  - copy improvements of current Scala REPL wrt to this version
 *    (somewhat urgent, intermediate)
 *  - re-enable bindSettings (not urgent, easy, see TODO in InterpreterLoop.scala)
 *  - make string generation more functional (not urgent, easy)
 *  - better handling of ^C (not urgent, intermediate)
 *  - syntax highlighting (not urgent, intermediate)
 *  - integrate with presentation compiler for command completion (not urgent, hard)
 */
/** The main entry point of the REPL */
object Main extends REPL
