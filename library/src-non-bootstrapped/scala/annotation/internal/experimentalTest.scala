// KEEP IN NON-BOOTSTRAPPED SOURCES
package scala.annotation
package internal

/** This is a dummy definition that tests that the stdlib can be compiled with an experimental definition.
 *  This might be redundant, we keep it definition in case there are no other @experimental definitions in the library.
 *  As this definition is in `src-non-bootstrapped`, it will not be published.
 *  It may accidentally be visible while compiling the non-bootstrapped library.
 */
@experimental def testExperimental = 4
