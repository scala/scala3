package tests.snippetComments


/**
 * This is my codeblock
 * 
 * ```
 * //{{
 * import xd
 * import xd2
 * //}}
 *
 *
 * val x = 1 // This is my valid comment
 *
 * /*
 *  multi line comment
 *  */
 * val reallyFLongDeclaration = "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
 * val y = 2 // comment in the same line
 * // comment in new line
 * val z = 3
 * 
 * //{{
 * val hideMe = 7
 * //}}
 * ```
 *
 * The end of my codeblock
 */
class A
