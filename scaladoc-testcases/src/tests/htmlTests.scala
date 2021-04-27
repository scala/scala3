package tests
package htmlTests


/**<table>
 * <tr>
 * <th> ASD </th>
 * <th> FGH </th>
 * <th> JKL </th>
 * </tr>
 * <tr>
 * <td> 123 </td>
 * <td> 456 </td>
 * <td> 789 </td>
 * </tr>
 * </table>
  * This is <span style="color:red;">red</span> text.
  * <b> I'm bold </b>
  * <i> And I'm italic </i>
  *
  * An example documention with markdown formatting
  *
  *
  * <pre><code>
  * def someScalaCode(x: String) = println("Hello " + x)
  * </code></pre>
  *
*/
class HtmlTest

/** Implements functionality for printing Scala values on the terminal. For reading values
 *  use [[scala.io.StdIn$ StdIn]].
 *  Also defines constants for marking up text on ANSI terminals.
 *
 *  == Console Output ==
 *
 *  Use the print methods to output text.
 *  {{{
 *   scala> Console.printf(
 *     "Today the outside temperature is a balmy %.1f°C. %<.1f°C beats the previous record of %.1f°C.\n",
 *     -137.0,
 *     -135.05)
 *   Today the outside temperature is a balmy -137.0°C. -137.0°C beats the previous record of -135.1°C.
 *  }}}
 *
 *  == ANSI escape codes ==
 *  Use the ANSI escape codes for colorizing console output either to STDOUT or STDERR.
 *  {{{
 *    import Console.{GREEN, RED, RESET, YELLOW_B, UNDERLINED}
 *
 *    object PrimeTest {
 *
 *      def isPrime(): Unit = {
 *
 *        val candidate = io.StdIn.readInt().ensuring(_ > 1)
 *
 *        val prime = (2 to candidate - 1).forall(candidate % _ != 0)
 *
 *        if (prime)
 *          Console.println(s"\${RESET}\${GREEN}yes\${RESET}")
 *        else
 *          Console.err.println(s"\${RESET}\${YELLOW_B}\${RED}\${UNDERLINED}NO!\${RESET}")
 *      }
 *
 *      def main(args: Array[String]): Unit = isPrime()
 *
 *    }
 *  }}}
 *
 *  <table style="border: 10px solid #000;width:100%">
 *    <tr><td style="background-color:#000;color:#fff">\$ scala PrimeTest</td></tr>
 *    <tr><td style="background-color:#000;color:#fff">1234567891</td></tr>
 *    <tr><td style="background-color:#000;color:#0f0">yes</td></tr>
 *    <tr><td style="background-color:#000;color:#fff">\$ scala PrimeTest</td></tr>
 *    <tr><td style="background-color:#000;color:#fff">56474</td></tr>
 *    <tr><td style="background-color:#000;color:#fff"><span style="background-color:#ff0;color:#f00;text-decoration:underline">NO!</span></td></tr>
 *  </table>
 *
 *  == IO redefinition ==
 *
 *  Use IO redefinition to temporarily swap in a different set of input and/or output streams. In this example the stream based
 *  method above is wrapped into a function.
 *
 *  {{{
 *    import java.io.{ByteArrayOutputStream, StringReader}
 *
 *    object FunctionalPrimeTest {
 *
 *      def isPrime(candidate: Int): Boolean = {
 *
 *        val input = new StringReader(s"\$candidate\n")
 *        val outCapture = new ByteArrayOutputStream
 *        val errCapture = new ByteArrayOutputStream
 *
 *        Console.withIn(input) {
 *          Console.withOut(outCapture) {
 *            Console.withErr(errCapture) {
 *              PrimeTest.isPrime()
 *            }
 *          }
 *        }
 *
 *        if (outCapture.toByteArray.nonEmpty) // "yes"
 *          true
 *        else if (errCapture.toByteArray.nonEmpty) // "NO!"
 *          false
 *        else throw new IllegalArgumentException(candidate.toString)
 *      }
 *
 *      def main(args: Array[String]): Unit = {
 *        val primes = (2 to 50) filter (isPrime)
 *        println(s"First primes: \$primes")
 *      }
 *
 *    }
 *  }}}
 *
 *
 *  <table style="border: 10px solid #000;width:100%">
 *    <tr><td style="background-color:#000;color:#fff">\$ scala FunctionalPrimeTest</td></tr>
 *    <tr><td style="background-color:#000;color:#fff">First primes: Vector(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47)</td></tr>
 *  </table>
 *
 *  @syntax wiki
 *  @groupname console-output Console Output
 *  @groupprio console-output 30
 *  @groupdesc console-output These methods provide output via the console.
 *
 *  @groupname io-default IO Defaults
 *  @groupprio io-default 50
 *  @groupdesc io-default These values provide direct access to the standard IO channels
 *
 *  @groupname io-redefinition IO Redefinition
 *  @groupprio io-redefinition 60
 *  @groupdesc io-redefinition These methods allow substituting alternative streams for the duration of
 *             a body of code. Threadsafe by virtue of [[scala.util.DynamicVariable]].
 *
 */
object Console