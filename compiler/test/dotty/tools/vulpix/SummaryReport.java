package dotty.tools.vulpix;

import org.junit.BeforeClass;
import org.junit.AfterClass;
import java.util.ArrayDeque;
import java.util.function.Supplier;
import scala.Function0;
import scala.Unit;

import dotty.tools.dotc.reporting.TestReporter;

/** Note that while `ParallelTesting` runs in parallel, JUnit tests cannot with
 *  this class
 */
public class SummaryReport {
    public final static boolean isInteractive = !System.getenv().containsKey("DRONE");

    private static TestReporter rep = TestReporter.reporter(System.out, -1);
    private static ArrayDeque<String> failedTests = new ArrayDeque<>();
    private static ArrayDeque<String> reproduceInstructions = new ArrayDeque<>();
    private static Supplier<Void> cleanup;
    private static int passed;
    private static int failed;

    public final static void reportFailed() {
        failed++;
    }

    public final static void reportPassed() {
        passed++;
    }

    public final static void addFailedTest(String msg) {
        failedTests.offer(msg);
    }

    public final static void addReproduceInstruction(String msg) {
        reproduceInstructions.offer(msg);
    }

    public final static void addCleanup(Function0<Unit> func) {
        // Wow, look at how neatly we - compose cleanup callbacks:
        if (cleanup == null) {
            cleanup = () -> {
                func.apply();
                return null;
            };
        } else {
            Supplier<Void> oldCleanup = cleanup;
            cleanup = () -> {
                oldCleanup.get();
                func.apply();
                return null;
            };
        }
    }

    @BeforeClass public final static void setup() {
        rep = TestReporter.reporter(System.out, -1);
        failedTests = new ArrayDeque<>();
        reproduceInstructions = new ArrayDeque<>();
    }

    @AfterClass public final static void teardown() {
        rep.echo(
            "\n================================================================================" +
            "\nTest Report" +
            "\n================================================================================" +
            "\n" +
            passed + " passed, " + failed + " failed, " + (passed + failed) + " total" +
            "\n"
        );

        failedTests
            .stream()
            .map(x -> "    " + x)
            .forEach(rep::echo);

        // If we're compiling locally, we don't need reproduce instructions
        if (isInteractive) rep.flushToStdErr();

        rep.echo("");

        reproduceInstructions
            .stream()
            .forEach(rep::echo);

        // If we're on the CI, we want everything
        if (!isInteractive) rep.flushToStdErr();

        if (failed > 0) rep.flushToFile();

        // Perform cleanup callback:
        if (cleanup != null) cleanup.get();
    }
}
