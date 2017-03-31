package dotty.tools.dotc;

import org.junit.BeforeClass;
import org.junit.AfterClass;
import java.util.ArrayDeque;

import dotty.tools.dotc.reporting.TestReporter;
import dotty.tools.dotc.reporting.TestReporter$;

/** Note that while `ParallelTesting` runs in parallel, JUnit tests cannot with
 *  this class
 */
public class ParallelSummaryReport {
    public final static boolean isInteractive = !System.getenv().containsKey("DRONE");

    private static TestReporter rep = TestReporter.reporter(-1);
    private static ArrayDeque<String> failedTests = new ArrayDeque<>();
    private static ArrayDeque<String> reproduceInstructions = new ArrayDeque<>();
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

    @BeforeClass public final static void setup() {
        rep = TestReporter.reporter(-1);
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
    }
}
