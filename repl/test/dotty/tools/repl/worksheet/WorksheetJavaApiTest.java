package dotty.tools.repl.worksheet;

import dotty.tools.repl.worksheet.interfaces.DiagnosticSeverity;
import dotty.tools.repl.worksheet.interfaces.EvaluatedWorksheet;
import dotty.tools.repl.worksheet.interfaces.WorksheetEvaluator;

import java.nio.file.Path;
import java.util.List;
import java.util.ServiceLoader;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

public class WorksheetJavaApiTest {
  @Test
  public void evaluatesThroughJavaInterfaces() {
    Path userClasspath = Path.of("target", "worksheet-user-classes");
    try (WorksheetEvaluator evaluator = new WorksheetDriver()
        .withClasspath(List.of(userClasspath))
        .withScreenWidth(100)) {
      EvaluatedWorksheet result = evaluator.evaluate(
          "java-api.worksheet.scala",
          "val answer = 21\nanswer * 2\n");

      assertTrue(result.diagnostics().isEmpty());
      assertEquals(2, result.statements().size());
      assertEquals(": Int = 21", result.statements().get(0).summary());
      assertEquals(": Int = 42", result.statements().get(1).summary());
      assertEquals(1, result.statements().get(1).position().startLine());
      assertEquals(List.of(userClasspath), result.classpath());
      assertThrows(UnsupportedOperationException.class, () -> result.statements().clear());
      assertThrows(UnsupportedOperationException.class, () -> result.classpath().clear());
    }
  }

  @Test
  public void configurationDoesNotMutateTheOriginalEvaluator() {
    try (WorksheetEvaluator original = new WorksheetDriver()) {
      Path userClasspath = Path.of("target", "worksheet-user-classes");
      try (WorksheetEvaluator configured = original.withClasspath(List.of(userClasspath))) {
        assertNotSame(original, configured);
        assertTrue(original.evaluate("original.worksheet.scala", "1 + 1\n").classpath().isEmpty());
        assertEquals(
            List.of(userClasspath),
            configured.evaluate("configured.worksheet.scala", "1 + 1\n").classpath());
      }
    }
  }

  @Test
  public void exposesDiagnosticsThroughJavaInterfaces() {
    try (WorksheetEvaluator evaluator = new WorksheetDriver()) {
      EvaluatedWorksheet result = evaluator.evaluate(
          "java-error.worksheet.scala",
          "val answer: String = 42\n");

      assertTrue(result.statements().isEmpty());
      assertEquals(DiagnosticSeverity.Error, result.diagnostics().get(0).severity());
      assertEquals(0, result.diagnostics().get(0).position().startLine());
    }
  }

  @Test
  public void discoversTheImplementationAsAService() {
    WorksheetEvaluator evaluator = ServiceLoader
        .load(WorksheetEvaluator.class, WorksheetDriver.class.getClassLoader())
        .findFirst()
        .orElseThrow();

    try (evaluator) {
      assertEquals(WorksheetDriver.class, evaluator.getClass());
    }
  }
}
