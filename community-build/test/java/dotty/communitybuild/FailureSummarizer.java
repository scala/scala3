package dotty.communitybuild;

import java.util.List;

import org.junit.runner.Description;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;

public class FailureSummarizer extends RunListener {
  @Override
  public void testRunFinished(Result result) throws Exception {
    super.testRunFinished(result);
    if (result.getFailureCount() > 0) {
      Thread.sleep(500);  // pause to give sbt log buffers some time to flush
      summarizeFailures(result.getFailures());
    }
  }

  private void summarizeFailures(List<Failure> failures) {
    err("********************************************************************************");
    err("Failed projects:");
    for (Failure f : failures) {
      err(" - " + getProjectName(f.getDescription()));
    }
    err("********************************************************************************");
  }

  private String getProjectName(Description desc) {
    return desc.getClassName() + "." + desc.getMethodName();
  }

  private void err(String msg) {
    System.err.println(msg);
  }
}
