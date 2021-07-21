/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package dotty.tools.xsbt;

import dotty.tools.dotc.core.Contexts.Context;
import dotty.tools.dotc.reporting.AbstractReporter;
import dotty.tools.dotc.reporting.Diagnostic;
import dotty.tools.dotc.reporting.Message;
import dotty.tools.dotc.util.SourcePosition;
import xsbti.Position;
import xsbti.Severity;

final public class DelegatingReporter extends AbstractReporter {
  private xsbti.Reporter delegate;

  public DelegatingReporter(xsbti.Reporter delegate) {
    super();
    this.delegate = delegate;
  }

  public void dropDelegate() {
    delegate = null;
  }

  @Override
  public void printSummary(Context ctx) {
    delegate.printSummary();
  }

  public void doReport(Diagnostic dia, Context ctx) {
    Severity severity = severityOf(dia.level());
    Position position = positionOf(dia.pos().nonInlined());

    StringBuilder rendered = new StringBuilder();
    rendered.append(messageAndPos(dia, ctx));
    Message message = dia.msg();
    boolean shouldExplain = Diagnostic.shouldExplain(dia, ctx);
    if (shouldExplain && !message.explanation().isEmpty()) {
      rendered.append(explanation(message, ctx));
    }

    delegate.log(new Problem(position, message.msg(), severity, rendered.toString()));
  }

  private static Severity severityOf(int level) {
    Severity severity;
    switch (level) {
      case dotty.tools.dotc.interfaces.Diagnostic.ERROR: severity = Severity.Error; break;
      case dotty.tools.dotc.interfaces.Diagnostic.WARNING: severity = Severity.Warn; break;
      case dotty.tools.dotc.interfaces.Diagnostic.INFO: severity = Severity.Info; break;
      default:
        throw new IllegalArgumentException(String.format("Bad diagnostic level: %s", level));
    }
    return severity;
  }

  private static Position positionOf(SourcePosition pos) {
    if (pos.exists()){
      return new PositionBridge(pos, pos.source());
    } else {
      return PositionBridge.noPosition;
    }
  }
}
