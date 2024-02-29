/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package dotty.tools.xsbt;

import java.util.List;

import scala.Tuple2;
import scala.collection.mutable.HashMap;
import scala.jdk.javaapi.CollectionConverters;

import dotty.tools.dotc.core.Contexts.Context;
import dotty.tools.dotc.reporting.AbstractReporter;
import dotty.tools.dotc.reporting.CodeAction;
import dotty.tools.dotc.reporting.Diagnostic;
import dotty.tools.dotc.reporting.Message;
import dotty.tools.dotc.util.SourceFile;
import dotty.tools.dotc.util.SourcePosition;
import xsbti.Position;
import xsbti.Severity;

import java.util.Collections;
import java.util.function.*;

final public class DelegatingReporter extends AbstractReporter {
  private xsbti.Reporter delegate;

  // A function that can lookup the `id` of the VirtualFile
  // associated with a SourceFile. If there is not an associated virtual file,
  // then it is the path of the SourceFile as a String.
  private final Function<SourceFile, String> lookupVirtualFileId;

  public DelegatingReporter(xsbti.Reporter delegate, Function<SourceFile, String> lookupVirtualFileId) {
    super();
    this.delegate = delegate;
    this.lookupVirtualFileId = lookupVirtualFileId;
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
    Message message = dia.msg();
    String text;
    if (Diagnostic.shouldExplain(dia, ctx) && !message.explanation().isEmpty())
      text = message.message() + System.lineSeparator() + explanation(message, ctx);
    else
      text = message.message();
    String rendered = messageAndPos(dia, ctx);
    String diagnosticCode = String.valueOf(message.errorId().errorNumber());
    List<CodeAction> actions = CollectionConverters.asJava(message.actions(ctx));
    Problem problem = new Problem(position, text, severity, rendered, diagnosticCode, actions, lookupVirtualFileId);
    delegate.log(problem);
  }

  public void reportBasicWarning(String message) {
    Position position = PositionBridge.noPosition;
    Severity severity = Severity.Warn;
    String diagnosticCode = "-1"; // no error code
    List<CodeAction> actions = Collections.emptyList();
    delegate.log(new Problem(position, message, severity, message, diagnosticCode, actions, lookupVirtualFileId));
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

  private Position positionOf(SourcePosition pos) {
    if (pos.exists()) {
      return new PositionBridge(pos, lookupVirtualFileId.apply(pos.source()));
    } else {
      return PositionBridge.noPosition;
    }
  }

  @SuppressWarnings("unchecked")
  // [warn] sbt-bridge/src/dotty/tools/xsbt/DelegatingReporter.java:18:1: dotty$tools$dotc$reporting$UniqueMessagePositions$$positions() in dotty.tools.dotc.reporting.AbstractReporter implements dotty$tools$dotc$reporting$UniqueMessagePositions$$positions() in dotty.tools.dotc.reporting.UniqueMessagePositions
  // [warn]   return type requires unchecked conversion from scala.collection.mutable.HashMap to scala.collection.mutable.HashMap<scala.Tuple2<dotty.tools.dotc.util.SourceFile,java.lang.Integer>,dotty.tools.dotc.reporting.Diagnostic>
  public HashMap<Tuple2<SourceFile, Integer>, Diagnostic> dotty$tools$dotc$reporting$UniqueMessagePositions$$positions() {
    return (HashMap<Tuple2<SourceFile, Integer>, Diagnostic>) super.dotty$tools$dotc$reporting$UniqueMessagePositions$$positions();
  }
}
