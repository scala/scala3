package dotty.tools.xsbt;

import java.util.List;
import java.util.Optional;
import static java.util.stream.Collectors.toList;

import dotty.tools.dotc.reporting.CodeAction;
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch;
import dotty.tools.dotc.util.SourcePosition;

import xsbti.Position;
import xsbti.Severity;

final public class Problem implements xsbti.Problem {
  private final Position _position;
  private final String _message;
  private final Severity _severity;
  private final Optional<String> _rendered;
  private final String _diagnosticCode;
  private final List<CodeAction> _actions;

  public Problem(Position position, String message, Severity severity, String rendered, String diagnosticCode, List<CodeAction> actions) {
    super();
    this._position = position;
    this._message = message;
    this._severity = severity;
    this._rendered = Optional.of(rendered);
    this._diagnosticCode = diagnosticCode;
    this._actions = actions;
  }

  public String category() {
    return "";
  }

  public Position position() {
    return _position;
  }

  public String message() {
    return _message;
  }

  public Severity severity() {
    return _severity;
  }

  public Optional<String> rendered() {
    return _rendered;
  }

  public Optional<xsbti.DiagnosticCode> diagnosticCode() {
    // We don't forward the code if it's -1 since some tools will assume that this is actually
    // the diagnostic code and show it or attempt to use it. This will ensure tools consuming
    // this don't all have to be adding checks for -1.
    if (_diagnosticCode == "-1") {
      return Optional.empty();
    } else {
      // NOTE: It's important for compatibility that we only construct a
      // DiagnosticCode here to maintain compatibility with older versions of
      // zinc while using this newer version of the compiler. If we would
      // contstruct it earlier, you'd end up with ClassNotFoundExceptions for
      // DiagnosticCode.
      return Optional.of(new DiagnosticCode(_diagnosticCode, Optional.empty()));
    }
  }

  public List<xsbti.Action> actions() {
    if (_actions.isEmpty()) {
      return java.util.Collections.emptyList();
    } else {
      // Same as with diagnosticCode, we need to ensure we don't create the actual
      // Action until we are here to ensure that when using an older version of sbt/zinc
      // with the new versions of the compiler, this doesn't blow up because this is
      // never getting called.
      return _actions
              .stream()
              .map(action -> new Action(action.title(), action.description(), toWorkspaceEdit(action.patches())))
              .collect(toList());
    }
  }

  private static WorkspaceEdit toWorkspaceEdit(List<ActionPatch> patches) {
    return new WorkspaceEdit(
      patches
        .stream()
        .map(patch -> new TextEdit(positionOf(patch.srcPos()), patch.replacement()))
        .collect(toList())
    );
  }

  private static Position positionOf(SourcePosition pos) {
    if (pos.exists()){
      return new PositionBridge(pos, pos.source());
    } else {
      return PositionBridge.noPosition;
    }
  }

  @Override
  public String toString() {
    return "Problem(" + _position + ", " + _message + ", " + _severity + ", " + _rendered + ", " + _diagnosticCode + ")";
  }
}
