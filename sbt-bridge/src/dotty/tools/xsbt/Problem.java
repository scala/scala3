package dotty.tools.xsbt;

import java.util.Optional;
import xsbti.Position;
import xsbti.Severity;

final public class Problem implements xsbti.Problem {
  private final Position _position;
  private final String _message;
  private final Severity _severity;
  private final Optional<String> _rendered;
  private final String _diagnosticCode;

  public Problem(Position position, String message, Severity severity, String rendered, String diagnosticCode) {
    super();
    this._position = position;
    this._message = message;
    this._severity = severity;
    this._rendered = Optional.of(rendered);
    this._diagnosticCode = diagnosticCode;
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
    // NOTE: It's important for compatibility that we only construct a
    // DiagnosticCode here to maintain compatibility with older versions of
    // zinc while using this newer version of the compiler. If we would
    // contstruct it earlier, you'd end up with ClassNotFoundExceptions for
    // DiagnosticCode.
    return Optional.of(new DiagnosticCode(_diagnosticCode, Optional.empty()));
  }

  @Override
  public String toString() {
    return "Problem(" + _position + ", " + _message + ", " + _severity + ", " + _rendered + ", " + _diagnosticCode + ")";
  }
}
