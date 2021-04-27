package dotty.tools.xsbt;

import java.util.Optional;
import xsbti.Position;
import xsbti.Severity;

final public class Problem implements xsbti.Problem {
  private final Position _position;
  private final String _message;
  private final Severity _severity;
  private final Optional<String> _rendered;

  public Problem(Position position, String message, Severity severity, String rendered) {
    super();
    this._position = position;
    this._message = message;
    this._severity = severity;
    this._rendered = Optional.of(rendered);
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

  @Override
  public String toString() {
    return "Problem(" + _position + ", " + _message + ", " + _severity + ", " + _rendered + ")";
  }
}
