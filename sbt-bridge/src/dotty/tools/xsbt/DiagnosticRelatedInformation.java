package dotty.tools.xsbt;

import xsbti.Position;

final public class DiagnosticRelatedInformation implements xsbti.DiagnosticRelatedInformation {
  private final Position _position;
  private final String _message;

  public DiagnosticRelatedInformation(Position position, String message) {
    super();
    this._position = position;
    this._message = message;
  }

  public Position position() {
    return _position;
  }

  public String message() {
    return _message;
  }
}
