package dotty.tools.xsbt;

import xsbti.Position;

final public class TextEdit implements xsbti.TextEdit {
  private final Position _position;
  private final String _newText;

  public TextEdit(Position position, String newText) {
    super();
    this._position = position;
    this._newText = newText;
  }

  public Position position() {
    return _position;
  }

  public String newText() {
    return _newText;
  }
  
}
