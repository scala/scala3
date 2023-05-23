package dotty.tools.xsbt;

import java.util.List;

import xsbti.TextEdit;

final public class WorkspaceEdit implements xsbti.WorkspaceEdit {

  private final List<TextEdit> _changes;

  public WorkspaceEdit(List<TextEdit> changes) {
    super();
    this._changes = changes;
  }

  public List<TextEdit> changes() {
    return _changes;
  }
  
}
