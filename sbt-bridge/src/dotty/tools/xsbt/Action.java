package dotty.tools.xsbt;

import java.util.Optional;

final public class Action implements xsbti.Action {
  private final String _title;
  private final Optional<String> _description;
  private final WorkspaceEdit _edit;

  public Action(String title, Optional<String> description, WorkspaceEdit edit) {
    super();
    this._title = title;
    this._description = description;
    this._edit = edit;
  }

  public String title() {
    return _title;
  }

  public Optional<String> description() {
    return _description;
  }

  public WorkspaceEdit edit() {
    return _edit;
  }
}
