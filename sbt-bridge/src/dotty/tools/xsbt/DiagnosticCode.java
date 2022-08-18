package dotty.tools.xsbt;

import java.util.Optional;

final public class DiagnosticCode implements xsbti.DiagnosticCode {
	private final String _code;
	private final Optional<String> _explanation;

	public DiagnosticCode(String code, Optional<String> explanation) {
		super();
		this._code = code;
		this._explanation = explanation;
	}

	public String code() {
		return _code;
	}

	public Optional<String> explanation() {
		return _explanation;
	}

}
