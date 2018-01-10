package dotty.tools.dotc.reporting.diagnostic;

public enum ErrorKind {

    COMPATIBILITY("Compatibility"),
    DEFINITION_NOT_FOUND("Definition Not Found"),
    DUPLICATE_SYMBOL("Duplicate Symbol"),
    MATCH_CASE_UNREACHABLE("Match case Unreachable"),
    MEMBER_NOT_FOUND("Member Not Found"),
    NAMING("Naming"),
    NO_KIND(""),
    PATTERN_MATCH_EXHAUSTIVITY("Pattern Match Exhaustivity"),
    REFERENCE("Reference"),
    SYNTAX("Syntax"),
    TYPE_MISMATCH("Type Mismatch"),
    UNBOUND_IDENTIFIER("Unbound Identifier"),
    USAGE("Usage");

    private String repr;
    ErrorKind(String repr) {
        this.repr = repr;
    }

    @Override
    public String toString() {
        return repr;
    }
}
