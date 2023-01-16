package dotty.tools.dotc.interfaces;

public interface DiagnosticRelatedInformation {
  SourcePosition position();
  String message();
}
