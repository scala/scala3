package dotty.tools.dotc.callbacks;

/**
 * @author Nikolay.Tropin
 */
public interface ReportingCallback {

    void info(String msg, Position pos);

    void warning(String msg, Position pos);

    void error(String msg, Position pos);

    void debug(String msg);

    void trace(Throwable exception);
}
