package dotty.tools.dotc.vulpix;

import java.io.Serializable;

/** The status of each call to `main` in the test applications */
public class Statuses {
    interface Status {}

    static class Success implements Status, Serializable {
        public final String output;
        public Success(String output) { this.output = output; }
    }

    static class Failure implements Status, Serializable {
        public final String output;
        public Failure(String output) { this.output = output; }
    }

    static class Timeout implements Status, Serializable {}
}
