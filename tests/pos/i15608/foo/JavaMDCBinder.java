package foo;

import bar.MDCAdapter;
import bar.ScalaMDCAdapter;
import bar.ScalaMDCAdapter$;

public class JavaMDCBinder {
    public static final JavaMDCBinder SINGLETON = new JavaMDCBinder();

    private JavaMDCBinder() {
    }

    public MDCAdapter getMDCA() {
        return ScalaMDCAdapter$.MODULE$;
    }

    public String getMDCAdapterClassStr() {
        return ScalaMDCAdapter.class.getName();
    }
}