package foo.slf4j.impl;

import bar.slf4j.MDCAdapter;
import bar.slf4j.ScalaMDCAdapter;
import bar.slf4j.ScalaMDCAdapter$;

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