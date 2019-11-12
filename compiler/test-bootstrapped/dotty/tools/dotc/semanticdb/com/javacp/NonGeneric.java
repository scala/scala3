package com.javacp;

import java.io.IOException;
import java.io.Serializable;

public class NonGeneric extends java.lang.Exception implements Serializable, java.io.Flushable {
    @Override
    public void flush() throws IOException { }
}
