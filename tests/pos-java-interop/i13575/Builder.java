package com.lamoroso.example;

import java.util.Collections;
import java.util.List;

public abstract class Builder<T extends Builder<T, R>, R> {

    private List<String> pools;

    public Builder<T, R> withPool(String... pools) {
        Collections.addAll(this.pools, pools);
        return this;
    }

    public Builder<T, R> build(){return null;}
}