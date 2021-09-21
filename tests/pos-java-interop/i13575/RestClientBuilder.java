package com.lamoroso.example;

public class RestClientBuilder<T extends Builder<T, R>, R> {

    private Builder<?, ?> wrappedBuilder;

    protected RestClientBuilder() {
        this.wrappedBuilder = Client.builder();
    }
    
    public RestClientBuilder<T, R> withPool(String... pools) {
        this.wrappedBuilder.withPool(pools);
        return this;
    }

    public RestClient build() {
        return new RestClient(wrappedBuilder.build());
    }
}