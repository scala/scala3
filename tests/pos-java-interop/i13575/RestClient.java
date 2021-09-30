package com.lamoroso.example;

public class RestClient {

    private Object instance;

    public RestClient(Object instance) {
        this.instance = instance;
    }

    public static RestClientBuilder<?,?> builder() {
        return new RestClientBuilder();
    }
    
}
