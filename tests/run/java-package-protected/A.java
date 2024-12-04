// filter: unchecked
package a;

/** This is package protected. */
class B<T extends B<T>> {
    private int connectTimeout = 10000;
    private int failedAttempts = 3;

    public T setConnectTimeout(int connectTimeout) {
        this.connectTimeout = connectTimeout;
        return (T) this;
    }

    public T setFailedAttempts(int failedAttempts) {
        this.failedAttempts = failedAttempts;
        return (T) this;
    }
}

/** This is public. */
public class A extends B<A> { }
