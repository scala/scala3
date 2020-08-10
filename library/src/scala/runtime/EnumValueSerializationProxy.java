package scala.runtime;

import java.io.Serializable;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;

/** A serialization proxy for singleton enum values */
public final class EnumValueSerializationProxy implements Serializable {
    private static final long serialVersionUID = 1L;
    private final Class<?> enumClass;
    private final int ordinal;
    private static final ClassValue<Object[]> enumValues = new ClassValue<Object[]>() {
        @Override
        protected Object[] computeValue(Class<?> type) {
            try {
                return AccessController.doPrivileged((PrivilegedExceptionAction<Object[]>) () ->
                  (Object[])type.getMethod("values").invoke(null));
            } catch (PrivilegedActionException e) {
                return rethrowRuntime(e.getCause());
            }
        }
    };

    private static <T> T rethrowRuntime(Throwable e) {
        Throwable cause = e.getCause();
        if (cause instanceof RuntimeException) throw (RuntimeException) cause;
        else throw new RuntimeException(cause);
    }

    public EnumValueSerializationProxy(Class<?> enumClass, int ordinal) {
        this.enumClass = enumClass;
        this.ordinal = ordinal;
    }

    @SuppressWarnings("unused")
    private Object readResolve() {
        return enumValues.get(enumClass)[ordinal];
    }
}
