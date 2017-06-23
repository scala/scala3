package scala.tools.nsc;

// Override the MainGenericRunner for link deadcode elimination tests
public class MainGenericRunner {
    public static void main(String[] args) {
        try {
            java.lang.Class.forName("Test").getMethod("main", String[].class).invoke(null, (Object) args);
        } catch (ClassNotFoundException ex) {
            throw new java.lang.RuntimeException(ex);
        } catch (NoSuchMethodException ex) {
            throw new java.lang.RuntimeException(ex);
        } catch (IllegalAccessException ex) {
            throw new java.lang.RuntimeException(ex);
        } catch (java.lang.reflect.InvocationTargetException ex) {
            throw new java.lang.RuntimeException(ex);
        }
    }
}
