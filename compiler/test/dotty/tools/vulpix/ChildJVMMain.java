package dotty.tools.vulpix;

import java.io.File;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.lang.reflect.Method;

public class ChildJVMMain {
    static final String MessageStart = "##THIS IS THE START FOR ME, HELLO##";
    static final String MessageEnd = "##THIS IS THE END FOR ME, GOODBYE##";

    private static void runMain(String dir) throws Exception {
        Method meth = null;
        Object[] args = new Object[]{ new String[]{ } };
        try {
            String jcp = System.getProperty("java.class.path");
            String sep = File.pathSeparator;
            System.setProperty("java.class.path", jcp == null ? dir : dir + sep + jcp);

            ArrayList<URL> cp = new ArrayList<>();
            for (String path : dir.split(sep))
                cp.add(new File(path).toURI().toURL());

            URLClassLoader ucl = new URLClassLoader(cp.toArray(new URL[cp.size()]));

            Class<?> cls = ucl.loadClass("Test");
            meth = cls.getMethod("main", String[].class);
        }
        catch (Throwable e) {
            // Include the failure stack trace to the test output
            System.out.println(MessageStart);
            e.printStackTrace();
            throw e;
        }
        System.out.println(MessageStart);

        meth.invoke(null, args);
    }

    public static void main(String[] args) throws Exception {
      BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));

      while (true) {
          runMain(stdin.readLine());
          System.out.println(MessageEnd);
      }
    }
}
