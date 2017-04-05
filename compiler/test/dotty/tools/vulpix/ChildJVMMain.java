package dotty.tools.vulpix;

import java.io.File;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.lang.reflect.Method;

public class ChildJVMMain {
    static final String MessageEnd = "##THIS IS THE END FOR ME, GOODBYE##";

    private static void runMain(String dir) throws Exception {
        ArrayList<URL> cp = new ArrayList<>();
        for (String path : dir.split(":"))
            cp.add(new File(path).toURI().toURL());

        URLClassLoader ucl = new URLClassLoader(cp.toArray(new URL[cp.size()]));
        Class<?> cls = ucl.loadClass("Test");
        Method meth = cls.getMethod("main", String[].class);
        Object[] args = new Object[]{ new String[]{ "jvm" } };
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
