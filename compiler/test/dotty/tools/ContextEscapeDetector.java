package dotty.tools;

import org.junit.runner.Result;
import org.junit.runner.notification.RunListener;
import org.junit.Assert;
import java.lang.ref.WeakReference;

public class ContextEscapeDetector extends RunListener {

    //context can be captured by objects, eg NoDenotation
    public static final int CONTEXTS_ALLOWED = 1;

    @Override
    public void testRunFinished(Result result) throws Exception {
        if (contextsAlive() > CONTEXTS_ALLOWED) {
            forceGCHeuristic0();
            if (contextsAlive() > CONTEXTS_ALLOWED) {
                forceGCHeuristic1();
                if (contextsAlive() > CONTEXTS_ALLOWED) {
                    forceGCHeuristic2();
                    forceGCHeuristic1();
                    int contextAlive = contextsAlive();
                    if (contextAlive > CONTEXTS_ALLOWED) {
                        StringBuilder names = new StringBuilder();
                        for (ContextEscapeDetection.TestContext ref : ContextEscapeDetection.contexts) {
                            if (ref.context.get() != null) names.append(ref.testName).append(' ');
                        }
                        Assert.fail("Multiple contexts survived test suite: " + names.toString());
                    }
                }
            }
        }
        super.testRunFinished(result);
    }

    private static synchronized int contextsAlive() {
        int count = 0;
        for (ContextEscapeDetection.TestContext ref : ContextEscapeDetection.contexts) {
            if (ref.context.get() != null) count++;
        }
        return count;
    }

    @SuppressWarnings("unused")
    private static volatile Object o = null;

    private static synchronized void forceGCHeuristic0() {
        System.gc();
        Runtime.getRuntime().gc();
        System.gc();
        Runtime.getRuntime().gc();
        System.gc();
        Runtime.getRuntime().gc();
        System.gc();
        Runtime.getRuntime().gc();
        System.gc();
    }

    private static synchronized void forceGCHeuristic1() {
        Object obj = new Object();
        WeakReference<Object> ref = new WeakReference<>(obj);
        obj = null;
        while (ref.get() != null) {
            System.gc();
        }
    }

    private static synchronized void forceGCHeuristic2() {
        try {
            Object[] arr = new Object[1024]; // up to 8 GB
            WeakReference<Object> ref = new WeakReference<>(arr);
            o = arr; // make sure array isn't optimized away

            Runtime runtime = Runtime.getRuntime();
            // allocate memory until no more that 64MB is left
            for (int i = 0; i < 1024 &&
                    runtime.totalMemory() != runtime.maxMemory() ||
                    runtime.freeMemory() < 1024 * 1024 * 64; i++) {
                int[] data = new int[1024 * 1024]; // 8MB
                for (int j = 0; j < 1024 * 1024; j++) {
                    data[j] = j; // force actual pages allocation
                }
                arr[i] = data;
            }
            o = null;
            arr = new Object[128];
            o = arr;
            // allocate 1 more GB
            for (int i = 0; i < 128; i++) {
                int[] data = new int[1024 * 1024]; // 8MB
                for (int j = 0; j < 1024 * 1024; j++) {
                    data[j] = j; // force actual pages allocation
                }
                arr[i] = data;
            }
            o = null;
            arr = null;

            forceGCHeuristic0();
            while (ref.get() != null) {
                System.gc();
            }
        } catch (OutOfMemoryError e) {
            o = null;
            // just swallow
        }
    }
}
