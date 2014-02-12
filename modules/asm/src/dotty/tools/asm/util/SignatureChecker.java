/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 */

package dotty.tools.asm.util;

import dotty.tools.asm.util.CheckMethodAdapter;
import dotty.tools.asm.MethodVisitor;

/**
 * A subclass of ASM's CheckMethodAdapter for the sole purpose of accessing some protected methods there.
 *
 */
public class SignatureChecker extends CheckMethodAdapter {

    public SignatureChecker(final MethodVisitor mv) {
        super(mv);
    }

    /**
     * Checks a class signature.
     *
     * @param signature a string containing the signature that must be checked.
     */
    public static void checkClassSignature(final String signature) {
      CheckMethodAdapter.checkClassSignature(signature);
    }

    /**
     * Checks a method signature.
     *
     * @param signature a string containing the signature that must be checked.
     */
    public static void checkMethodSignature(final String signature) {
      CheckMethodAdapter.checkMethodSignature(signature);
    }

    /**
     * Checks a field signature.
     *
     * @param signature a string containing the signature that must be checked.
     */
    public static void checkFieldSignature(final String signature) {
      CheckMethodAdapter.checkFieldSignature(signature);
    }

}
