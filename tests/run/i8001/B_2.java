public class B_2 {
  public static void test() {
    A a = new A();
    a.test();

    int[] intArr = { 1 };
    int[][] intArr2 = { { 1 } };
    String[] stringArr = { "" };
    String[][] stringArr2 = { { "" } };
    Integer[] integerArr = { 1 };

    a.poly1(1);
    a.poly2("");
    a.poly3(intArr);
    a.poly4(stringArr);

    a.arr1(intArr);
    a.arr2(stringArr);
    a.arr3(intArr);
    a.arr4(stringArr2);

    a.arrRef1(integerArr);
    a.arrRef2(stringArr);
    a.arrRef3(intArr2);
    a.arrRef4(stringArr2);
  }
}
