//> using options -Wenum-comment-discard
/**
 * Description of enum
 */
enum MyEnum {

  /**
   * Description of case 1
   */
  case MyCase1

  /** // warn
   * Description of case 2 and 3
   */
  case MyCase2, MyCase3
}