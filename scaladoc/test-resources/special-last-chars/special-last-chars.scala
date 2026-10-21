/** [[`bad$`]] */
def `bad$`: Int = ???

/** [[bad_!]] */
def bad_! : Int = ???

/** [[ok_+]] */
def ok_+ : Int = ???

/** [[class_!]] */
class class_! {
  /** [[class_!.`bad$`]] (or [[this.`bad$`]], or [[class_!#`bad$`]]) */
  def `bad$`: Int = ???

  /** [[class_!.bad_!]] (or [[this.bad_!]], or [[class_!#bad_!]]) */
  def bad_! : Int = ???

  /** [[class_!.ok_+]] (or [[this.ok_+]], or [[class_!#ok_+]]) */
  def ok_+ : Int = ???
}

/** [[`class_$`]] */
class `class_$` {
  /** [[`class_$`.`bad$`]] */
  def `bad$`: Int = ???

  /** [[`class_$`.bad_!]] */
  def bad_! : Int = ???

  /** [[`class_$`.ok_+]] */
  def ok_+ : Int = ???
}

package inPackage {
  /** [[`bad$`]] (or [[package.`bad$`]]) */
  def `bad$`: Int = ???

  /** [[bad_!]] (or [[package.bad_!]]) */
  def bad_! : Int = ???

  /** [[ok_+]] (or [[package.ok_+]]) */
  def ok_+ : Int = ???
}