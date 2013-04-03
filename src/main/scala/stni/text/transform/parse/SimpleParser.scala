package stni.text.transform.parse

import stni.text.transform.Segment._
import stni.text.transform.TransformContext

/**
 *
 */
class SimpleParser(context: TransformContext) extends AbstractCharReadingParser(context) {
  def doParse() {
    addToResult(plain(input))
  }
}
