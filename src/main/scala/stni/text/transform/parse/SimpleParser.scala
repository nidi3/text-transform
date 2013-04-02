package stni.text.transform.parse

import stni.text.transform.Segment._
import stni.text.transform.Context

/**
 *
 */
class SimpleParser(context: Context) extends AbstractCharReadingParser(context) {
  def doParse() {
    addToResult(plain(input))
  }
}
