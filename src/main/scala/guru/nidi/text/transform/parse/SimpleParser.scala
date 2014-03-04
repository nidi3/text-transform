package guru.nidi.text.transform.parse

import guru.nidi.text.transform.Segment._
import guru.nidi.text.transform.TransformContext

/**
 *
 */
class SimpleParser(context: TransformContext) extends AbstractCharReadingParser(context) {
  def doParse() {
    addToResult(plain(input))
  }
}
