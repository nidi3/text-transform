package stni.text.transform.parse

import stni.text.transform.Segment._

/**
 *
 */
class SimpleParser extends AbstractParser {
  def doParse() {
    addToResult(plain(input))
  }
}
