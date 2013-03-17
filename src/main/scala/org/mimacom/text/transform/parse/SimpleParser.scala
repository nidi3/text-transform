package org.mimacom.text.transform.parse

import org.mimacom.text.transform.Segment._

/**
 *
 */
class SimpleParser extends AbstractParser {
  def doParse() {
    addToResult(plain(input))
  }
}
