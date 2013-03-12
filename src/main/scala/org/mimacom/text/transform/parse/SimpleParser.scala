package org.mimacom.text.transform.parse

import org.mimacom.text.transform.Segment

/**
 *
 */
class SimpleParser extends AbstractParser {
  def doParse {
    addToResult(Segment.plainText(input))
  }
}
