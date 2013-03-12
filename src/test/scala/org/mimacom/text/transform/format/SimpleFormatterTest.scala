package org.mimacom.text.transform.format


import org.mimacom.text.transform.Segment._
import org.mimacom.text.transform.Name._
import org.mimacom.text.transform.Attribute._

import org.scalatest.FlatSpec
import org.mimacom.text.transform.Segment

/**
 *
 */
class SimpleFormatterTest extends FlatSpec {
  behavior of "SimpleFormater"

  it should "understand text and symbols and ignore all the rest" in {
    val formatter = new SimpleFormatter()
    val seg = Segment(ROOT, plainText("hula"),
      Segment(SYMBOL, ORIGINAL -> "-->"), Segment(BOLD, plainText("xxx")))
    assert("hula-->" === formatter.format(seg))
  }
}
