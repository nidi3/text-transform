package org.mimacom.text.transform.format


import org.mimacom.text.transform.Segment._
import org.mimacom.text.transform.Name._
import org.mimacom.text.transform.Attribute._


/**
 *
 */
class SimpleFormatterTest extends FormatterTest {
  val formatter = new SimpleFormatter

  behavior of "SimpleFormater"

  it should "understand text and symbols and ignore all the rest" in {
    "hula-->" formatOf ROOT(plain("hula"), SYMBOL(ORIGINAL -> "-->"), BOLD(plain("xxx")))
  }
}
