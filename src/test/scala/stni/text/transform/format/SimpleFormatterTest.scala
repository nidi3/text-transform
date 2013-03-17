package stni.text.transform.format


import stni.text.transform.Segment._
import stni.text.transform.Name._
import stni.text.transform.Attribute._


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
