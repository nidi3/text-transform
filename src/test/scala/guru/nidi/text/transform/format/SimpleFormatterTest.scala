package guru.nidi.text.transform.format


import guru.nidi.text.transform.Segment._
import guru.nidi.text.transform.Name._
import guru.nidi.text.transform.Attribute._


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
