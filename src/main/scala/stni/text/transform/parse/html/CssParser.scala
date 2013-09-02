package stni.text.transform.parse.html

import java.util.regex.Pattern
import stni.text.transform.parse.CustomizerParser

/**
 * Parse customizing css attributes of an element.
 */
object CssParser {
  private val PATTERN = Pattern.compile("([A-Za-z-_]+)\\s*(:\\s*([^;]+))?")

  def apply(input: String, block: (String, String) => Unit) = CustomizerParser.apply(PATTERN, input, block)
}
