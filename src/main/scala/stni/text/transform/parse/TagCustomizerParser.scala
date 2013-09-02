package stni.text.transform.parse

import java.util.regex.Pattern

/**
 * Parse customizing attributes of an element, like the span in "|&lt;span=2>cell|".
 */
object TagCustomizerParser {
  private val PATTERN = Pattern.compile("<([A-Za-z-]+)(=([^>]+))?>")

  def apply(input: String, block: (String, String) => Unit) = CustomizerParser.apply(PATTERN, input, block)
}
