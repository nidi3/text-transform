package stni.text.transform.parse

import java.util.regex.Pattern

/**
 * Parse customizing attributes of an element, like the span in "|&lt;span=2>cell|".
 */
object CustomizerParser {
  private val CUSTOMIZER = Pattern.compile("<(\\w+)(=([^>]+))?>")

  def apply(input: String, block: (String, String) => Unit) = {
    val matcher = CUSTOMIZER.matcher(input)
    var rest = input

    def find(): Boolean = {
      matcher.reset(rest)
      val found = matcher.find()
      if (found) {
        rest = rest.substring(0, matcher.start()) + rest.substring(matcher.end())
      }
      found
    }

    def name = matcher.group(1)

    def value = if (matcher.groupCount() == 3) matcher.group(3) else null

    while (find()) {
      block(name, value)
    }
    rest
  }
}
