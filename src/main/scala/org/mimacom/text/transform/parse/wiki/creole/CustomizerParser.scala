package org.mimacom.text.transform.parse.wiki.creole

;


import java.util.regex.Pattern;

/**
 * Parse customizing attributes of an element, like the span in "|&lt;span=2>cell|".
 */
class CustomizerParser(input: String) {
  private val CUSTOMIZER = Pattern.compile("<(\\w+)(=([^>]+))?>")
  private val matcher = CUSTOMIZER.matcher(input)
  private var _rest = input

  def find(): Boolean = {
    matcher.reset(rest)
    val found = matcher.find()
    if (found) {
      _rest = _rest.substring(0, matcher.start()) + _rest.substring(matcher.end())
    }
    found
  }

  def name = matcher.group(1)

  def value = if (matcher.groupCount() == 3) matcher.group(3) else null

  def rest = _rest
}
