package org.mimacom.text.transform.parse

/**
 *
 */
class TextAccumulator() {
  private val text = new StringBuilder

  def append(s: String) {
    text.append(s)
  }

  def append(c: Char) {
    text.append(c)
  }

  def trim() {
    while (text.last <= ' ') text.deleteCharAt(text.length - 1)
  }

  def reset() {
    text.clear()
  }

  def endsWith(ends: String*) = ends.find(end => text.endsWith(end))

  def isEmptyOrNewline = text.length == 0 || text.charAt(text.length - 1) == '\n'

  def removeLast(n: Int) {
    text.setLength(text.length - n)
  }

  override def toString = text.toString
}
