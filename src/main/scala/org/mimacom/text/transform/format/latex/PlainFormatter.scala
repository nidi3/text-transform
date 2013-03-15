package org.mimacom.text.transform.format.latex

import org.mimacom.text.transform.Segment
import org.mimacom.text.transform.Attribute._

/**
 * Formats plain text by handling escaping issues.
 */
object PlainFormatter {
  private val REPLACEMENT = Map(
    '\\' -> "\\textbackslash ",
    '#' -> "\\#",
    '$' -> "\\$",
    '%' -> "\\%",
    '&' -> "\\&",
    '~' -> "\\~{}",
    '_' -> "\\_",
    '^' -> "\\^{}",
    '{' -> "\\{",
    '}' -> "\\}",
    '[' -> "{[}",
    ']' -> "{]}")

  def format(context:Context,segment: Segment) = escaped(segment(TEXT).get.asInstanceOf[String])


  private def escaped(s: String): String = {
    val res = new StringBuilder
    var inDoubleQuotes = false
    s.foreach(c => {
      REPLACEMENT.get(c) match {
        case Some(replace) =>
          res.append(replace)
        case _ =>
          c match {
            case '"' =>
              res.append(if (inDoubleQuotes) "\"'" else "\"`")
              inDoubleQuotes = !inDoubleQuotes
            case '„' =>
              inDoubleQuotes = true
              res.append('„')
            case _ =>
              res.append(c)
          }
      }
    })
    res.toString
  }
}
