package stni.text.transform.format.latex

import stni.text.transform.{TransformContext, Segment}
import stni.text.transform.Attribute._

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
    ']' -> "{]}",
    160 -> '~') //non-break space (&nbsp; in HTML)

  def format(context: TransformContext, segment: Segment) = escaped(segment(TEXT).get.asInstanceOf[String])

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
    res.toString()
  }
}
