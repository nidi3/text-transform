package org.mimacom.text.transform.format.latex

import org.mimacom.text.transform.format.ResourceLoader
import org.mimacom.text.transform.{Formatter, Segment}
import java.util.Locale
import org.mimacom.text.transform.Attribute._
import org.mimacom.text.transform.Name._
import org.mimacom.text.transform.AttributeValue._

class LatexFormatter(headingLevel: Int, locale: Locale, resourceLoader: ResourceLoader) extends Formatter {
  val context = new Context(headingLevel, locale, resourceLoader)

  def format(segment: Segment) = {
    LatexFormatter.formatChildren(context, segment)
  }
}

private[latex] object LatexFormatter {
  def env(name: String)(block: => String) = s"\\begin{$name}$block\\end{$name}"

  def staticFormatter(s: String)(context: Context, segment: Segment) = s

  def cmdFormatter(name: String)(context: Context, segment: Segment) =
    "\\%s{%s}".format(name, formatChildren(context, segment))

  val listFormatter = (context: Context, segment: Segment) =>
    env(if (segment(TYPE).get == UNORDERED) "itemize" else "enumerate") {
      formatChildren(context, segment)
    }

  val linkFormatter = (context: Context, segment: Segment) => {
    val target = segment(TARGET).get.asInstanceOf[String]
    if (target.startsWith("http://") || target.startsWith("https://")) {
      val fc = formatChildren(context, segment)
      s"\\href{$target}{$fc}"
    } else if (target.startsWith("image:")) {
      val msg = context.message("image")
      s"$msg \\ref{$target}"
    } else {
      s"\\ref{$target}"
    }
  }

  val definitionFormatter = (context: Context, segment: Segment) => {
    def formatDefs(context: Context, segment: Segment): String = {
      segment.children.map(child => formatChildren(context, child)).mkString("\\\\")
    }

    val width = segment(WIDTH) match {
      case Some(s) => s
      case _ => "5cm"
    }
    val item = segment(TEXT).get.asInstanceOf[String]
    val text = formatDefs(context, segment)
    s"\\begin{description}[leftmargin=$width,style=sameline]\\item[$item]$text\\end{description}"
  }

  val headingFormatter = (context: Context, segment: Segment) => {
    val headings = List(
      "chapter", "section", "subsection", "subsubsection", "paragraph", "subparagraph"
    )

    val level = segment(LEVEL).getOrElse(0).asInstanceOf[Int]
    val heading = headings(Math.min(headings.length, context.headingLevel + level) - 1)
    cmdFormatter(heading)(context, segment)
  }

  val defaultFormatter = (context: Context, segment: Segment) => ""

  val formatters = Map(
    LIST -> listFormatter,
    LINK -> linkFormatter,
    HEADING -> headingFormatter,
    SYMBOL -> SymbolFormatter.format _,
    IMAGE -> ImageFormatter.format _,
    TABLE -> TableFormatter.format _,
    LINE -> staticFormatter("\\\\ \\hline \\noindent \\\\") _,
    NEWLINE -> staticFormatter("\\\\") _,
    ITEM -> cmdFormatter("item") _,
    ITALICS -> cmdFormatter("textit") _,
    BOLD -> cmdFormatter("textbf") _,
    PLAIN -> PlainFormatter.format _,
    DEFINITION -> definitionFormatter
  )

  def format(context: Context, segment: Segment): String =
    formatters.getOrElse(segment.name, defaultFormatter)(context, segment)

  def formatChildren(context: Context, segment: Segment): String =
    segment.children.map(child => format(context, child)).mkString
}


