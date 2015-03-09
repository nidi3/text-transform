/**
 * Copyright (C) 2013 Stefan Niederhauser (nidin@gmx.ch)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package guru.nidi.text.transform.format.latex

import guru.nidi.text.transform.Attribute._
import guru.nidi.text.transform.AttributeValue._
import guru.nidi.text.transform.Name._
import guru.nidi.text.transform._

class LatexFormatter(val context: TransformContext) extends Formatter {
  def format(segment: Segment) = {
    LatexFormatter.formatChildren(context, segment)
  }
}

private[latex] object LatexFormatter {
  def env(name: String, param: String = "")(block: => String) = s"\\begin{$name}$param $block\\end{$name}"

  def formatLabel(label: Option[String]): String = {
    label match {
      case Some(l) => s"\\label{$l}"
      case None => ""
    }
  }

  def formatCaption(context: TransformContext, segment: Segment) = segment(CAPTION) match {
    case Some(cap: Segment) => formatChildren(context, cap)
    case _ => ""
  }

  def staticFormatter(s: String)(context: TransformContext, segment: Segment) = s

  def cmdFormatter(name: String)(context: TransformContext, segment: Segment) = {
    val ch = formatChildren(context, segment)
    s"\\$name{$ch}"
  }

  val listFormatter = (context: TransformContext, segment: Segment) =>
    env(if (segment(TYPE).get == UNORDERED) "itemize" else "enumerate") {
      formatChildren(context, segment)
    }

  val definitionFormatter = (context: TransformContext, segment: Segment) => {
    def formatDefs(context: TransformContext, segment: Segment): String = {
      segment.children.map(child => formatChildren(context, child)).mkString("\\\\")
    }

    val width = segment(WIDTH) match {
      case Some(s) => s
      case _ => "5cm"
    }
    val item = segment(TEXT).get
    val text = formatDefs(context, segment)
    s"\\begin{description}[leftmargin=$width,style=sameline]\\item[$item]$text\\end{description}"
  }

  val headingFormatter = (context: TransformContext, segment: Segment) => {
    val headings = List(
      "chapter", "section", "subsection", "subsubsection", "paragraph", "subparagraph"
    )

    val level = segment(LEVEL).getOrElse(0)
    val heading = headings(Math.min(headings.length, context.headingLevel + level) - 1)
    cmdFormatter(heading)(context, segment)
  }

  val defaultFormatter = (context: TransformContext, segment: Segment) => ""

  val formatters = Map(
    LIST -> listFormatter,
    LINK -> LinkFormatter.format _,
    HEADING -> headingFormatter,
    SYMBOL -> SymbolFormatter.format _,
    IMAGE -> ImageFormatter.format _,
    TABLE -> TableFormatter.format _,
    LINE -> staticFormatter( """\\ \hline \noindent \\""") _,
    NEWLINE -> staticFormatter("\\\\") _,
    ITEM -> cmdFormatter("item") _,
    ITALICS -> cmdFormatter("textit") _,
    UNDERLINED -> cmdFormatter("underline") _,
    BOLD -> cmdFormatter("textbf") _,
    PLAIN -> PlainFormatter.format _,
    DEFINITION -> definitionFormatter
  )

  def format(context: TransformContext, segment: Segment): String =
    formatters.getOrElse(segment.name, defaultFormatter)(context, segment)

  def formatChildren(context: TransformContext, segment: Segment): String =
    segment.children.map(child => format(context, child)).mkString
}


