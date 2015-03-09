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
import guru.nidi.text.transform.format.latex.LatexFormatter.env
import guru.nidi.text.transform.{Attribute, Segment, TransformContext}

/**
 * Formats a table element.
 */
object TableFormatter {
  def format(context: TransformContext, segment: Segment) = new TableFormatter(context, segment).format
}

class TableFormatter(context: TransformContext, segment: Segment) {
  val rows = segment(ROWS).get
  val cols = segment(COLUMNS).get

  def format: String = {
    val caption = LatexFormatter.formatCaption(context, segment)
    segment(FLOAT) match {
      case Some(true) => float(caption, LatexFormatter.formatLabel(segment(ID)))
      case _ => nonfloat(caption, LatexFormatter.formatLabel(segment(ID)))
    }
  }

  private def caption(cmd: String, caption: String, formattedLabel: String) = {
    if (caption == "") ""
    else s"\n\\$cmd{$caption} $formattedLabel"
  }

  private def float(cap: String, formattedLabel: String) = {
    env("table", "[hpt]") {
      "\\centering\n" +
        table + caption("caption", cap, formattedLabel)
    }
  }

  private def nonfloat(cap: String, formattedLabel: String) = {
    env("center") {
      table + caption("captionof{table}", cap, formattedLabel)
    }
  }

  private def table = {
    env("longtable", s"{@{} $columnsStyle@{}} \\toprule") {
      (1 to rows).map(row => doRow(row)).mkString + "\\bottomrule "
    }
  }

  private def columnWidth(index: Int) = {
    val noWidth = (1 to cols).forall(col => segment(WIDTH(col)).isEmpty)
    if (noWidth) Some(90.0 / cols + "%")
    else segment(WIDTH(index))
  }

  private def columnsStyle = {
    (1 to cols).map(col => columnWidth(col) match {
      case Some(width: String) => "p{" + handlePercent(width) + "} "
      case _ => "l "
    }).mkString
  }

  private def handlePercent(value: String): String = {
    def calcPercent = {
      try {
        java.lang.Double.parseDouble(value.substring(0, value.length() - 1))
      } catch {
        case _: NumberFormatException => 100.0
      }
    }
    if (value.endsWith("%")) (calcPercent / 100.0) + " \\textwidth" else value
  }

  private def doRow(row: Int) = {
    var currentCol = 1

    def alignStyle(align: Option[Any], fallback: Option[Any]): String = align match {
      case Some(LEFT) => "\\raggedright "
      case Some(RIGHT) => "\\raggedleft "
      case _ => if (fallback.isEmpty) "" else alignStyle(fallback, None)
    }

    def doCell(cell: Segment) = {
      val content = alignStyle(cell(ALIGN), segment(ALIGN(currentCol))) + LatexFormatter.formatChildren(context, cell)

      cell(SPAN) match {
        case Some(span: Int) =>
          currentCol += span - 1
          s"\\multicolumn{$span}{l}{$content}"
        case _ => columnWidth(currentCol) match {
          case Some(width: String) => "\\parbox[t]{" + handlePercent(width) + "}{" + content + "}"
          case _ => content
        }
      }
    }

    val s = new StringBuilder
    var header = false
    while (currentCol <= cols) {
      segment(Attribute[Segment](row + "," + currentCol)) match {
        case Some(cell: Segment) =>
          cell(HEADER) match {
            case Some(true) => header = true
            case _ =>
          }
          s.append(doCell(cell))
        case _ =>
      }
      s.append(doEndOfRow(currentCol == cols, header))
      currentCol += 1
    }
    s
  }

  private def doEndOfRow(last: Boolean, header: Boolean) = {
    if (last) {
      "\\tabularnewline " + (if (header) "\\midrule \\endhead" else "") + "\n"
    } else {
      "&"
    }
  }
}
