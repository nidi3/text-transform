package stni.text.transform.format.latex

import stni.text.transform.{TransformContext, Attribute, Segment}
import stni.text.transform.Attribute._
import stni.text.transform.AttributeValue._
import stni.text.transform.format.latex.LatexFormatter.env

/**
 * Formats a table element.
 */
object TableFormatter {
  def format(context: TransformContext, segment: Segment) = new TableFormatter(context, segment).format
}

class TableFormatter(context: TransformContext, segment: Segment) {
  val rows = segment(ROWS).get.asInstanceOf[Int]
  val cols = segment(COLUMNS).get.asInstanceOf[Int]

  def format: String = {
    val caption = LatexFormatter.formatCaption(context, segment)
    segment(FLOAT) match {
      case Some(true) => float(caption)
      case _ => nonfloat(caption)
    }
  }

  private def caption(cmd: String, caption: String) = {
    if (caption == "") ""
    else s"\n\\$cmd{$caption} \\label{table:$caption}"
  }

  private def float(cap: String) = {
    env("table", "[hpt]") {
      "\\centering\n" +
        table + caption("caption", cap)
    }
  }

  private def nonfloat(cap: String) = {
    env("center") {
      table + caption("captionof{table}", cap)
    }
  }

  private def table = {
    env("longtable", s"{$columnsStyle}") {
      (1 to rows).map(row => doRow(row)).mkString
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
      segment(Attribute(row + "," + currentCol)) match {
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
      "\\tabularnewline " + (if (header) "\\hline \\endhead" else "") + "\n"
    } else {
      "&"
    }
  }
}
