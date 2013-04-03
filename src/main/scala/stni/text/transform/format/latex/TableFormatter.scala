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
    val caption = segment(CAPTION).asInstanceOf[Option[Segment]]
    segment(FLOAT) match {
      case Some(true) => float(caption)
      case _ => nonfloat(caption)
    }
  }

  private def caption(cmd: String, caption: Option[Segment]) = {
    caption match {
      case Some(seg) =>
        val desc = LatexFormatter.format(context, seg)
        s"\n\\$cmd{$desc} \\label{table:$desc}"
      case _ => ""
    }
  }

  private def float(cap: Option[Segment]) = {
    env("table") {
      "[hpt] \\centering\n" +
        table + caption("caption", cap)
    }
  }

  private def nonfloat(cap: Option[Segment]) = {
    env("center") {
      table + caption("captionof{table}", cap)
    }
  }

  private def table = {
    env("longtable") {
      s"{$columnsStyle}\n" + (1 to rows).map(row => doRow(row)).mkString
    }
  }

  private def columnsStyle = {
    val noWidth = (1 to cols).forall(col => segment(WIDTH(col)).isEmpty)
    if (noWidth) {
      ("p{" + (.9 / cols) + " \\textwidth} ") * cols
    } else {
      (1 to cols).map(col => segment(WIDTH(col)) match {
        case Some(width) => "p{" + width + "} "
        case _ => "l "
      }).mkString
    }
  }

  private def doRow(row: Int) = {
    var currentCol = 1

    def doCell(cell: Segment) = {
      val content =
        (cell(ALIGN) match {
          case Some(LEFT) => "\\raggedright "
          case Some(RIGHT) => "\\raggedleft "
          case _ => ""
        }) + LatexFormatter.formatChildren(context, cell)

      cell(SPAN) match {
        case Some(span: Int) =>
          currentCol += span - 1
          s"\\multicolumn{$span}{l}{$content}"
        case _ => content
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
