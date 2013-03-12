package org.mimacom.text.transform.format.latex

import org.mimacom.text.transform.{Attribute, Segment}
import org.mimacom.text.transform.Attribute._
import org.mimacom.text.transform.AttributeValue._
import org.mimacom.text.transform.format.latex.LatexFormatter.env

/**
 * Formats a table element.
 */
object TableFormatter {
  def format(context: Context, segment: Segment) = new TableFormatter(context, segment).format
}

class TableFormatter(context: Context, segment: Segment) {
  val rows = segment(ROWS).get.asInstanceOf[Int]
  val cols = segment(COLUMNS).get.asInstanceOf[Int]
  private var currentCol = 0

  def format: String = {
    val caption = segment(CAPTION).asInstanceOf[Option[Segment]]
    segment(FLOAT) match {
      case Some(true) => transformFloatEnvironment(caption)
      case _ => transformNonfloatEnvironment(caption)
    }
  }

  private def transformFloatEnvironment(caption: Option[Segment]) = {
    env("table") {
      "[hpt] \\centering\n" +
        transformTable +
        transformFloatCaption(caption)
    }
  }

  private def transformFloatCaption(caption: Option[Segment]) = {
    caption match {
      case Some(segment) =>
        val desc = LatexFormatter.format(context, segment)
        s"\n\\caption{$desc} \\label{table:$desc}"
      case _ => ""
    }
  }

  private def transformNonfloatEnvironment(caption: Option[Segment]) = {
    env("center") {
      transformTable + tranformNonfloatCaption(caption)
    }
  }

  private def tranformNonfloatCaption(caption: Option[Segment]) = {
    caption match {
      case Some(segment) =>
        val desc = LatexFormatter.format(context, segment)
        s"\n\\captionof{table}{$desc} \\label{table:$desc}"
      case _ => ""
    }
  }

  private def transformTable = {
    env("tabular") {
      val s = new StringBuilder("{" + columnsStyle + "}\n")
      for (row <- 1 to rows) {
        s.append(transformRow(row))
      }
      s.toString
    }
  }

  private def columnsStyle = {
    val s = new StringBuilder
    for (i <- 1 to cols) {
      s.append(
        segment(WIDTH.index(i)) match {
          case Some(width) => "p{" + width + "} "
          case _ => "l "
        })
    }
    s
  }

  private def transformRow(row: Int) = {
    val s = new StringBuilder
    var header = false
    currentCol = 1
    while (currentCol <= cols) {
      segment(Attribute(row + "," + currentCol)) match {
        case Some(cell: Segment) =>
          cell(HEADER) match {
            case Some(true) => header = true
            case _ =>
          }
          s.append(transformCell(cell))
        case _ =>
      }
      s.append(transformEndOfRow(currentCol == cols, header))
      currentCol += 1
    }
    s
  }

  private def transformCell(cell: Segment) = {
    val res = new StringBuilder
    res.append(
      cell(ALIGN) match {
        case Some(LEFT) => "\\raggedright "
        case Some(RIGHT) => "\\raggedleft "
        case _ => ""
      })
    res.append(LatexFormatter.formatChildren(context, cell))

    cell(SPAN) match {
      case Some(span: Int) =>
        currentCol += span - 1
        s"\\multicolumn{$span}{l}{$res}"
      case _ => res
    }
  }

  private def transformEndOfRow(last: Boolean, header: Boolean) = {
    if (last) {
      "\\tabularnewline " + (if (header) "\\hline" else "") + "\n"
    } else {
      "&"
    }
  }
}
