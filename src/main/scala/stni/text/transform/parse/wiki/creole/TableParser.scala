package stni.text.transform.parse.wiki.creole

import stni.text.transform.{Attribute, Segment}
import stni.text.transform.Name._
import stni.text.transform.Attribute._
import stni.text.transform.AttributeValue._
import stni.text.transform.Segment._
import stni.text.transform.parse.TagCustomizerParser


/**
 * Parses a table element.
 */
class TableParser(parser: CreoleWikiParser) {
  private val CUSTOMIZER_COLSPAN = "colspan"
  private val CUSTOMIZER_WIDTH = "width"
  private val CUSTOMIZER_ALIGN_CELL = "align-cell"
  private val CUSTOMIZER_ALIGN = "align"
  private val CUSTOMIZER_NONFLOAT = "nonfloat"

  private val table = TABLE()
  private var rows = 0
  private var columns = 0
  private var targetColumn = 0
  private var cellContent: String = null
  private var cell: Segment = null


  def parseTable() {
    parser.addToResult(table)
    parseRows(parser)
    parseTableOptions()
  }

  private def parseRows(parser: CreoleWikiParser) {
    do {
      rows += 1
      val row = parser.readUntil("\n")
      parseRow(row.split("\\|"))
    } while (parser.currentChar == '|')
  }

  private def parseRow(cols: Seq[String]) {
    targetColumn = 1
    for (i <- 1 until cols.size) {
      cellContent = cols(i)
      parseCell()
    }
    if (targetColumn - 1 > columns) {
      columns = targetColumn - 1
    }
  }

  private def parseCell() {
    cell = TABLE_CELL()
    handleCellCustomizers()
    handleHeader()
    handleContent()
    handleSpan()
  }

  private def handleCellCustomizers() {
    cellContent =
      TagCustomizerParser(cellContent, (name, value) => name match {
        case CUSTOMIZER_COLSPAN =>
          val span = Integer.parseInt(value)
          if (span > 1) {
            cell(SPAN -> span)
          }
        case CUSTOMIZER_WIDTH => table(WIDTH(targetColumn) -> value)
        case CUSTOMIZER_ALIGN => table(ALIGN(targetColumn) -> leftOrRight(value))
        case CUSTOMIZER_ALIGN_CELL => cell(ALIGN -> leftOrRight(value))
      }).trim
  }

  private def handleHeader() {
    if (cellContent.length() > 0 && cellContent.charAt(0) == '=') {
      cell(HEADER -> true)
      cellContent = cellContent.substring(1)
    }
  }

  private def handleContent() {
    if (cellContent.length() > 0 || !cell.attributes.isEmpty) {
      cell(parser.parseSub(cellContent).children: _*)
      table(Attribute(rows + "," + targetColumn) -> cell)
    }
  }

  private def handleSpan() {
    cell(SPAN) match {
      case None => targetColumn += 1
      case Some(i: Int) => targetColumn += i
      case _ => throw new RuntimeException("Non integer value for SPAN")
    }
  }

  private def parseTableOptions() {
    table(FLOAT -> true, ROWS -> rows, COLUMNS -> columns)

    if (parser.currentChar == '!') {
      val options = parser.readUntil("\n").substring(1)
      val caption = handleTableCustomizers(options)
      if (caption.length > 0) {
        table(CAPTION -> caption)
      }
    }
  }

  private def handleTableCustomizers(options: String): String = {
    TagCustomizerParser(options, (name, value) => name match {
      case CUSTOMIZER_NONFLOAT => table(FLOAT -> false)
      case _ => //TODO warn
    }).trim
  }
}
