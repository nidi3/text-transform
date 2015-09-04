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
package guru.nidi.text.transform.parse.html

import guru.nidi.text.transform.Attribute._
import guru.nidi.text.transform.AttributeValue._
import guru.nidi.text.transform.Name._
import guru.nidi.text.transform.parse.TagCustomizerParser
import guru.nidi.text.transform.{Attribute, Segment}

import scala.xml.{Node, NodeSeq}

/**
 *
 */
class TableParser(parser: HtmlParser) {
  private val CUSTOMIZER_COLSPAN = "colspan"
  private val CUSTOMIZER_WIDTH = "width"
  private val CUSTOMIZER_ALIGN = "align"
  private val CUSTOMIZER_ALIGN_CELL = "align-cell"

  val table = TABLE()
  var rowIndex = 1
  var colIndex = 1
  var maxColumns = 1

  def parse(style: String, ns: NodeSeq, listLevel: Int): Segment = {
    if ((ns \\ "caption").nonEmpty) {
      table(CAPTION -> ROOT(parser.parse((ns \\ "caption")(0).child, listLevel): _*))
    }
    for (row <- ns \ "tr") {
      parseRow(row, listLevel)
    }

    setWidths(style)
    table(ROWS -> (rowIndex - 1), COLUMNS -> (maxColumns - 1))
    table
  }

  private def parseRow(row: Node, listLevel: Int) {
    colIndex = 1
    for (col <- row.child) {
      parseCell(col, listLevel)
    }
    maxColumns = Math.max(maxColumns, colIndex)
    rowIndex += 1
  }

  private def parseCell(col: Node, listLevel: Int) {
    if (col.label == "td" || col.label == "th") {
      table(Attribute(rowIndex + "," + colIndex) -> cell(col, colIndex, listLevel))
      colIndex += 1
    }
  }

  private def cell(col: Node, index: Int, listLevel: Int) = {
    val cell = TABLE_CELL()
    val content = parser.parse(col.child, listLevel)
    if (content.nonEmpty && content.head.name == PLAIN) handleTagCustomizer(content.head, index, cell)
    if ((col \ "@style").nonEmpty) handleStyle((col \ "@style").text, index, cell)
    handleColspan(col, cell)
    if (col.label == "th" || (col \ "@class").text == "highlight") cell(HEADER -> true)
    parser.trimNewlines(cell(content: _*))
  }

  def handleColspan(col: Node, cell: Segment) {
    try {
      val span = Integer.parseInt((col \ "@colspan").text)
      if (span > 1) cell(SPAN -> span)
      colIndex += span - 1
    } catch {
      case e: NumberFormatException =>
    }
  }

  private def handleTagCustomizer(content: Segment, index: Int, cell: Segment) {
    content(TEXT -> TagCustomizerParser(content(TEXT).get, (name, value) => name match {
      case CUSTOMIZER_WIDTH => table(WIDTH(index) -> value)
      case CUSTOMIZER_ALIGN => table(ALIGN(index) -> leftOrRight(value))
      case CUSTOMIZER_ALIGN_CELL => cell(ALIGN -> leftOrRight(value))
      case _ =>
    }).trim)
  }

  private def handleStyle(style: String, index: Int, cell: Segment) {
    CssParser(style, (name, value) => name match {
      case "width" => table(WIDTH(index) -> value)
      case _ =>
    })
  }

  private def setWidths(style: String) {
    if (!setWidthsFromStyle(style)) setWidthsFromCells()
  }

  private def setWidthsFromStyle(style: String): Boolean = {
    var hasWidthStyle = false
    def setWidths(value: String) {
      hasWidthStyle = true
      value.split(",").zipWithIndex.foreach({
        case (width, index) => table(WIDTH(index + 1) -> width)
      })
    }

    CssParser(style, (name, value) => name match {
      case CUSTOMIZER_WIDTH => setWidths(value)
      case _ =>
    })

    hasWidthStyle
  }

  private def setWidthsFromCells() {
    def isPx(col: Int) = table(WIDTH(col)).getOrElse("").endsWith("px")
    def valuePx(col: Int) = {
      val s = table(WIDTH(col)).get
      try {
        Integer.parseInt(s.substring(0, s.length - 2))
      } catch {
        case _: NumberFormatException => 0
      }
    }

    val allPxWidths = (1 until maxColumns).forall(col => isPx(col))
    if (allPxWidths) {
      val sum = (1 until maxColumns).foldLeft(0)((sum, col) => valuePx(col) + sum)
      (1 until maxColumns).foreach(col => table(WIDTH(col) -> (100.0 * valuePx(col) / sum + "%")))
    } else {
      (1 until maxColumns).foreach(col => if (isPx(col)) table(WIDTH(col) -> null))
    }
  }
}
