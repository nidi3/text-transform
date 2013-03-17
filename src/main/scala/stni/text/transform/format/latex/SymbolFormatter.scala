package stni.text.transform.format.latex

import stni.text.transform.{AttributeValue, Segment}
import stni.text.transform.Attribute._
import stni.text.transform.AttributeValue._
import scala.Some

/**
 * Formats an image element.
 */
object SymbolFormatter {

  val symbols = Map(
    ARROW_LEFT -> "$\\leftarrow$ ",
    ARROW_RIGHT -> "$\\rightarrow$ ",
    ARROW_BOTH -> "$\\leftrightarrow$ ",
    DOUBLE_ARROW_LEFT -> "$\\Leftarrow$ ",
    DOUBLE_ARROW_RIGHT -> "$\\Rightarrow$ ",
    DOUBLE_ARROW_BOTH -> "$\\Leftrightarrow$ "
  )

  def format(context: Context, segment: Segment) =
    symbols.get(segment(TYPE).get.asInstanceOf[AttributeValue]) match {
      case None => segment(ORIGINAL).get.asInstanceOf[String]
      case Some(s) => s
    }
}

