package stni.text.transform


import java.text.MessageFormat
import java.util.Locale
import java.util.ResourceBundle

/**
 *
 */
class Context(val headingLevel: Int, locale: Locale, resourceLoader: ResourceLoader) {
  val messages = ResourceBundle.getBundle(getClass.getPackage.getName + ".messages", locale)

  def message(key: String, parameters: AnyRef*) = MessageFormat.format(messages.getString(key), parameters.toArray: _*)

  def loadResource(source: Segment, name: String): Option[String] = resourceLoader.loadResource(source, name)

  def sub = new Context(headingLevel + 1, locale, resourceLoader)
}


