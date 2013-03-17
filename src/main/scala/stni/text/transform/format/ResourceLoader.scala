package stni.text.transform.format

import stni.text.transform.Segment

trait ResourceLoader {
  def loadResource(source: Segment, name: String): Option[String]
}
