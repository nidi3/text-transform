package org.mimacom.text.transform.format

import org.mimacom.text.transform.Segment

trait ResourceLoader {
  def loadResource(source: Segment, name: String): Option[String]
}
