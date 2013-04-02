package stni.text.transform


trait ResourceLoader {
  def loadResource(source: Segment, name: String): Option[String]
}
