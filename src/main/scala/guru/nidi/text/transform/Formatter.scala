package guru.nidi.text.transform

trait Formatter {
  def format(segment: Segment): String
}