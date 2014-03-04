package guru.nidi.text.transform

trait Parser {
  def parse(input: String): Segment
}