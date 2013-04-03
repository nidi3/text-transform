package stni.text.transform

import stni.text.transform.Segment._
import stni.text.transform.Name._
import org.scalatest.FlatSpec


/**
 *
 */
class SegmentTest extends FlatSpec {
  behavior of "removeAll"

  it should "remove all elements matching the condition" in {
    assert(ROOT(plain("a"), BOLD(plain("b")), plain("c")).removeAll(_.name == BOLD) ===
      ROOT(plain("a"), plain("b"), plain("c")))
  }

  it should "remove all two following elements matching the condition" in {
    assert(ROOT(plain("a"), BOLD(plain("b")), BOLD(plain("b")), plain("c")).removeAll(_.name == BOLD) ===
      ROOT(plain("a"), plain("b"), plain("b"), plain("c")))
  }

  it should "remove all two following empty elements matching the condition" in {
    assert(ROOT(plain("a"), NEWLINE(), NEWLINE(), plain("c")).removeAll(_.name == NEWLINE) ===
      ROOT(plain("a"), plain("c")))
  }
}
