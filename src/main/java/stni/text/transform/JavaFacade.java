package stni.text.transform;

import scala.Option;
import scala.Some;

/**
 *
 */
public class JavaFacade {
    private JavaFacade() {
    }

    public static Attribute newAttribute(String name) {
        return Attribute$.MODULE$.apply(name);
    }

    public static Object getAttribute(Segment segment, String name) {
        return getAttribute(segment, newAttribute(name));
    }

    public static Object getAttribute(Segment segment, Attribute name) {
        Option<Object> val = segment.attributes().get(name);
        return val instanceof Some ? val.get() : null;
    }

    public static void setAttribute(Segment segment, String name, Object value) {
        setAttribute(segment, newAttribute(name), value);
    }

    public static void setAttribute(Segment segment, Attribute name, Object value) {
        segment.attributes().put(name, value);
    }
}
