/*
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
package guru.nidi.text.transform;

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

    public static Object getInherited(Segment segment, String name) {
        return getInherited(segment, newAttribute(name));
    }

    public static Object getInherited(Segment segment, Attribute name) {
        Option<Object> val = segment.inherited(name);
        return val instanceof Some ? val.get() : null;
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
