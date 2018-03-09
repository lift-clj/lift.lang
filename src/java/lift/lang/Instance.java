package lift.lang;

import clojure.lang.IHashEq;
import clojure.lang.PersistentList;
import clojure.lang.Symbol;
import java.util.Arrays;
import java.util.List;

public class Instance implements IHashEq {

    private int hq = 0;
    private final Type type;
    private final Symbol tag;
    private final Object[] elems;

    public Instance(Type type, Symbol tag, List elems) {
        this.type = type;
        this.tag = tag;
        this.elems = elems.toArray();
    }

    public boolean equals(Object other) {
        if (other instanceof Instance) {
            Instance otherp = (Instance) other;
            return isa(otherp.getTag())
                && getElems().equals(otherp.getElems());
        } else {
            return false;
        }
    }

    public int hasheq() {
        if (hq == 0) {
            PersistentList plist =
                (PersistentList) PersistentList.create(getElems());
            hq = tag.hasheq() ^ plist.hasheq();
        }
        return hq;
    }

    public int hashCode() { return hasheq(); }

    public Boolean isa(Symbol tag) { return this.tag.equals(tag); }

    public Object nth(int n) { return elems[n]; }

    public Type getType() { return type; }

    public Symbol getTag() { return tag; }

    public List getElems() { return Arrays.asList(elems); }
}
