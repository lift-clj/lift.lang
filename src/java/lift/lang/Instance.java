package lift.lang;

import clojure.lang.IHashEq;
import clojure.lang.IPersistentVector;
import clojure.lang.IType;
import clojure.lang.PersistentList;
import clojure.lang.PersistentVector;
import clojure.lang.Symbol;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class Instance implements IHashEq, IType, Tagged {

    private static HashMap<Symbol, List<Type>> projections =
        new HashMap<Symbol, List<Type>>();
    // should really be a Prim     ^^

    private int hq = 0;
    public final Type type;
    public final Symbol tag;
    public final List elems;

    public Instance(Type type, Symbol tag, List elems) {
        this.type = type;
        this.tag = tag;
        this.elems = elems;
    }

    public boolean equals(Object other) {
        if (other instanceof Instance) {
            Instance otherp = (Instance) other;
            return isa(otherp.tag) && elems.equals(otherp.elems);
        } else {
            return false;
        }
    }

    public int hasheq() {
        if (hq == 0) {
            PersistentList plist = (PersistentList) PersistentList.create(elems);
            hq = tag.hasheq() ^ plist.hasheq();
        }
        return hq;
    }

    public int hashCode() { return hasheq(); }

    public Boolean isa(Symbol tag) { return this.tag.equals(tag); }

    public Object nth(int n) { return elems.get(n); }

    public Type getType() { return type; }

    public static IPersistentVector getBasis() {
        Symbol[] fields = {
            Symbol.create(null, "type"),
            Symbol.create(null, "tag"),
            Symbol.create(null, "elems")
        };
        return PersistentVector.create(Arrays.asList(fields));
    }

    public static void resetProjections(Symbol tag) {
        projections.remove(tag);
    }

    public static void addProjection(Symbol tag, Type prim) {
        List<Type> prjs = projections.get(tag);
        if (prjs == null) {
            prjs = new ArrayList<Type>();
            projections.put(tag, prjs);
        }
        prjs.add(prim);
    }

    public static Type getProjection(Symbol tag, int i) {
        return projections.get(tag).get(i);
    }
}
