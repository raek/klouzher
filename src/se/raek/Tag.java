package se.raek;

import clojure.lang.IMapEntry;

public class Tag implements IMapEntry {
    
    private Object key;
    private Object val;
    
    public Tag(Object key, Object val) {
        this.key = key;
        this.val = val;
    }
    
    public Object key() {
        return this.key;
    }
    
    public Object val() {
        return this.val;
    }
    
    public Object getKey() {
        return this.key;
    }
    
    public Object getValue() {
        return this.val;
    }
    
    public Object setValue(Object o) {
        throw new UnsupportedOperationException();
    }
    
    public String toString() {
        if (this.key == null)
            return "null";
        else
            return this.key.toString();
    }
    
}
