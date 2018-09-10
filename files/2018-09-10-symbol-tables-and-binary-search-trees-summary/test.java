public class ST<Key extends Comparable<Key>, Value> {
    void put(Key key, Value val);
    Value get(Key key);
    void delete(Key key);
    boolean contains(Key key);
    boolean isEmpty();
    Key min();
    Key max();
    Key floor(Key key);
    Key ceiling(Key key);
    int rank(Key key); // number of keys less than key
    Key select(int k); // select key of rank k
    void deleteMin();
    void deleteMax();
    // range operations
    int size(); // size of the whole tree
    int size(Key lo, Key hi); // size of range
    Iterable<Key> keys(); // iterate through all keys
    Iterable<Key> keys(Key lo, Key hi); // iterate though keys in range
}
