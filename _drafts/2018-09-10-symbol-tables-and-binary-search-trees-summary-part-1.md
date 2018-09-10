---
layout: post
title: "Symbol Tables and Binary Search Trees summary - Part 1"
description: ""
categories: [algorithm]
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Although
> this was already taught in the University, it's still god to summarize here

# Symbol Tables

Key-value pair abstraction.
- Insert a value with specified key.
- Given a key, search for the corresponding value.

Example

|domain name|IP address|
|---|---|
|www.cs.princeton.edu|128.112.136.11|
|www.princeton.edu|128.112.128.15|
|www.yale.edu|130.132.143.21|
|www.harvard.edu|128.103.060.55|
|www.simpsons.com|209.052.165.60|
{: .table }

### Symbol Table APIs

Symbol Tables act as an associative array, associate one value with each key.

{% highlight java %}
public class ST<Key, Value> {
    void put(Key key, Value, val);
    Value get(Key key);
    void delete(Key key);
    boolean contains(Key key);
    boolean isEmpty();
    int size();
    Iterable<Key> keys();
}
{% endhighlight %}

# Ordered Symbol Tables

If we order the keys in Symbol table, we can support a much wider number of operations, for example
min, max operations and especially range operations.

```
| operations                    | keys     | values  |
|-------------------------------|----------|---------|
| min() ======================> | 09:00:00 | Chicago |
|                               | 09:00:03 | Phoenix |
| get(09:00:13) ==============> | 09:00:13 | Houston |
|                               | 09:00:59 | Chicago |
|                               | 09:01:10 | Houston |
| floor(09:05:00) ============> | 09:03:13 | Chicago |
|                               | 09:10:11 | Seattle |
| select(7) ==================> | 09:10:25 | Seattle |
|                               | 09:14:25 | Phoenix |
| ============================> | 09:19:32 | Chicago |
| ============================> | 09:19:46 | Chicago |
| keys(09:15:00, 09:25:00) ===> | 09:21:05 | Chicago |
| ============================> | 09:22:43 | Seattle |
| ============================> | 09:22:54 | Seattle |
|                               | 09:25:52 | Chicago |
| ceiling(09:30:00) ==========> | 09:35:21 | Chicago |
|                               | 09:36:14 | Seattle |
| max() ======================> | 09:37:44 | Phoenix |
| size(09:15:00, 09:25:00) = 5  |          |         |
| rank(09:10:25) = 7            |          |         |
```

### Ordered Symbol Table APIs

{% highlight java %}
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
{% endhighlight %}

# Binary Search Tress

Binary Search Tree is a binary tree in symmetric order. Each node has a key, and every node's key is
- Larger than all keys in its left subtree.
- Sgmaller than all keys in its right subtree

![BST](/files/2018-09-10-symbol-tables-and-binary-search-trees-summary/bst1.png)

Using BST, we can implement all the operations of Ordered Symbol Tables quite efficiently, as long
as keys are inserted in random order.

