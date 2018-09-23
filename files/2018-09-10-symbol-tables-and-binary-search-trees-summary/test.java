public void delete(Key key) {
    root = delete(root, key);
}

// delete method returns the new tree after deletion
private Node delete(Node x, Key key) {
    if (x == null) return null;

    // search for key by traserving left/right
    int cmp = key.compareTo(x.key);
    if (cmp < 0)
        // set the subtree to the new tree after deletion
        x.left  = delete(x.left,  key);
    else if (cmp > 0)
        // set the subtree to the new tree after deletion
        x.right = delete(x.right, key);

    else {
        // case 0 and case 1, return null or the left/right subtree to
        // update the link in parent node
        if (x.right == null) return x.left;
        if (x.left  == null) return x.right;

        // case 2, replace with successor
        Node t = x;
        x = min(t.right);
        x.right = deleteMin(t.right);
        x.left = t.left;
    }

    // update subtree counts
    x.count = size(x.left) + size(x.right) + 1;
    return x;
}
