---
layout: post
title: "Priority Queues - Related Questions"
description: ""
categories: [algorithm]
---

> Nothing special here. It's just a blog post for summarising my algorithm learning course. Here are
> some questions related to Priority Queues.

Related knowledge: [Binary Heap & Heapsort Summary - Part 1 - Binary Heap]({% post_url 2018-06-07-binary-heap-heapsort-summary-part-1 %})

# 1. Dynamic median

Design a data type that supports insert in logarithmic time, find-the-median in constant time, and
remove-the-median in logarithmic time. If the number of keys in the data type is even, find/remove
the lower median.

**Solution**: Use 2 Sorted Binary Heap
- The Max heap to store half smaller items
  - No items in the Max heap are bigger than the ones in the Mean heap
- The Min heap to store the other half
  - No items in the Min heap are smaller than the ones in the Max heap
- The 2 heap should be equal. That means, the number of item in each heap should be equal or maximum
  1 item different than the other one.

You will need these methods

```java
class Median {
    int[] maxHeap;
    int[] minHeap;

    void balance() {...}
    void insert(int) {...}
    int findMedian() {...}
    int removeMedian() {...}
}
```

**Find median**
- Depending on which heap containing more item, it will be the max or the mean, always the first
  item in the array.

**Balance**
- Depending on which heap having more items, remove the max/min from that heap
  - If `maxHeap` has more items, remove the max from that heap
  - Otherwise, remove the mean from `minHeap`
- Insert that item to the other heap
- Repeat until they are balanced
- Complexity: `logN`

**Insert**
- If the item is smaller than the median, insert to the `maxHeap`
- Otherwise, insert to the `minHeap`
- Balance the 2 heaps
- Complexity: `logN` for insert and `logN` for balance

**Remove**
- Find the median
- Remove from the corresponding heap
- Balance
- Complexity: `logN` for remove and `logN` for balance

# 2. Randomized priority queue

Describe how to add the methods `sample()` and
`delRandom()` to our binary heap implementation. The two methods return
a key that is chosen uniformly at random among the remaining keys, with the latter method also
removing that key. The `sample()` method should take constant time; the
`delRandom()` method should take logarithmic time. Do not worry about
resizing the underlying array.

- For `sample()`, simply pick a random item from the underlying array of the heap
- For `delRandom()`, instead of swapping the max item with the last item
  - swap a random item with the last item
  - remove the random item (currently the last item)
  - swim the last item (which is currently in the random position)

# 3. Taxicab numbers

A taxicab number is an integer that can be expressed as the sum of two cubes of positive integers in
two different ways: <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>a</mi><mn>3</mn></msup><mo>+</mo><msup><mi>b</mi><mn>3</mn></msup><mo>=</mo><msup><mi>c</mi><mn>3</mn></msup><mo>+</mo><msup><mi>d</mi><mn>3</mn></msup></math>.
For example, <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>1729</mi></math> is the
smallest taxicab number: <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>9</mi><mn>3</mn></msup><mo>+</mo><msup><mi>10</mi><mn>3</mn></msup><mo>=</mo><msup><mi>1</mi><mn>3</mn></msup><mo>+</mo><msup><mi>12</mi><mn>3</mn></msup></math>.

Design an algorithm to find all taxicab numbers with `a`, `b`, `c` and `d` less than `n`.

**Version 1**: Use time proportional to <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>n</mi><mn>2</mn></msup><mi>log</mi><mi>n</mi></math>
and space proportional to <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>n</mi><mn>2</mn></msup></math>.

- Create a matrix like this. Each cell is the sum of the cubes of row and column value <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>m[i][j]</mi><mo>=</mo><msup><mi>i</mi><mn>3</mn></msup><mo>+</mo><msup><mi>j</mi><mn>3</mn></msup></math>.
- Sort all the values in the matrix, highest to lowest, using any logarithmic time algorithm, a
  binary sorted heap in this case
- Pop out the items from the heap until the end
- If exists any 4 continuous equal values, that's the taxicab number

```
  | 0    1    2    3    4    5
--+-------------------------------
0 | 0    1    8    27   64   125
1 | 1    2    9    28   65   126
2 | 8    9    16   35   72   133
3 | 27   28   35   54   91   152
4 | 64   65   72   91   128  189
5 | 125  126  133  152  189  250
```

**Version 2**: Use time proportional to <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>n</mi><mn>2</mn></msup><mi>log</mi><mi>n</mi></math>
and space proportional to <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>n</mi></math>.

Actually, I couldn't think of the solution for this. I checked the answers on Stackoverflow and on
the internet, took me too long to understand...

Start with the above matrix. However, since half of the matrix is duplicated, we only need to care
about the other half. Of course, the matrix will never be created to save memory

```
  | 0    1    2    3    4    5
--+-------------------------------
0 | 0    1    8    27   64   125
1 |      2    9    28   65   126
2 |           16   35   72   133
3 |                54   91   152
4 |                     128  189
5 |                          250
```

Create a min heap and store the diagonal values. You can also use the max heap, simply do the
reverse way.

Pop out the min value from the heap. For each value, if
the current min equal to the previous min, we found the taxicab pair. Otherwise, add the value to
the right of the current min to the heap and repeat there is no value left in the heap.

To illustrate it
- Starting heap `2 16 54 128 250` (`m[1,1],m[2,2],m[3,3],m[4,4],m[5,5]`)
- Loop
  - Current heap `2 16 54 128 250`
  - prevMin: `0`
  - currMin: `2` (`m[1,1]`)
  - Add `9` (`m[2,1]`)
- Loop
  - Current heap `9 16 54 128 250`
  - prevMin: `2`
  - currMin: `9` (`m[2,1]`)
  - Add `28` (`m[3,1]`)
- Loop
  - Current heap `16 28 54 128 250`
  - prevMin: `9`
  - currMin: `16` (`m[2,2]`)
  - Add `35` (`m[3,2]`)
- Loop
  - Current heap `28 35 54 128 250`
  - prevMin: `16`
  - currMin: `28` (`m[3,1]`)
  - Add `65` (`m[4,1]`)
- Loop
  - Current heap `35 54 65 128 250`
  - prevMin: `28`
  - currMin: `35` (`m[3,2]`)
  - Add `72` (`m[4,2]`)
- ...
