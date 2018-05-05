---
layout: post
title: "Solutions to the Sorting with 1MB RAM computer problem"
description: ""
categories: []
tags: []
thumbnail:
---

> An interviewer once asked me this question. At first, I was a bit nervous and thought that this is
> a very tricky question. It took me a while to figure out the solution and turned out this can be
> converted to a simpler algorithm we already learned in University.

# Question

Given a computer with only 1MB of RAM and unlimited hard disk space, sort a file which contain 1
million 8-bit integers, each on one line.

# Solution

The problem with this is our computer is limited in memory. We cannot read all 1 million
integers at once into the memory. That means we can only read some small parts of the file, which
leads me to a `divide and conquer` solution like this. The basic idea is to divide the large problem
into smaller one, process each of them and then try to merge the result.

- First, read a number of integers that can fit into the memory. Actually, you should read
  the number of integers that can fit in the RAM/2 space (which is `512KB` in this case). The reason
  is in the next part
- Perform any sort algorithm that you can think of on that array. Because you need another
  array for storing the result, you can only read only the number of integers that can
  fit in half of your RAM.
  - In this case, each 8-bit integer is 2 bytes, so each time you can only retrieve maximum
  `512KB * 1024 / 2B = 262,144` integers.
- After the array is sorted, write them back to a file.
- Repeat the above steps until you finally process all the integers in the source file.
  - You will need to read 4 times in this case
- Now you have a collection of sorted integers files. The next step is to apply merge sort on those
  file to produce the final output file. The merge sort algorithm is like this
  - Read the 2 sorted files, line by line
  - Compare the 2 values
  - Compare which one is smaller, write to the output file
  - Repeat the above steps until we reach the end one 1 of the 2 files
  - Write back all the remaining values of the other files to the output file

# Pseudo code

<!-- more -->

I just give the pseudo code for merge sort here. That is enough to solve both problems including
sorting in memory and combining the 2 sorted files. You can also read more about merge sort on
[Wikipedia](https://en.wikipedia.org/wiki/Merge_sortURL).

```js
let val1, val2;

val1 = readLine(file1);
val2 = readLine(file2);

// repeat until we reach the end of both files
while (val1 || val2) {
  // if we reach to the end of file1,
  // write all the remaining values
  // of file2 to the outputFile
  if (!val1) {
    writeLine(val2, outputFile);
    val2 = readLine(file2);
    continue;
  }

  // if we reach to the end of file2
  // write all the remaining values
  // of file1 to the outputFile
  if (!val2) {
    writeLine(val1, outputFile);
    val1 = readLine(file1);
    continue;
  }

  // if there are 2 values
  // write the smaller one to
  // the output file and continue
  if (val1 < val2) {
    writeLine(val1, outputFile);
    val1 = readLine(file1);
  } else {
    writeLine(val2, outputFile);
    val2 = readLine(file2);
  }
};
```
