Treaps in Scala (just for the hell of it)
=========================================

A recent assignmenet for my son's freshman Data Structures class was
to implement the [Treap](https://en.wikipedia.org/wiki/Treap) data
structure.  This was a new data structure to me and it looked
interesting so I decided to do my own implementation in Scala.  For me
this was a intended as a [CodeKata](http://codekata.com/) type of
thing; I did this simply for the exercise of doing it.

[Note: I did not share any of this code with my son or make it public until after the assignment was due.
Besides, the poor kids had to do their implementations in Java so I doubt this code would be of much
use to them. Maybe I'll it try Haskell next.]

In addition just playing around with the Treap data structure I was
interested in experimenting with a few additional things. Namely:

1. Variance:  My implementation is covariant in both the key and value types
2. Property based tests:  Which ended up catching a silly bug that eluded my ad hoc testing
3. Micro-benchmarking:  I'm still trying to figure out how to best use Scalameter
4. Implicit ordering (and implicits in general)


Treaps
======

A Treap is a key/value store that provides efficient insertion and
searching. A Treap combines two different data structures, namely, a
Binary Search Tree and a Heap.  Get it? Tree. Heap. Treap. Yeah,
naming things is hard. The Treaps for my son's assignment were just
key stores (no values) but that just seems silly, even for a Code Kata
exercise. Also, they were supposed to return an error if a key was
added multiple times. My implmentation uses 'upsert' semantics so
adding a key multiple times simply updates the value for the existing
key.

The idea behind a Treap is that each node is assigned a random
priority value.  At first glance it seems odd to select a priority at
random, but we'll see shortly how it's used.

When a node is added to a Treap it is first inserted like a regular
Binary Search Tree (BST). Assuming the key did not already exist in
the Treap (the only interesting case) then the new node will end up as
a leaf node in its appropriate location in the BST based on its
key. However, since the priority was assigned randomly it is almost
certain that resulting tree is not in proper Heap ordering based on
the priority.

Once the new node has been inserted into the BST we need to move it up
the tree into a position that restores Heap ordering. This relocation
is accomplished using combinations of two different rotation
operations. We can rotate left around a node or rotate right around
it. A right rotation means that the node's left child will become the
new root and the current node will become this new root's right
child. The new root's previous right child becomes the new left child
of the node that got pushed down. A left rotation works similarly but
going in the opposite direction. This sounds complicated in words but
it's pretty clear in pictures.  Take a look at the picture in
[this](http://pavpanchekha.com/blog/treap.html) article for a good
example.

As a result of a rotation a node is pushed one level down in the tree
and one of its children is moved up one level. The magic of this
operation is that it also preserves the BST ordering. This means we
can use successive left and right rotations to move the newly added
node up the Treap until it reaches its appropriate position based on
Heap ordering of the priorities.

Why do this? Here's where the random priorties come in. Re-ordering
the Treap based on random priority values has the result of
_balancing_ the BST. The results after all the rotations is still a
valid BST but the overall struture is now balanced. The depth of
balanced Treap is logarithmic in number of elements. This means that
searches (and insertions) can be performend in O(log n) time.

Deletions can be accomplished by using the rotation operations to move
the node to be deleted down the Treap until it is a leaf node where it
can simply be removed.

Implementation
==============

My implementation provides an immutable Treap. Each insertion or
deletion returns a new Treap and the previous Treap remains available
unchanged. As much of the previous Treap as possible is re-used by the
new Treap so that storage is used as efficiently as
possible. Insertion/deletion creates something on the order of log n
new nodes.

A Treap is made up of nodes. There four node types: LeafNode,
LeftNode, RightNode, FullNode. A LeafNode is a node with no
children. A LeftNode has a left child but no right child. A RightNode
has a right child but no left child. A FullNode has, you guessed it,
both a left and a right child. There is also unique EmptyTreap to
represent a Treap with no values.

The Treap expects to find an implicit Ordering for the key type and
uses this to perform the BST operations. I wrote some wrapper classes
around the Scala Ordering stuff to provide what seems like a cleaner
interface. Scala follows Java's example (which followed C's example)
and uses 32-bit integer values to represent the three possible
comparison results (LT, GT, EQ). That's 2^32 unique values to
represent 3 unique results. This is why we can't have nice things.

The priority generator is also accepted as an implicit argument. I did
this for experimentation and it's basically worthless. I realized that
priority generator that always returned a constant value would
degenerate the Treap into a normal unbalanced BST. I thought I could
do some performance comparisons of the unbalanced trees vs the
balanced Treaps. However, even moderately sized unbalanced BSTs blew
out the stack during the performance tests. I considered that
sufficient demonstration of the benefits of a Treap and gave up that
line of experimentation.

My son's assignment recommended against using a recursive algorithm
and recommended implementing a stack to keep track of the current path
through the Treap. I admit that I don't understand this at all. Not
using a recursive algorithm on a recursive data structure seems silly
and why create your own stack when the call stack can give you
everything you need. I guess that's why I'm not a professor.

Performance
===========

I was interested in experimenting with Scalameter for benchmarking the
implementation.

This chart show the results. The logarithmic performance profile is
apparent for both insertions and searches.

![Treap performance](http://raw.githubusercontent.com/marcsaegesser/treap/master/charts/PerfChart.png)

The blue lines show the results of insertions. For each test a random
Treap of some size was generated (using Integer keys and values) and
then the performance test measured the time required to add 10,000 new
elements to the Treap. The size of the heap is indicated on the X
axis.

The orange lines show the results of searches. Again, random treaps
were generated and 10,000 searches were performed for keys that were
known to exist in the Treap.



