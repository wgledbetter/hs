# Daily Notes

## Day 1

I've had two "solutions" to part 2, both of which validate on the test input, but one produced 54750 (too high) and the other produced 54702 (too low).

## Day 3

First attempt: $546887$. Too large.

Processing the input file, there aren't any symbols types I wasn't detecting.
And I'm finding the correct number of symbols.
I'm also finding the correct number of numbers.

Oh, and like Day 01, it gives the right answer on the sample input.

It's a problem with the boundary. In the input file, the first number, `232` is registered as a part number, but it shouldn't be.
No, it's not a boundary issue. Well, not in the way you thought it was. The above line was saying it was a vertical issue, but really it was a horizontal issue. The valid adjacency region was one too long.

New candidate: $540212$. Not submitted yet. Want to verify. Let me clean git up real quick.
Yes!

## Day 4

Sol 1 = 25571
Sol 2 = 8805731

## Day 5

Ans 1 = 322500873
Ans 2 = 108956227

## Day 6

Is it guaranteed that if you hold the button longer you will have gone farther at the end?

$d_f = t_h(t_f-t_h)$
where $d_f$ is the final distance, $t_h$ is the hold time,

Ans 1 = 1155175
Ans 2 = 35961505

The quadratic formulation (numStrats') is waaaaaaaaaay faster than my list-based approach, but it's high by 2. Probably a floating-point thing.

Yeah, just moved around where the `ceiling` and `floor` were placed.

## Day 07

Ans 1 = 250957639

### Puz 2

Attempt 1 = 251371468 (too low)

Ans = 251515496

ick...

## Day 08

I literally was just doing binary tree unfolding stuff last night in the Haskell Book.

Ans 1 = 21883

### Puz 2

Attempt 1 is sloooow.

I need to have a way to find loops in the traversal, get the frequency of them, and then extend them out until multiple ones line up.

When we're detecting looping, not only do we check if we're at the same node, we also have to see where we are in the LR instructions.
Only when the node _and_ instruction index match does that indicate a loop.

There are two parts to loops: pre-loop and loop.
Any occurrances pre-loop can not be said to repeat periodically, but once you inter the loop, the further indices of that occurance will be (pL + lC*lS + lO), where pL is the pre-loop size, lC is the current loop count, lS is the size of the loop, and lO is the occurence's offset within the loop.

New approach:
Don't compare list contents.
Generate one reference list and have use the generating equations for the others to determine candidacy.

#### GHCI Commands

```haskell
sb = Day08.parseInput day08Input
steps = fst sb
branches = snd sb
nodes = map fst branches
trees = map (\startNode -> unfoldTree startNode branches) $ startNodes nodes
loops = map (\t -> firstLoopOf steps t) trees
lOffs = map loopOffset loops
lLens = map loopLength loops
lIdxs = map (loopPattern (\s -> last s == 'Z')) loops
oos = map (\(lo, ll, (lip,lil)) -> lip ++ [lo + lc*ll + ili | lc <- [0..], ili <- lil]) $ zip3 lOffs lLens lIdxs
```

```haskell
sb = Day08.parseInput day08Input
steps = fst sb
branches = snd sb
nodes = map fst branches
trees = map (\startNode -> unfoldTree startNode branches) $ startNodes nodes
loops = map (\t -> firstLoopOf steps t) trees
lOffs = map loopOffset loops
lLens = map loopLength loops
lIdxs = map (loopPattern (\s -> last s == 'Z')) loops
eqs = map (\(lo, ll, (lip, lil)) -> [(x, 0) | x <- lip] ++ [(lo + x, ll) | x <- lil]) $ zip3 lOffs lLens lIdxs

```

ANSWER 2 = 12833235391111
WOOOOOOO!!!!!!!!!!!
Jesus.
I feel like that one really required some background in cyclic graphs or whatever the fuck it's called.
I'm confident my earlier attempts were logically correct; they were just computationally inefficient.
In the end, I don't think I relied on any assumptions about the input structure, which is nice.

Lastly, it feels good to read the haskell reddit post for today (_AFTER I SOLVED IT_) and see everyone else having frustrations with the same stuff.
Saw some people recognize the same thing I did that the input structure is a simple sub-case of the general problem and were thinking about LCM/GCD-based solutions.
I'm really happy with my algebra-based general solution though.
Sick!

## Day 09

Felt super easy compared to yesterday.

## Day 10

### Puz 2

It's tricky because it's not the area inside the curve, it's the nodes inside the curve.
I'm working on an incremental cross-product solution, but I'm not going to finish tonight.
Actually, I don't think that'll work.

New approach:

- Find sub-rectangles
- Calculate nodes within sub-rectangle
  - What's "within" includes the opposite border.
- Remove sub-square from path by connecting endpoints.

Square detection occurs by considering three sides.
If the first and third sides go in opposite directions, then you have a sub-square.
Even if one side is longer.

Nope.
The right solution is to scan left to right on each row and count crossings.

## Day 12

I'm going to write an inefficient approach and try to band-aid it by running it in parallel.
Success!

Yup, I knew part 2 was going to punish inefficient implementations.
I don't have a better solution right now.
I think the solution is to come up with a better mapping from a [Bool] permutation to an [Int] group for a given string.
And there are probably more efficient data structures I should be using rather than lists of bool.
I did initially think of making it an integer.
It's okay.
Maybe I'll come back to it later.

I can't even get an answer for the test input in a reasonable amount of time... heh...

If the last value in a string is a #, then
No
Maybe.
If the last value in a string is a #
No, definitely not.

## Day 13

Very similar to the leetcode palindrome example.

Approach:

- Find adjacent pairs in first row and column
- Check if remaining rows and columns also have pair
