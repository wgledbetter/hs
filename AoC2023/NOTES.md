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
