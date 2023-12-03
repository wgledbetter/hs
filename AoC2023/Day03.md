# Day 3

First attempt: $546887$. Too large.

Processing the input file, there aren't any symbols types I wasn't detecting.
And I'm finding the correct number of symbols.
I'm also finding the correct number of numbers.

Oh, and like Day 01, it gives the right answer on the sample input.

It's a problem with the boundary. In the input file, the first number, `232` is registered as a part number, but it shouldn't be.
No, it's not a boundary issue. Well, not in the way you thought it was. The above line was saying it was a vertical issue, but really it was a horizontal issue. The valid adjacency region was one too long.

New candidate: $540212$. Not submitted yet. Want to verify. Let me clean git up real quick.
Yes!
