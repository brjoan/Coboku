# Coboku
Cobol implementation of Peter Norvig's sudoku solver.

It solves every sudoku that I've tried it on, including all those on his webpage in a comparable amount of time.

Because recursion is not supported in Cobol, a list with two indices was used in the Eliminate procedure. The main loop in this procedure runs over the elements of a list of elements to eliminate, and the loop itself updates this list with new possible values to remove in a future iteration.

Compiled with GnuCobol 2.0.
