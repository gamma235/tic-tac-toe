WISH LIST:

[1] GUI or browser UI with start screen

[2] midis

[3] full type checking on record.clj file

[4] easy mode and hard mode

[5] complete decoupling of record from helper logic

_____________________________________________________________________________________________

KNOWN BUGS:

1. sometimes computer thinks it is blocking a human from forking, when it actually isn't.

2. using a lot of

    '(if (< 1 (count coll)) do-this)'

and

    '(take (first (get-best-moves)))'

type code, to just get a good move and take it. Gets the job done but is a temporary solution.
