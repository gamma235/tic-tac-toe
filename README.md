wish-list: 

[1] GUI or browser UI with start screen 
[2] midis
[3] full type checking
[4] easy mode and hard mode
[5] complete decoupling of record from helper logic

known bugs:

[1] sometimes computer thinks it is blocking a human from forking, when it actually isn't. 
[2] using a lot of '(if (< 1 (count coll)) do-this)' type branching. Get's the job done, but may cause trouble in edge cases
[3] using a lot of '(take (first (get-best-moves)))' type code, to just get a good move and take it. Imprecise and leads to suboptimal game-play
