type table = { popen : int; pclose : int }

(* This works: *)
let node count_when pred = n where
	rec n = 0 -> pre n + 1 when pred

(* However, expressions can't be used as clocks, so this is no good: *)
let node less input = count_when (input.popen < input.pclose)

(* This neither: *)
let clock gt i = i.popen > i.pclose
let node more input = count_when (gt input)

