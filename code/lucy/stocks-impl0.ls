(* Simple implementation of the following Icicle queries: *)
(*
table stocks { open : Int, close : Int }
query 
  more = filter open > close of count;
  less = filter open < close of count;
  mean = filter open > close of sum open / count;
*)
(* This implementation implements filtering as a stream of booleans, with no real abstraction over the aggregation to perform over the filtered stream *)

type table = { popen : int; pclose : int }

let parse_input k =
	match k with
		"A" -> {popen=0; pclose=1}
	| "B" -> {popen=1; pclose=0}
  |  _ -> {popen=0;pclose=0}
	end

let div_safe x y =
  if y == 0 then 0 else x / y

let node count_whenb pred = n where
	rec n = 0 -> pre n + if pred then 1 else 0

let node more input = count_whenb (input.popen > input.pclose)
let node less input = count_whenb (input.popen < input.pclose)
let node mean input = (div_safe s c) where
	rec s = 0 -> pre s +
				if input.popen > input.pclose then input.popen else 0
	and c = count_whenb (input.popen > input.pclose)

let node main () =
  let line = read_line () in
	let i = parse_input line in
	let imore = more i in
	let iless = less i in
	let imean = mean i in
	let icount = count_whenb true in
	  print_int imore; 
	  print_int iless; 
	  print_int imean;
		print_int icount


