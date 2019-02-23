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


(*
table stocks { open : Int, close : Int }
query 
  more = filter open > close of count;
  less = filter open < close of count;
  mean = filter open > close of sum open / count;
*)

(*
let node count_from n0 = n where
 rec n = n0 -> pre n + 1

let node count_when pred = count_from 0 when pred
*)

(*
let node more0 input = c where
	rec c = 0 ->
			pre c +
			if input.popen > input.pclose
			then 1
			else 0

let node more1 opens closes = c where
	rec c = 0 ->
			pre c +
			if opens > closes
			then 1
			else 0
*)
(*
let node more0 input =
	   let pred = input.popen > input.pclose
	in let f = input when pred
  in let rec count = 0 -> (pre count + 1) when pred
	in count
*)

(*
let node more0 opens closes =
	   let pred = opens > closes
  in let rec count = 0 -> (pre count + 1) when pred
	in count
*)


(*
let node main () =
  let key = read_line () in
	let c = count_when (key =="A") in
	  print_int c 
*)

