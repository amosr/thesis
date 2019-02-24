(* Simplified `direct' / stream of bools implementation, like impl0. *)
type table = { popen : int; pclose : int }

let parse_input k =
	match k with
		"L" -> {popen=0; pclose=1}
	| "L2" -> {popen=0; pclose=2}
	| "M" -> {popen=1; pclose=0}
	| "M2" -> {popen=2; pclose=0}
  |  _ -> {popen=0;pclose=0}
	end


let div x y =
  if y == 0 then 0 else x / y

let node sum i =
  let rec s = i -> pre s + i
  in s

let node count_when pred =
  sum (if pred then 1 else 0)

let node more input = count_when (input.popen > input.pclose)
let node less input = count_when (input.popen < input.pclose)
let node mean input =
  let s = sum (if input.popen > input.pclose then input.popen else 0)
	and c = count_when (input.popen > input.pclose)
  in div s c

let node main () =
  let line = read_line () in
	let i = parse_input line in
	let imore = more i in
	let iless = less i in
	let imean = mean i in
	let icount = count_when true in
	  print_int imore; 
    print_string " ";
	  print_int iless; 
    print_string " ";
	  print_int imean;
    print_string " ";
		print_int icount

