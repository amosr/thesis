(* The previous versions perform the eject function for every element of the input stream. For the "mean" function, this means they're performing division on every element, rather than just at the end of the stream. For more complex queries the eject function can require more work --- for example, queries with "group" operations often perform a map over the group values, and doing this on every iteration is wasteful. This version uses an 'eject' clock that determines when to evaluate the eject function to compute the query result. When to perform the clock restriction is subtle, and we need to restrict the arguments to the eject function to the eject clock; if we restricted the result of the eject function, the result would still be computed every element, but not emitted. *)
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

let node more input eject = count_when (input.popen > input.pclose) when eject
let node less input eject = count_when (input.popen < input.pclose) when eject
let node mean input eject =
  let s = sum (if input.popen > input.pclose then input.popen else 0)
	and c = count_when (input.popen > input.pclose)
  in div (s when eject) (c when eject)

let node all_queries input eject =
  (more input eject, less input eject, mean input eject)


let node print_when c i =
  merge c (print_int i) (() whenot c)

let node main () =
  let line = read_line () in
	let i = parse_input line in
  let clock eject = line = "E" in
	let (imore, iless, imean) = all_queries i eject in
	let icount = count_when true in
    print_string line;
    print_string " ";
	  print_when eject imore; 
    print_string " ";
	  print_when eject iless; 
    print_string " ";
	  print_when eject imean;
    print_string " ";
		print_int icount

