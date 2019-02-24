(* Clocks for filters *)
type table = { popen : int; pclose : int }

let div x y =
  if y == 0 then 0 else x / y

let node sum i = s where
	rec s = i -> pre s + i

let node count () = sum 1

let node hold ydef c x = y where
  rec y = merge c x ((ydef -> pre y) whenot c)

let node more input eject =
 let clock gt = (input.popen > input.pclose)
 in (hold 0 gt (count (() when gt))) when eject

let node less input eject =
 let clock lt = (input.popen < input.pclose)
 in (hold 0 lt (count (() when lt))) when eject

let node mean input eject =
 let clock gt = (input.popen > input.pclose)
 in let s = sum (input.popen when gt)
 in let c = count (() when gt)
 in let s' = hold 0 gt s
 in let c' = hold 0 gt c
 in let m = div (s' when eject) (c' when eject)
 in m

let node all_queries input eject =
  (more input eject, less input eject, mean input eject)

let parse_input k =
	match k with
		"L" -> {popen=0; pclose=1}
	| "M" -> {popen=1; pclose=0}
  |  _ -> {popen=0;pclose=0}
	end

let node print_when c i =
  merge c (print_int i) (() whenot c)

let node main () =
  let line = read_line () in
	let i = parse_input line in
  let clock eject = true in
	let (imore,iless,imean) = all_queries i eject in
	let icount = count () in
    print_string line;
    print_string " ";
	  print_when eject imore; 
    print_string " ";
	  print_when eject iless; 
    print_string " ";
	  print_when eject imean;
    print_string " ";
		print_int icount

