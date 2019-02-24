(* Clocks for filters *)
type table = { popen : int; pclose : int }

let div x y =
  if y == 0 then 0 else x / y

let node sum i = s where
	rec s = i -> pre s + i

let node count () = sum 1

let node hold ydef c x = y where
  rec y = merge c x ((ydef -> pre y) whenot c)

let node more input =
 let clock gt = (input.popen > input.pclose)
 in hold 0 gt (count (() when gt))

let node less input =
 let clock lt = (input.popen < input.pclose)
 in hold 0 lt (count (() when lt))

let node mean input =
 let clock gt = (input.popen > input.pclose)
 in let s = sum (input.popen when gt)
 in let c = count (() when gt)
 in let m = div s c
 in hold 0 gt m

let node all_queries input =
  (more input, less input, mean input)

let parse_input k =
	match k with
		"L" -> {popen=0; pclose=1}
	| "M" -> {popen=1; pclose=0}
  |  _ -> {popen=0;pclose=0}
	end

let node main () =
  let line = read_line () in
	let i = parse_input line in
	let (imore,iless,imean) = all_queries i in
	let icount = count () in
	  print_int imore; 
    print_string " ";
	  print_int iless; 
    print_string " ";
	  print_int imean;
    print_string " ";
		print_int icount
