type table = { popen : int; pclose : int }

let parse_input k =
	match k with
		"L" -> {popen=0; pclose=1}
	| "M" -> {popen=1; pclose=0}
  |  _ -> {popen=0;pclose=0}
	end

let div_safe x y =
  if y == 0 then 0 else x / y

let node count () = n where
	rec n = 0 -> pre n + 1

let node sum i = n where
	rec n = 0 -> pre n + i

let node hold ydef c x = y where
  rec y = merge c x ((ydef -> pre y) whenot c)

(* This almost works, but something is wrong... there's an extra delay somewhere *)
let node more input =
 let clock gt = (input.popen > input.pclose)
 in hold 0 gt (count () when gt)

let node less input =
 let clock lt = (input.popen < input.pclose)
 in hold 0 lt (count () when lt)

let node mean input =
 let clock gt = (input.popen > input.pclose)
 in let m = (div_safe (sum input.popen) (count ()) when gt)
 in hold 0 gt m

let node main () =
  let line = read_line () in
	let i = parse_input line in
	let imore = more i in
	let iless = less i in
	let imean = mean i in
	let icount = count () in
	  print_int imore; 
    print_string " ";
	  print_int iless; 
    print_string " ";
	  print_int imean;
    print_string " ";
		print_int icount
