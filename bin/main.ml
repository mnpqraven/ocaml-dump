(* let square x = x * x *)
(* let square_is_even x = square x mod 2 = 0 *)
(* let ordered a b c = a <= b && b <= c *)
let rec range a b = if a > b then [] else a :: range (a + 1) b
let digits = range 0 9
let () = List.iter print_int digits;;

print_endline ""

let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
let () = print_int (factorial 5)
let rec factorial_new = function 0 | 1 -> 1 | n -> n * factorial_new (n - 1)
let () = print_int (factorial_new 5);;

print_endline ""

(*prepend -1 to 0..9 *)
(* not mutating but returns new array, similar to array spreading *)
let new_digits = -1 :: digits
let () = List.iter print_int new_digits;;

print_endline ""

let head = List.hd new_digits
let tail = List.hd (List.rev new_digits);;

print_int head;;
print_int tail;;
print_endline ""

(* total sum of a list *)
let rec total_list = function [] -> 0 | head :: rest -> head + total_list rest

(* 10 * 4 + 5 *)
let sum = total_list [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
let () = print_int sum;;

print_endline ""

let rec length l = match l with [] -> 0 | _ :: rest -> 1 + length rest
let rec map fn ls = match ls with [] -> [] | h :: t -> fn h :: map fn t
let a = map total_list [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ] ]
let b = map (fun x -> x * 2) [ 1; 2; 3 ];;

print_endline ""

let add a b = a + b
let f = add 7
let a = f 8
let tuple = (1, "one", '1')

type role = User | Admin

type obj_lookalike = {
  name : string;
  age : int;
  rate : float;
  isAdmin : bool;
  role : role;
}

let user : obj_lookalike =
  { name = "naarst"; age = 20; rate = 2.0; isAdmin = true; role = User }

(* RGB is tuple struct *)
type colour = Red | Blue | Green | RGB of int * int * int

let colors : colour list = [ Blue; RGB (123, 20, 200) ]

(* binary tree graph *)
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

(* int is the type of 'a *)
let basic_tree : int tree = Node (Node (Leaf, 0, Leaf), 2, Node (Leaf, 0, Leaf))

(* get the total sum of values in a tree *)
let rec total_tree t =
  match t with Leaf -> 0 | Node (l, v, r) -> total_tree l + v + total_tree r

let try_find predicate list =
  try Some (List.find predicate list) with Not_found -> None

let try_find2 predicate list =
  match List.find predicate list with Not_found -> None | value -> Some value

let r = ref 0;;

r := 100

let swap a b =
  (* assign temp value *)
  let t = !a in
  (* reassign existing values *)
  a := !b;
  b := t
