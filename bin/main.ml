(* tail of a list *)
let last l = match l with [] -> None | _ -> Some (List.hd (List.rev l));;

(* - : string option = Some "d" *)
last [ "a"; "b"; "c"; "d" ];;

(* - : 'a option = None *)
last []

(* Last Two Elements of a List *)
let last_two l =
  match List.rev l with ult :: pen_ult :: _ -> Some (pen_ult, ult) | _ -> None

(* N'th Element of a List *)
let rec at l index =
  match l with
  | [] -> None
  | head :: rest -> if index = 0 then Some head else at rest (index - 1)

(* Length of a List *)
(* alias for *)
(* 1. declaring a recursive function *)
(* 2. running said recursive function with initial params *)
let length list =
  let rec runner l = function [] -> l | _ :: t -> runner (l + 1) t in
  runner 0 list

(* Reverse a List *)
(* let rev list =  let rec runner l = function *)
(*     in runner 0 l *)
