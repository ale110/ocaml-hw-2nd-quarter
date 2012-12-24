(* ale110.incrowd.ws * WTFPL *)

(** Tools *)
let rec range a b =
  if a > b then []
  else a :: range (a+1) b;;

let (^^) x y =
  int_of_float
    ((float_of_int x) ** (float_of_int y));;

(** Normalise bal3 (replace 2's and -2's with normal values) *)
(* https://github.com/pooh110andco/ocaml-hw-2nd-quarter/issues/1 *)
let rec bal3_norm lst =
  let rec bal3_norm_inner l =
    match l with
      |  2:: 2::ls -> failwith "FIX THIS!"
      |  2::el::ls -> -1::(bal3_norm_inner ((el+1)::ls))
      | -2::-2::ls -> failwith "FIX THIS!"
      | -2::el::ls ->  1::(bal3_norm_inner ((el-1)::ls))
      | el::    ls -> el::(bal3_norm_inner          ls)
      | [] -> []
  in 
    if (List.exists
         (fun x -> (x == 2) || (x == -2))
         (bal3_norm_inner (lst @ [0])))
    then
      bal3_norm (bal3_norm_inner (lst @ [0]))
    else
      bal3_norm_inner (lst @ [0]);;

let bal3_cut_zeros l = l;; (* TODO: implement this *)

(** Add l1 to l2 *)
let rec bal3_sum l1 l2 = 
  bal3_norm(
    match (l1, l2) with
      | (x::xs, y::ys) -> (x+y)::(bal3_sum xs ys)
      | (_, []) -> l1
      | ([], _) -> l2
  );;

(** Convert integer to bal3 *)
(*  Did not test yet :( *)
let bal3_of_int n =
  let rec max_less_than f p q =
    if (f p) > q then p-1
    else max_less_than f (p+1) q
  in let bal3_of_pos p =
    (List.map (fun x-> 0) (range 1 (p-1))) @ [1]
  in let rec bal3_of_int_inner m =
    match m with
      |  0 -> []
      |  1 -> [1]
      | -1 -> [-1]
      |  _ -> bal3_sum
                (bal3_of_pos
                  (max_less_than (fun x -> x ^^ 3) 1 m)
                )
                (bal3_of_int_inner
                  (m - (max_less_than (fun x -> x ^^ 3) 1 m))
                )
  in bal3_of_int_inner n;;

(** DEBUG **)

let print_list f lst =
  print_string "[";
  List.iter (fun x -> (f x; print_string "; ")) lst;
  print_string "]";;

print_list (print_int) (bal3_cut_zeros (bal3_norm (bal3_of_int 26)));;
