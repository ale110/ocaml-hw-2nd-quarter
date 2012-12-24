(* ale110.incrowd.ws * WTFPL *)

let bal3_norm lst =
  let rec bal3_norm_inner l =
    match l with
      |  2:: 2::ls -> failwith "FIX THIS!"
      |  2::el::ls -> -1::(bal3_norm_inner ((el+1)::ls))
      | -2::-2::ls -> failwith "FIX THIS!"
      | -2::el::ls ->  1::(bal3_norm_inner ((el-1)::ls))
      | el::    ls -> el::(bal3_norm_inner          ls)
      | [] -> []
  in bal3_norm_inner (lst @ [0; 0; 0; 0]);;

let rec bal3_sum l1 l2 = 
  bal3_norm(
    match (l1, l2) with
      | (x::xs, y::ys) -> (x+y)::(bal3_sum xs ys)
      | (_, []) -> l1
      | ([], _) -> l2
  );;

let print_list f lst =
  print_string "[";
  List.iter (fun x -> (f x; print_string "; ")) lst;
  print_string "]";;

print_list (print_int) (bal3_norm [2; -1; 1; -1; 0; -1; 2; 0]);;
