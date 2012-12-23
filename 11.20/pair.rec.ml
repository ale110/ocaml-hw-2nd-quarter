(* ale110.incrowd.ws * WTFPL *)

let rec pair l =
  match l with
    | l1::l2::ls -> (l1, l2)::(pair ls)
    | _::[] -> failwith "pair"
    | [] -> [];;
