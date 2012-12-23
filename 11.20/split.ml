(* ale110.incrowd.ws * WTFPL *)

let split l f =
  (
    (List.filter (f) l), 
    (List.filter (fun x -> not (f x)) l)
  );;
