let val f = (fn f => fn x => f x + x + 1) in 
  f 
end 

(* (int -> int) -> (int -> int) *)
