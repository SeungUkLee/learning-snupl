let val p = (1, (1, (fn x => (fn y => x + y + 1), true))) in
  p
end

(* (int, (int, ((int -> (int -> int)), bool))) *)

