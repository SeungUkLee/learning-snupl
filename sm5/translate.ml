(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 *)

open K
open Sm5

module Translator = struct
  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [ Sm5.PUSH (Sm5.Val (Sm5.Z i)) ]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [ Sm5.ADD ]
    | K.LETV (x, e1, e2) ->
        trans e1
        @ [ Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE ]
        @ trans e2 @ [ Sm5.UNBIND; Sm5.POP ]
    | K.READ x ->
        [
          Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD;
        ]
    | K.TRUE -> [ Sm5.PUSH (Sm5.Val (Sm5.B true)) ]
    | K.FALSE -> [ Sm5.PUSH (Sm5.Val (Sm5.B false)) ]
    | K.UNIT -> [ Sm5.PUSH (Sm5.Val Sm5.Unit) ]
    | K.VAR x -> [ Sm5.PUSH (Sm5.Id x); Sm5.LOAD ]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [ Sm5.SUB ]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [ Sm5.MUL ]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [ Sm5.DIV ]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [ Sm5.EQ ]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [ Sm5.LESS ]
    | K.NOT e -> trans e @ [ Sm5.NOT ]
    | K.ASSIGN (x, e) ->
        trans e
        @ [ Sm5.PUSH (Sm5.Id x); Sm5.STORE ]
        @ [ Sm5.PUSH (Sm5.Id x); Sm5.LOAD ]
    | K.SEQ (e1, e2) -> trans e1 @ [ Sm5.POP ] @ trans e2
    | IF (e1, e2, e3) -> trans e1 @ [ Sm5.JTR (trans e2, trans e3) ]
    (* | WHILE of exp * exp while loop *)
    (* | FOR of id * exp * exp * exp for loop *)
    | LETF (f, x, e1, e2) -> trans e1 @ []
    (* | CALLV of id * exp call by value *)
    (* | CALLR of id * id call by referenece *)
    | WRITE e -> trans e @ [ Sm5.PUT ]
    | _ -> failwith "Unimplemented"
end
