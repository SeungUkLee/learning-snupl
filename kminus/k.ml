(*
 * SNU 4190.310 Programming Languages
 *  K- Interpreter Skeleton Code
 *)

(* Location Signature *)
module type LOC = sig
  type t

  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC = struct
  type t = Location of int

  let base = Location 0
  let equal (Location a) (Location b) = a = b
  let diff (Location a) (Location b) = a - b
  let increase (Location base) n = Location (base + n)
end

(* Memory Signature *)
module type MEM = sig
  type 'a t

  exception Not_allocated
  exception Not_initialized

  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t -> 'a (* load value : Mem.load mem loc => value *)

  val store :
    'a t ->
    Loc.t ->
    'a ->
    'a t (* save value : Mem.store mem loc value => mem' *)

  val alloc :
    'a t ->
    Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV = sig
  type ('a, 'b) t

  exception Not_bound

  val empty : ('a, 'b) t (* get empty environment *)

  val lookup :
    ('a, 'b) t ->
    'a ->
    'b (* lookup environment : Env.lookup env key => content *)

  val bind :
    ('a, 'b) t ->
    'a ->
    'b ->
    ('a, 'b) t (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM = struct
  exception Not_allocated
  exception Not_initialized

  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list

  let empty = M (Loc.base, [])

  let rec replace_nth l n c =
    match l with
    | h :: t -> if n = 1 then c :: t else h :: replace_nth t (n - 1) c
    | [] -> raise Not_allocated

  let load (M (boundary, storage)) loc =
    match List.nth storage (Loc.diff boundary loc - 1) with
    | V v -> v
    | U -> raise Not_initialized

  let store (M (boundary, storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary, storage)) =
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV = struct
  exception Not_bound

  type ('a, 'b) t = E of ('a -> 'b)

  let empty = E (fun x -> raise Not_bound)
  let lookup (E env) id = env id
  let bind (E env) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS = sig
  exception Error of string

  type id = string

  type exp =
    | NUM of int
    | TRUE
    | FALSE
    | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp (* sequence *)
    | IF of exp * exp * exp (* if-then-else *)
    | WHILE of exp * exp (* while loop *)
    | LETV of id * exp * exp (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list (* call by value *)
    | CALLR of id * id list (* call by referenece *)
    | RECORD of (id * exp) list (* record construction *)
    | FIELD of exp * id (* access record field *)
    | ASSIGN of id * exp (* assgin to variable *)
    | ASSIGNF of exp * id * exp (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type memory
  type env
  type value = Num of int | Bool of bool | Unit | Record of (id -> Loc.t)

  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS = struct
  exception Error of string

  type id = string

  type exp =
    | NUM of int
    | TRUE
    | FALSE
    | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp (* sequence *)
    | IF of exp * exp * exp (* if-then-else *)
    | WHILE of exp * exp (* while loop *)
    | LETV of id * exp * exp (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list (* call by value *)
    | CALLR of id * id list (* call by referenece *)
    | RECORD of (id * exp) list (* record construction *)
    | FIELD of exp * id (* access record field *)
    | ASSIGN of id * exp (* assgin to variable *)
    | ASSIGNF of exp * id * exp (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type value = Num of int | Bool of bool | Unit | Record of (id -> Loc.t)
  type memory = value Mem.t

  type env = (id, env_entry) Env.t
  and env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with Num n -> n | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with Bool b -> b | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with Unit -> () | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with Record r -> r | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc")
      | Proc (id_list, exp, env) -> (id_list, exp, env)
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    let eval_num op n1 n2 =
      let v1, mem' = eval mem env n1 in
      let v2, mem'' = eval mem' env n2 in
      (Num (op (value_int v1) (value_int v2)), mem'')
    in
    let batch_eval (vs, m) e =
      let v, m' = eval m env e in
      (vs @ [ v ], m')
    in
    let batch_bind_and_store (env, mem) p v =
      let loc, mem' = Mem.alloc mem in
      (Env.bind env p (Addr loc), Mem.store mem' loc v)
    in
    match e with
    | READ x ->
        let v = Num (read_int ()) in
        let l = lookup_env_loc env x in
        (v, Mem.store mem l v)
    | WRITE e ->
        let v, mem' = eval mem env e in
        let n = value_int v in
        let _ = print_endline (string_of_int n) in
        (v, mem')
    | LETV (x, e1, e2) ->
        let v, mem' = eval mem env e1 in
        let l, mem'' = Mem.alloc mem' in
        eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | ASSIGN (x, e) ->
        let v, mem' = eval mem env e in
        let l = lookup_env_loc env x in
        (v, Mem.store mem' l v)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | NUM n -> (Num n, mem)
    | UNIT -> (Unit, mem)
    | VAR x ->
        let v = Mem.load mem (lookup_env_loc env x) in
        (v, mem)
    | ADD (n1, n2) -> eval_num ( + ) n1 n2
    | SUB (n1, n2) -> eval_num ( - ) n1 n2
    | MUL (n1, n2) -> eval_num ( * ) n1 n2
    | DIV (n1, n2) -> eval_num ( / ) n1 n2
    | EQUAL (e1, e2) ->
        let v1, mem' = eval mem env e1 in
        let v2, mem'' = eval mem' env e2 in
        (Bool (v1 = v2), mem'')
    | LESS (e1, e2) ->
        let v1, mem' = eval mem env e1 in
        let v2, mem'' = eval mem' env e2 in
        (Bool (v1 < v2), mem'')
    | NOT e ->
        let v, mem' = eval mem env e in
        (Bool (not (value_bool v)), mem')
    | SEQ (e1, e2) ->
        let _, mem' = eval mem env e1 in
        let v2, mem'' = eval mem' env e2 in
        (v2, mem'')
    | IF (e1, e2, e3) ->
        let cond, mem' = eval mem env e1 in
        eval mem' env (if value_bool cond then e2 else e3)
    | WHILE (e1, e2) ->
        let cond, mem' = eval mem env e1 in
        if value_bool cond then
          let _, mem1 = eval mem' env e2 in
          eval mem1 env e
        else (Unit, mem')
    | LETF (f, xs, e1, e2) ->
        let v, mem' = eval mem (Env.bind env f (Proc (xs, e1, env))) e2 in
        (v, mem')
    | CALLV (f, es) ->
        let params, e', env' = lookup_env_proc env f in
        let values, memn = List.fold_left batch_eval ([], mem) es in
        let env'', memn' =
          List.fold_left2 batch_bind_and_store (env', memn) params values
        in
        eval memn' (Env.bind env'' f (Proc (params, e', env'))) e'
    | CALLR (f, ys) ->
        let params, e, env' = lookup_env_proc env f in
        let batch_alias env p y =
          Env.bind env p (Addr (lookup_env_loc env y))
        in
        let env' = List.fold_left2 batch_alias env params ys in
        let env'' = Env.bind env' f (Proc (params, e, env')) in
        eval mem env'' e
    | RECORD [] -> (Unit, mem)
    | RECORD rs ->
        let split_record (xs, ys) (x, e) = (x :: xs, e :: ys) in
        let xs, es = List.fold_left split_record ([], []) rs in
        let values, memn = List.fold_left batch_eval ([], mem) es in
        let env', mem' =
          List.fold_left2 batch_bind_and_store (env, memn) xs values
        in
        (Record (lookup_env_loc env'), mem')
    | FIELD (e, x) ->
        let v, mem' = eval mem env e in
        let r = value_record v in
        (Mem.load mem' (r x), mem')
    | ASSIGNF (e1, x, e2) ->
        let v1, mem1 = eval mem env e1 in
        let r = value_record v1 in
        let v, mem2 = eval mem1 env e2 in
        (v, Mem.store mem2 (r x) v)

  let run (mem, env, pgm) =
    let v, _ = eval mem env pgm in
    v
end
