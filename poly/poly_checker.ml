(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton
 *)

open M

type var = string

type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
(* Modify, or add more if needed *)

type typ_scheme = SimpleTyp of typ | GenTyp of (var list * typ)

let count = ref 0

let new_var () =
  let _ = count := !count + 1 in
  "x_" ^ string_of_int !count

(* Definitions related to type environment *)
module TyEnv = struct
  type t = (M.id * typ_scheme) list

  let empty = []

  let find tyenv x =
    try List.assoc x tyenv
    with Not_found -> raise (M.TypeError "Undefined variable!")

  (* TODO *)
  let extend tyenv (x, t) = (x, t) :: tyenv
end

(* Definitions related to free type variable *)
let union_ftv ftv_1 ftv_2 =
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2

let sub_ftv ftv_1 ftv_2 = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v -> [ v ]

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas

let ftv_of_env : TyEnv.t -> var list =
 fun tyenv ->
  List.fold_left
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv

(* Generalize given type into a type scheme *)
let generalize : TyEnv.t -> typ -> typ_scheme =
 fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then SimpleTyp t else GenTyp (ftv, t)

(* Definitions related to substitution *)
module Subst = struct
  type t = typ -> typ

  let empty s = s

  let make : var -> typ -> t =
   fun x t ->
    let rec subs t' =
      match t' with
      | TVar x' -> if x = x' then t else t'
      | TPair (t1, t2) -> TPair (subs t1, subs t2)
      | TLoc t'' -> TLoc (subs t'')
      | TFun (t1, t2) -> TFun (subs t1, subs t2)
      | TInt | TBool | TString -> t'
    in
    subs

  let compose s1 s2 t = s1 (s2 t)

  (* let rec apply : t -> typ -> typ =
     fun subst typ ->
      match typ with
      | TInt -> TInt
      | TBool -> TBool
      | TString -> TString
      | TPair (a, b) -> TPair (apply subst a, apply subst b)
      | TLoc l -> TLoc (apply subst l)
      | TVar x -> subst typ
      | TFun (t1, t2) -> TFun (apply subst t1, apply subst t2) *)

  (* TODO *)
  let apply : t -> typ -> typ = fun subst typ -> subst typ

  let apply_scheme : t -> typ_scheme -> typ_scheme =
   fun subs tyscm ->
    match tyscm with
    | SimpleTyp t -> SimpleTyp (subs t)
    | GenTyp (alphas, t) ->
        (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
        let betas = List.map (fun _ -> new_var ()) alphas in
        let s' =
          List.fold_left2
            (fun acc_subst alpha beta ->
              compose (make alpha (TVar beta)) acc_subst)
            empty alphas betas
        in
        GenTyp (betas, subs (s' t))

  let apply_tyenv : t -> TyEnv.t -> TyEnv.t =
   fun subs tyenv ->
    List.map (fun (x, tyscm) -> (x, apply_scheme subs tyscm)) tyenv
end

let rec convert_typ : typ -> M.typ = function
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (convert_typ t1, convert_typ t2)
  | TLoc l -> M.TyLoc (convert_typ l)
  | TFun _ | TVar _ -> raise (M.TypeError "convert error")

let rec typ_to_string = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TPair (t1, t2) -> "pair(" ^ typ_to_string t1 ^ ", " ^ typ_to_string t2 ^ ")"
  | TLoc l -> "loc (" ^ typ_to_string l ^ ")"
  | TFun (t1, t2) -> "(" ^ typ_to_string t1 ^ " -> " ^ typ_to_string t2 ^ ")"
  | TVar x -> x

let rec unify : typ * typ -> Subst.t =
  let rec occurs (x : var) (t : typ) : bool =
    match t with
    | TVar a -> x = a
    | TFun (t1, t2) -> occurs x t1 || occurs x t2
    | TLoc l -> occurs x l
    | _ -> false
  in
  fun (given, expected) ->
    match (given, expected) with
    | a, b when a = b -> Subst.empty
    | TVar a, t when not (occurs a t) -> Subst.make a t
    | t, TVar a when not (occurs a t) -> Subst.make a t
    | TPair (t1, t2), TPair (t1', t2') | TFun (t1, t2), TFun (t1', t2') ->
        let s = unify (t1, t1') in
        let s' = unify (Subst.apply s t2, Subst.apply s t2') in
        Subst.compose s' s
    | TLoc t1, TLoc t2 -> unify (t1, t2)
    | _ ->
        raise
          (M.TypeError
             ("[TypeError]\nGiven: " ^ typ_to_string given ^ "\nExpected: "
            ^ typ_to_string expected))

(* TODO *)
let instantiation : typ_scheme -> typ =
 fun typscheme ->
  (* match typscheme with *)
  (* | SimpleTyp t -> t *)
  (* | GenTyp (vars, t) ->   *)
  (* let nvars = List.map (Fun.const (TVar (new_var ()))) vars in *)
  (* let s = List.combine vars nvars in *)
  (* Subst.apply s t *)
  match Subst.apply_scheme Subst.empty typscheme with
  | SimpleTyp t -> t
  | GenTyp (_, t) -> t

let rec expansive : M.exp -> bool = function
  | M.CONST _ | M.VAR _ | M.FN _ | M.READ -> false
  | M.APP _ | M.MALLOC _ -> true
  | M.LET (M.VAL (_, e1), e2)
  | M.LET (M.REC (_, _, e1), e2)
  | M.BOP (_, e1, e2)
  | M.ASSIGN (e1, e2)
  | M.SEQ (e1, e2)
  | M.PAIR (e1, e2) ->
      expansive e1 || expansive e2
  | M.WRITE e | M.BANG e | M.FST e | M.SND e -> expansive e
  | M.IF (e1, e2, e3) -> expansive e1 || expansive e2 || expansive e3

let rec m_algorithm : TyEnv.t -> M.exp -> typ -> Subst.t =
 fun tyenv exp expected ->
  match exp with
  | M.CONST (N _) -> unify (TInt, expected)
  | M.CONST (B _) -> unify (TBool, expected)
  | M.CONST (S _) -> unify (TString, expected)
  | M.VAR x ->
      let typscheme = TyEnv.find tyenv x in
      let typ = instantiation typscheme in
      unify (typ, expected)
  | M.FN (x, e) ->
      let a1, a2 = (TVar (new_var ()), TVar (new_var ())) in
      let s = unify (TFun (a1, a2), expected) in
      let tyenv, a2 =
        ( TyEnv.extend
            (Subst.apply_tyenv s tyenv)
            (x, SimpleTyp (Subst.apply s a1)),
          Subst.apply s a2 )
      in
      let s' = m_algorithm tyenv e a2 in
      Subst.compose s' s
  | M.APP (e1, e2) ->
      let a = TVar (new_var ()) in
      let s = m_algorithm tyenv e1 (TFun (a, expected)) in
      let tyenv, a = (Subst.apply_tyenv s tyenv, Subst.apply s a) in
      let s' = m_algorithm tyenv e2 a in
      Subst.compose s' s
  | M.LET (M.VAL (x, e1), e2) ->
      let a = TVar (new_var ()) in
      let s = m_algorithm tyenv e1 a in
      let tyenv, expected, a =
        (Subst.apply_tyenv s tyenv, Subst.apply s expected, Subst.apply s a)
      in

      let typescheme =
        if expansive e1 then SimpleTyp a else generalize tyenv a
      in
      let tyenv, expected =
        (TyEnv.extend tyenv (x, typescheme), Subst.apply s expected)
      in

      let s' = m_algorithm tyenv e2 expected in
      Subst.compose s' s
  | M.LET (M.REC (f, x, e1), e2) ->
      let a = TVar (new_var ()) in
      let s =
        m_algorithm (TyEnv.extend tyenv (f, SimpleTyp a)) (M.FN (x, e1)) a
      in

      let tyenv, expected, a =
        (Subst.apply_tyenv s tyenv, Subst.apply s expected, Subst.apply s a)
      in

      let s' =
        m_algorithm (TyEnv.extend tyenv (f, generalize tyenv a)) e2 expected
      in
      Subst.compose s' s
  | M.IF (e1, e2, e3) ->
      let s = m_algorithm tyenv e1 TBool in
      let tyenv, expected =
        (Subst.apply_tyenv s tyenv, Subst.apply s expected)
      in
      let s' = m_algorithm tyenv e2 expected in
      let tyenv, expected =
        (Subst.apply_tyenv s' tyenv, Subst.apply s' expected)
      in
      let s'' = m_algorithm tyenv e3 expected in

      Subst.compose s'' (Subst.compose s' s)
  | M.BOP (op, e1, e2) -> (
      let bop op r =
        let s = unify (r, expected) in
        let tyenv, op = (Subst.apply_tyenv s tyenv, Subst.apply s op) in
        let s' = m_algorithm tyenv e1 op in
        let tyenv, op = (Subst.apply_tyenv s' tyenv, Subst.apply s' op) in
        let s'' = m_algorithm tyenv e2 op in
        Subst.compose s'' (Subst.compose s' s)
      in
      match op with
      | M.ADD | M.SUB -> bop TInt TInt
      | M.AND | M.OR -> bop TBool TBool
      | M.EQ -> bop (TVar (new_var ())) TBool)
  | M.READ -> unify (TInt, expected)
  | M.WRITE e ->
      let a = TVar (new_var ()) in
      let s = unify (a, expected) in
      let tyenv, expected =
        (Subst.apply_tyenv s tyenv, Subst.apply s expected)
      in
      let s' = m_algorithm tyenv e expected in
      Subst.compose s' s
  | M.MALLOC e ->
      let a = TVar (new_var ()) in
      let s = m_algorithm tyenv e a in
      let expected, a = (Subst.apply s expected, Subst.apply s a) in
      let s' = unify (TLoc a, expected) in
      Subst.compose s' s
  | M.ASSIGN (e1, e2) ->
      let s = m_algorithm tyenv e1 (TLoc expected) in
      let tyenv, expected =
        (Subst.apply_tyenv s tyenv, Subst.apply s expected)
      in
      let s' = m_algorithm tyenv e2 expected in
      Subst.compose s' s
  | M.BANG e -> m_algorithm tyenv e (TLoc expected)
  | M.SEQ (e1, e2) ->
      let a = TVar (new_var ()) in
      let s = m_algorithm tyenv e1 a in
      let tyenv, expected =
        (Subst.apply_tyenv s tyenv, Subst.apply s expected)
      in
      let s' = m_algorithm tyenv e2 expected in
      Subst.compose s' s
  | M.PAIR (e1, e2) ->
      let a1, a2 = (TVar (new_var ()), TVar (new_var ())) in
      let s = unify (TPair (a1, a2), expected) in
      let tyenv, a1, a2 =
        (Subst.apply_tyenv s tyenv, Subst.apply s a1, Subst.apply s a2)
      in
      let s' = m_algorithm tyenv e1 a1 in
      let tyenv, a2 = (Subst.apply_tyenv s' tyenv, Subst.apply s' a2) in
      let s'' = m_algorithm tyenv e2 a2 in
      Subst.compose s'' (Subst.compose s' s)
  | M.FST e ->
      let a = TVar (new_var ()) in
      m_algorithm tyenv e (TPair (expected, a))
  | M.SND e ->
      let a = TVar (new_var ()) in
      m_algorithm tyenv e (TPair (a, expected))

let check : M.exp -> M.typ =
 fun exp ->
  let a = TVar (new_var ()) in
  let subst = m_algorithm TyEnv.empty exp a in
  convert_typ (subst a)
