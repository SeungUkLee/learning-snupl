(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type tyvar = string

let count = ref 0

let new_var () =
  let _ = count := !count + 1 in
  "x_" ^ string_of_int !count

type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of tyvar

type typ_eqn = (typ * typ) list

let rec typ_to_string = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TPair (t1, t2) -> "pair(" ^ typ_to_string t1 ^ ", " ^ typ_to_string t2 ^ ")"
  | TLoc l -> "loc (" ^ typ_to_string l ^ ")"
  | TFun (t1, t2) -> "(" ^ typ_to_string t1 ^ " -> " ^ typ_to_string t2 ^ ")"
  | TVar x -> x

module TEnv = struct
  type t = M.id -> typ

  let empty _ = raise (M.TypeError "empty type environment")
  let find tenv x = tenv x
  let extend tenv (x, t) y = if x = y then t else tenv y
end

module Subst = struct
  type t = typ -> typ

  let empty : t = fun s -> s
  let compose s' s ty = s' (s ty)

  let make ((x : tyvar), (t : typ)) =
    let rec subst (ty : typ) : typ =
      match ty with
      | TInt | TBool | TString -> ty
      | TPair (t1, t2) -> TPair (subst t1, subst t2)
      | TLoc t -> TLoc (subst t)
      | TFun (tfunc, tparam) -> TFun (subst tfunc, subst tparam)
      | TVar name when not (x = name) -> ty
      | TVar name -> t
    in
    subst

  let rec apply : t -> typ -> typ =
   fun subst typ ->
    match typ with
    | TInt -> TInt
    | TBool -> TBool
    | TString -> TString
    | TPair (a, b) -> TPair (apply subst a, apply subst b)
    | TLoc l -> TLoc (apply subst l)
    | TVar x -> subst typ
    | TFun (t1, t2) -> TFun (apply subst t1, apply subst t2)

  let apply_tenv : t -> TEnv.t -> TEnv.t =
   fun subst tenv x -> apply subst (TEnv.find tenv x)
end

let rec unify : typ * typ -> Subst.t =
  let rec occurs (x : tyvar) (t : typ) : bool =
    match t with
    | TVar a -> x = a
    | TFun (t1, t2) -> occurs x t1 || occurs x t2
    | TLoc l -> occurs x l
    | _ -> false
  in
  fun (given, expected) ->
    match (given, expected) with
    | a, b when a = b -> Subst.empty
    | TVar a, t when not (occurs a t) -> Subst.make (a, t)
    | t, TVar a when not (occurs a t) -> Subst.make (a, t)
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

let rec m_algorithm : TEnv.t -> M.exp -> typ -> Subst.t =
 fun tenv exp expected ->
  match exp with
  | M.CONST (N _) -> unify (TInt, expected)
  | M.CONST (B _) -> unify (TBool, expected)
  | M.CONST (S _) -> unify (TString, expected)
  | M.VAR x -> unify (TEnv.find tenv x, expected)
  | M.FN (x, e) ->
      let a1, a2 = (TVar (new_var ()), TVar (new_var ())) in
      let s = unify (TFun (a1, a2), expected) in
      let tenv, a2 =
        ( TEnv.extend (Subst.apply_tenv s tenv) (x, Subst.apply s a1),
          Subst.apply s a2 )
      in
      let s' = m_algorithm tenv e a2 in
      Subst.compose s' s
  | M.APP (e1, e2) ->
      let a = TVar (new_var ()) in
      let s = m_algorithm tenv e1 (TFun (a, expected)) in
      let tenv, a = (Subst.apply_tenv s tenv, Subst.apply s a) in
      let s' = m_algorithm tenv e2 a in
      Subst.compose s' s
  | M.LET (M.VAL (x, e1), e2) ->
      let a = TVar (new_var ()) in
      let s = m_algorithm tenv e1 a in
      let tenv, expected =
        ( TEnv.extend (Subst.apply_tenv s tenv) (x, Subst.apply s a),
          Subst.apply s expected )
      in
      let s' = m_algorithm tenv e2 expected in
      Subst.compose s' s
  | M.LET (M.REC (f, x, e1), e2) ->
      let a = TVar (new_var ()) in
      let s = m_algorithm (TEnv.extend tenv (f, a)) (M.FN (x, e1)) a in
      let tenv, expected, a =
        (Subst.apply_tenv s tenv, Subst.apply s expected, Subst.apply s a)
      in
      let s' = m_algorithm (TEnv.extend tenv (f, a)) e2 expected in
      Subst.compose s' s
  | M.IF (e1, e2, e3) ->
      let s = m_algorithm tenv e1 TBool in
      let tenv, expected = (Subst.apply_tenv s tenv, Subst.apply s expected) in
      let s' = m_algorithm tenv e2 expected in
      let tenv, expected =
        (Subst.apply_tenv s' tenv, Subst.apply s' expected)
      in
      let s'' = m_algorithm tenv e3 expected in

      Subst.compose s'' (Subst.compose s' s)
  | M.BOP (op, e1, e2) -> (
      let bop op r =
        let s = unify (r, expected) in
        let tenv, op = (Subst.apply_tenv s tenv, Subst.apply s op) in
        let s' = m_algorithm tenv e1 op in
        let tenv, op = (Subst.apply_tenv s' tenv, Subst.apply s' op) in
        let s'' = m_algorithm tenv e2 op in
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
      let tenv, expected = (Subst.apply_tenv s tenv, Subst.apply s expected) in
      let s' = m_algorithm tenv e expected in
      Subst.compose s' s
  | M.MALLOC e ->
      let a = TVar (new_var ()) in
      let s = m_algorithm tenv e a in
      let expected, a = (Subst.apply s expected, Subst.apply s a) in
      let s' = unify (TLoc a, expected) in
      Subst.compose s' s
  | M.ASSIGN (e1, e2) ->
      let s = m_algorithm tenv e1 (TLoc expected) in
      let tenv, expected = (Subst.apply_tenv s tenv, Subst.apply s expected) in
      let s' = m_algorithm tenv e2 expected in
      Subst.compose s' s
  | M.BANG e -> m_algorithm tenv e (TLoc expected)
  | M.SEQ (e1, e2) ->
      let a = TVar (new_var ()) in
      let s = m_algorithm tenv e1 a in
      let tenv, expected = (Subst.apply_tenv s tenv, Subst.apply s expected) in
      let s' = m_algorithm tenv e2 expected in
      Subst.compose s' s
  | M.PAIR (e1, e2) ->
      let a1, a2 = (TVar (new_var ()), TVar (new_var ())) in
      let s = unify (TPair (a1, a2), expected) in
      let tenv, a1, a2 =
        (Subst.apply_tenv s tenv, Subst.apply s a1, Subst.apply s a2)
      in
      let s' = m_algorithm tenv e1 a1 in
      let tenv, a2 = (Subst.apply_tenv s' tenv, Subst.apply s' a2) in
      let s'' = m_algorithm tenv e2 a2 in
      Subst.compose s'' (Subst.compose s' s)
  | M.FST e ->
      let a = TVar (new_var ()) in
      m_algorithm tenv e (TPair (expected, a))
  | M.SND e ->
      let a = TVar (new_var ()) in
      m_algorithm tenv e (TPair (a, expected))

let rec convert_typ : typ -> M.types = function
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (convert_typ t1, convert_typ t2)
  | TLoc l -> M.TyLoc (convert_typ l)
  | TFun _ | TVar _ -> raise (M.TypeError ("convert error"))

let check : M.exp -> M.types =
 fun exp ->
  let a = TVar (new_var ()) in
  let subst = m_algorithm TEnv.empty exp a in
  convert_typ (Subst.apply subst a)
