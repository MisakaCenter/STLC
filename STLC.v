Require Import Coq.Strings.String.
Open Scope string_scope.
Require Import Coq.Lists.List.
Import ListNotations.
Require Import Coq.ZArith.ZArith.
Require Import Coq.Relations.Relation_Operators.
Require Import Coq.Logic.Eqdep_dec.
Require Import Coq.Arith.Peano_dec.

Inductive ty : Type :=
  | tyNat
  | tyBool
  | tyArrow (t1:ty) (t2:ty).

Inductive tm : Type :=
  | const : string -> tm
  | var : string -> tm
  | app : tm -> tm -> tm
  | abs : string -> ty -> tm -> tm.

Fixpoint is_const_hd_val_rec (t: tm) (tl: list tm) : option (string * list tm) :=
  match t with
  | const s => Some (s, tl)
  | app t1 t2 => if is_val t2 then is_const_hd_val_rec t1 (t2 :: tl) else None
  | _ => None
  end
with is_val (t: tm): bool :=
  match t with
  | const _ => true
  | app t1 t2 => if is_val t2 then match is_const_hd_val_rec t1 (t2 :: nil) with | Some _ => true | _ => false end else false
  | _ => false
  end.

Definition is_const_hd_val (t: tm): option (string * list tm) :=
  is_const_hd_val_rec t nil.

Fixpoint rho_check (rho: list (string * ty))(s :string): option ty:=
  match rho with
  | nil => None
  | (s',typ)::rho' => if string_dec s s' then Some typ else rho_check rho' s
  end.

Fixpoint ty_dec (type1 type2: ty): bool:=
  match type1,type2 with
  | tyArrow t1 t2, tyArrow t1' t2' => andb (ty_dec t1 t1') (ty_dec t2 t2')
  | tyBool , tyBool => true
  | tyNat , tyNat => true
  | _ , _ => false
  end.

Fixpoint type_check (rho_const: list (string * ty))(rho_var: list (string * ty))(t: tm): option ty :=
  match t with
  | const s => rho_check rho_const s
  | abs s typ t' => match (type_check rho_const ((s,typ)::rho_var) t') with
                    | None => None
                    | Some a => Some (tyArrow typ a)
                    end
  | app t1 t2 => match (type_check rho_const rho_var t1 , type_check rho_const rho_var t2) with
                 | (Some (tyArrow a1 b) ,Some a2) => if ty_dec a1 a2 then Some b else None
                 | _ => None
                 end
  | var s => rho_check rho_var s
  end.

Definition rho_const: list (string * ty) := 
  [("S" , tyArrow tyNat tyNat);
   ("O" , tyNat);
   ("true" , tyBool);
   ("false" , tyBool)
  ].

Definition type_checker (t:tm) := type_check rho_const nil t.

Fixpoint tm_subst (t: tm)

Fixpoint next_state (t: tm): option tm :=
  match t with
  | const _ _ => None
  | var _ => None
  | abs _ _ _ => None
  | app t1 t2 => match next_state t1 with
                 | Some t' => next_state (app t' t2)
                 | None => match next_state t2 with
                           | Some t' => next_state t1 t'
                           | None => Some (app t1 t2)
                           end
                 end
  | app (abs s t1) t2 => Some (tm)
  end.