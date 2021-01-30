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
  | tyArrow (t1:ty) (t2:ty)
  | Error.

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
   ("false" , tyBool);
   ("Type Error" , Error);
   ("Unknown Error" , Error)
  ].

Definition type_checker (t: tm) := type_check rho_const nil t.

Fixpoint free_var_count (s: string)(t: tm): nat :=
  match t with
  | var s' => if string_dec s s' then 1 else 0
  | abs s' typ t' => if string_dec s s' then 0 else free_var_count s t'
  | app t1 t2 => (free_var_count s t1) + (free_var_count s t2)
  | _ => 0
  end.

Definition free_var_dec (s: string)(t: tm): bool :=
  match (free_var_count s t) with
  | O => false
  | _ => true
  end.

Fixpoint var_subst (l: list (string * tm)) (x: string) :=
match l with
| nil => var x
| (x',t') :: l => if string_dec x' x then t' else var_subst l x
end.

Fixpoint tm_subst (l: list (string * tm))(t: tm) :tm :=
  match t with
  | const s => const s
  | var s => var_subst l s
  | app t1 t2 => app (tm_subst l t1) (tm_subst l t2)
  | abs s typ t1 => 
  (fix abs_subst (l: list (string * tm)):=
  match l with
  | nil => abs s typ t
  | (x',t') :: l => if string_dec x' s
                        then abs_subst l
                        else if free_var_dec s t1
                                then abs "ss" typ (tm_subst ((s, var "ss") :: l) t1)
                                else abs s typ (tm_subst l t1)
  end) l
  end.

Fixpoint next_state (t: tm): option tm :=
  match t with
  | const _ => None
  | var _ => None
  | abs _ _ _ => None
  | app (abs s typ t1) t2 => match is_const_hd_val t2 with
                         | None => Some (tm_subst [(s,t2)] t1)
                         | Some _ => match next_state t2 with
                                     | Some t2' => Some (app (abs s typ t1) t2')
                                     | None => None
                                     end
                         end
  | app t1 t2 => match next_state t1 with
                 | Some t' => Some (app t' t2)
                 | None => match next_state t2 with
                           | Some t' => Some (app t1 t')
                           | None => Some (app t1 t2)
                           end
                 end
  end.

Fixpoint multi_state_pre (limit: nat) (t: tm): list tm :=
  match limit with
  | O => nil
  | S n => 
    match next_state t with
    | Some t' => t' :: (multi_state_pre n t')
    | None => nil
    end
  end.

Definition default_limit :nat := 10000.

Definition multi_state (t: tm) := head(multi_state_pre default_limit t).

Definition run (t: tm): tm :=
  match type_checker t with
  | None => const "Type Error"
  | Some TYPE => 
    match multi_state t with
    | None => const "Unknown Error"
    | Some t' => t'
    end
  end.
 
Require Coq.extraction.Extraction.
Extraction Language Haskell.
Require Coq.extraction.ExtrHaskellString.
Extraction "STLC.hs" run.
