module STLC where

import qualified Prelude
import GHC.Show

data Nat =
   O
 | S Nat

data Uint =
   Nil
 | D0 Uint
 | D1 Uint
 | D2 Uint
 | D3 Uint
 | D4 Uint
 | D5 Uint
 | D6 Uint
 | D7 Uint
 | D8 Uint
 | D9 Uint

data Uint0 =
   Nil0
 | D10 Uint0
 | D11 Uint0
 | D12 Uint0
 | D13 Uint0
 | D14 Uint0
 | D15 Uint0
 | D16 Uint0
 | D17 Uint0
 | D18 Uint0
 | D19 Uint0
 | Da Uint0
 | Db Uint0
 | Dc Uint0
 | Dd Uint0
 | De Uint0
 | Df Uint0

data Uint1 =
   UIntDecimal Uint
 | UIntHexadecimal Uint0

add :: Nat -> Nat -> Nat
add n m =
  case n of {
   O -> m;
   S p -> S (add p m)}

tail_add :: Nat -> Nat -> Nat
tail_add n m =
  case n of {
   O -> m;
   S n0 -> tail_add n0 (S m)}

tail_addmul :: Nat -> Nat -> Nat -> Nat
tail_addmul r n m =
  case n of {
   O -> r;
   S n0 -> tail_addmul (tail_add m r) n0 m}

tail_mul :: Nat -> Nat -> Nat
tail_mul = tail_addmul O

of_uint_acc :: Uint -> Nat -> Nat
of_uint_acc d acc =
  case d of {
   Nil -> acc;
   D0 d0 ->
    of_uint_acc d0 (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc);
   D1 d0 ->
    of_uint_acc d0 (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc));
   D2 d0 ->
    of_uint_acc d0 (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc)));
   D3 d0 ->
    of_uint_acc d0 (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc))));
   D4 d0 ->
    of_uint_acc d0 (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc)))));
   D5 d0 ->
    of_uint_acc d0 (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc))))));
   D6 d0 ->
    of_uint_acc d0 (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc)))))));
   D7 d0 ->
    of_uint_acc d0 (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc))))))));
   D8 d0 ->
    of_uint_acc d0 (S (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc)))))))));
   D9 d0 ->
    of_uint_acc d0 (S (S (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S O)))))))))) acc))))))))))}

of_uint :: Uint -> Nat
of_uint d =
  of_uint_acc d O

of_hex_uint_acc :: Uint0 -> Nat -> Nat
of_hex_uint_acc d acc =
  case d of {
   Nil0 -> acc;
   D10 d0 ->
    of_hex_uint_acc d0
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc);
   D11 d0 ->
    of_hex_uint_acc d0 (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc));
   D12 d0 ->
    of_hex_uint_acc d0 (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc)));
   D13 d0 ->
    of_hex_uint_acc d0 (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc))));
   D14 d0 ->
    of_hex_uint_acc d0 (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc)))));
   D15 d0 ->
    of_hex_uint_acc d0 (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc))))));
   D16 d0 ->
    of_hex_uint_acc d0 (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc)))))));
   D17 d0 ->
    of_hex_uint_acc d0 (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc))))))));
   D18 d0 ->
    of_hex_uint_acc d0 (S (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc)))))))));
   D19 d0 ->
    of_hex_uint_acc d0 (S (S (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc))))))))));
   Da d0 ->
    of_hex_uint_acc d0 (S (S (S (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc)))))))))));
   Db d0 ->
    of_hex_uint_acc d0 (S (S (S (S (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc))))))))))));
   Dc d0 ->
    of_hex_uint_acc d0 (S (S (S (S (S (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc)))))))))))));
   Dd d0 ->
    of_hex_uint_acc d0 (S (S (S (S (S (S (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc))))))))))))));
   De d0 ->
    of_hex_uint_acc d0 (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc)))))))))))))));
   Df d0 ->
    of_hex_uint_acc d0 (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (tail_mul (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
        O)))))))))))))))) acc))))))))))))))))}

of_hex_uint :: Uint0 -> Nat
of_hex_uint d =
  of_hex_uint_acc d O

of_num_uint :: Uint1 -> Nat
of_num_uint d =
  case d of {
   UIntDecimal d0 -> of_uint d0;
   UIntHexadecimal d0 -> of_hex_uint d0}

hd_error :: [] a1 -> Prelude.Maybe a1
hd_error l =
  case l of {
   [] -> Prelude.Nothing;
   (:) x _ -> Prelude.Just x}

data Ty =
   TyNat
 | TyBool
 | TyArrow Ty Ty
 | Error
 deriving (Show)
data Tm =
   Const Prelude.String
 | Var Prelude.String
 | App Tm Tm
 | Abs Prelude.String Ty Tm 
 deriving (Show)

is_const_hd_val_rec :: Tm -> [] Tm -> Prelude.Maybe
                       ((,) Prelude.String ([] Tm))
is_const_hd_val_rec t tl =
  case t of {
   Const s -> Prelude.Just ((,) s tl);
   App t1 t2 ->
    case is_val t2 of {
     Prelude.True -> is_const_hd_val_rec t1 ((:) t2 tl);
     Prelude.False -> Prelude.Nothing};
   _ -> Prelude.Nothing}

is_val :: Tm -> Prelude.Bool
is_val t =
  case t of {
   Const _ -> Prelude.True;
   App t1 t2 ->
    case is_val t2 of {
     Prelude.True ->
      case is_const_hd_val_rec t1 ([t2]) of {
       Prelude.Just _ -> Prelude.True;
       Prelude.Nothing -> Prelude.False};
     Prelude.False -> Prelude.False};
   _ -> Prelude.False}

is_const_hd_val :: Tm -> Prelude.Maybe ((,) Prelude.String ([] Tm))
is_const_hd_val t =
  is_const_hd_val_rec t []

rho_check :: [] ((,) Prelude.String Ty) -> Prelude.String ->
             Prelude.Maybe Ty
rho_check rho s =
  case rho of {
   [] -> Prelude.Nothing;
   (:) p rho' ->
    case p of {
     (,) s' typ ->
      case ((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool)
             s s' of {
       Prelude.True -> Prelude.Just typ;
       Prelude.False -> rho_check rho' s}}}

ty_dec :: Ty -> Ty -> Prelude.Bool
ty_dec type1 type2 =
  case type1 of {
   TyNat -> case type2 of {
             TyNat -> Prelude.True;
             _ -> Prelude.False};
   TyBool -> case type2 of {
              TyBool -> Prelude.True;
              _ -> Prelude.False};
   TyArrow t1 t2 ->
    case type2 of {
     TyArrow t1' t2' -> (Prelude.&&) (ty_dec t1 t1') (ty_dec t2 t2');
     _ -> Prelude.False};
   Error -> Prelude.False}

type_check :: [] ((,) Prelude.String Ty) -> []
              ((,) Prelude.String Ty) -> Tm -> Prelude.Maybe Ty
type_check rho_const0 rho_var t =
  case t of {
   Const s -> rho_check rho_const0 s;
   Var s -> rho_check rho_var s;
   App t1 t2 ->
    let {o = type_check rho_const0 rho_var t1} in
    let {o0 = type_check rho_const0 rho_var t2} in
    case o of {
     Prelude.Just t0 ->
      case t0 of {
       TyArrow a1 b ->
        case o0 of {
         Prelude.Just a2 ->
          case ty_dec a1 a2 of {
           Prelude.True -> Prelude.Just b;
           Prelude.False -> Prelude.Nothing};
         Prelude.Nothing -> Prelude.Nothing};
       _ -> Prelude.Nothing};
     Prelude.Nothing -> Prelude.Nothing};
   Abs s typ t' ->
    case type_check rho_const0 ((:) ((,) s typ) rho_var) t' of {
     Prelude.Just a -> Prelude.Just (TyArrow typ a);
     Prelude.Nothing -> Prelude.Nothing}}

rho_const :: [] ((,) Prelude.String Ty)
rho_const =
  [((,) "S" (TyArrow TyNat TyNat)), ((,) "O" TyNat), ((,) "true"
    TyBool), ((,) "false" TyBool), ((,) "Type Error" Error), ((,)
    "Unknown Error" Error)]

type_checker :: Tm -> Prelude.Maybe Ty
type_checker = type_check rho_const []

free_var_count :: Prelude.String -> Tm -> Nat
free_var_count s t =
  case t of {
   Const _ -> O;
   Var s' ->
    case ((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) s
           s' of {
     Prelude.True -> S O;
     Prelude.False -> O};
   App t1 t2 -> add (free_var_count s t1) (free_var_count s t2);
   Abs s' _ t' ->
    case ((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) s
           s' of {
     Prelude.True -> O;
     Prelude.False -> free_var_count s t'}}

free_var_dec :: Prelude.String -> Tm -> Prelude.Bool
free_var_dec s t =
  case free_var_count s t of {
   O -> Prelude.False;
   S _ -> Prelude.True}

var_subst :: [] ((,) Prelude.String Tm) -> Prelude.String -> Tm
var_subst l x =
  case l of {
   [] -> Var x;
   (:) p l0 ->
    case p of {
     (,) x' t' ->
      case ((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool)
             x' x of {
       Prelude.True -> t';
       Prelude.False -> var_subst l0 x}}}

tm_subst :: [] ((,) Prelude.String Tm) -> Tm -> Tm
tm_subst l t =
  case t of {
   Const s -> Const s;
   Var s -> var_subst l s;
   App t1 t2 -> App (tm_subst l t1) (tm_subst l t2);
   Abs s typ t1 ->
    let {
     abs_subst l0 =
       case l0 of {
        [] -> Abs s typ t;
        (:) p l1 ->
         case p of {
          (,) x' _ ->
           case ((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool)
                  x' s of {
            Prelude.True -> abs_subst l1;
            Prelude.False ->
             case free_var_dec s t1 of {
              Prelude.True -> Abs "ss" typ
               (tm_subst ((:) ((,) s (Var "ss")) l1) t1);
              Prelude.False -> Abs s typ (tm_subst l1 t1)}}}}}
    in abs_subst l}

next_state :: Tm -> Prelude.Maybe Tm
next_state t =
  case t of {
   App t1 t2 ->
    case t1 of {
     Abs s typ t3 ->
      case is_const_hd_val t2 of {
       Prelude.Just _ ->
        case next_state t2 of {
         Prelude.Just t2' -> Prelude.Just (App (Abs s typ t3) t2');
         Prelude.Nothing -> Prelude.Nothing};
       Prelude.Nothing -> Prelude.Just (tm_subst ([((,) s t2)]) t3)};
     _ ->
      case next_state t1 of {
       Prelude.Just t' -> Prelude.Just (App t' t2);
       Prelude.Nothing ->
        case next_state t2 of {
         Prelude.Just t' -> Prelude.Just (App t1 t');
         Prelude.Nothing -> Prelude.Just (App t1 t2)}}};
   _ -> Prelude.Nothing}

multi_state_pre :: Nat -> Tm -> [] Tm
multi_state_pre limit t =
  case limit of {
   O -> [];
   S n ->
    case next_state t of {
     Prelude.Just t' -> (:) t' (multi_state_pre n t');
     Prelude.Nothing -> []}}

default_limit :: Nat
default_limit =
  of_num_uint (UIntDecimal (D1 (D0 (D0 (D0 (D0 Nil))))))

multi_state :: Tm -> Prelude.Maybe Tm
multi_state t =
  hd_error (multi_state_pre default_limit t)

run :: Tm -> Tm
run t =
  case type_checker t of {
   Prelude.Just _ ->
    case multi_state t of {
     Prelude.Just t' -> t';
     Prelude.Nothing -> Const "Unknown Error"};
   Prelude.Nothing -> Const "Type Error"}