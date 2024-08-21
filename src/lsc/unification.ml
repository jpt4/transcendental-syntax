open Base

module type Signature = sig
  type idvar
  type idfunc
  val equal_idvar : idvar -> idvar -> bool
  val equal_idfunc : idfunc -> idfunc -> bool
  val compatible : idfunc -> idfunc -> bool
end

(* ---------------------------------------
   Elementary definitions
   --------------------------------------- *)

module Make (Sig : Signature) = struct

type term =
  | Var of Sig.idvar
  | Func of (Sig.idfunc * term list)

let rec equal_term t u =
  match t, u with
  | Var x, Var y -> Sig.equal_idvar x y
  | Func (f, ts), Func (g, us) ->
    Sig.equal_idfunc f g &&
    List.for_all2_exn ~f:(fun t u -> equal_term t u) ts us
  | _ -> false

type substitution = (Sig.idvar * term) list
type equation = term * term
type problem = equation list

let rec fold fnode fbase acc = function
  | Var x -> fbase x acc
  | Func (f, ts) ->
    let acc' = fnode f acc in
    List.fold ts ~init:acc' ~f:(fold fnode fbase)

let rec map fnode fbase = function
  | Var x -> fbase x
  | Func (g, ts) ->
    Func (fnode g, List.map ~f:(map fnode fbase) ts)

let skip = fun _ acc -> acc

let exists_var pred = fold skip (fun y acc -> pred y || acc) false
let for_all_var pred = fold skip (fun y acc -> pred y && acc) true
let exists_func pred = fold (fun y acc -> pred y || acc) skip false
let for_all_func pred = fold (fun y acc -> pred y && acc) skip true

let occurs x = exists_var (fun y -> Sig.equal_idvar x y)

let vars = fold skip List.cons []

let apply sub x =
  match List.Assoc.find sub ~equal:Sig.equal_idvar x with
  | None -> Var x
  | Some t -> t

let subst sub = map Fn.id (apply sub)

(* ---------------------------------------
   A few useful functions
   --------------------------------------- *)

let lift_pairl f (x, y) = (f x, y)
let lift_pairr f (x, y) = (x, f y)
let lift_pair f p = p |> lift_pairl f |> lift_pairr f

(* ---------------------------------------
   Unification algorithm
   --------------------------------------- *)

let rec solve ?(withloops=true) sub : problem -> substitution option = function
  | [] -> Some sub
  (* Clear *)
  | (Var x, Var y)::pbs when Sig.equal_idvar x y ->
  if withloops then solve ~withloops sub pbs else None
  (* Orient + Replace *)
  | (Var x, t)::pbs | (t, Var x)::pbs -> elim ~withloops x t pbs sub
  (* Open *)
  | (Func (f, ts), Func (g, us))::pbs when
    Sig.compatible f g && List.length ts = List.length us ->
      solve ~withloops sub ((List.zip_exn ts us)@pbs)
  | _ -> None
(* Replace *)
and elim ?(withloops=true) x t pbs sub : substitution option =
  if occurs x t then None (* Circularity *)
  else
    let new_prob = List.map ~f:(lift_pair (subst [(x, t)])) pbs in
    let new_sub = (x, t) :: List.map ~f:(lift_pairr (subst [(x, t)])) sub in
    solve ~withloops new_sub new_prob

let solution ?(withloops=true) : problem -> substitution option =
  solve ~withloops []

end
