open Ast
open Astlib
open Tctxt

(* Error Reporting ---------------------------------------------------------- *)
(* NOTE: Use type_error to report error messages for ill-typed programs. *)

exception TypeError of string

let type_error (l : 'a node) err = 
  let (_, (s, e), _) = l.loc in
  raise (TypeError (Printf.sprintf "[%d, %d] %s" s e err))


(* initial context: G0 ------------------------------------------------------ *)
(* The Oat types of the Oat built-in functions *)
let builtins =
  [ "array_of_string",  ([TRef RString],  RetVal (TRef(RArray TInt)))
  ; "string_of_array",  ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", ([TRef RString],  RetVal TInt)
  ; "string_of_int",    ([TInt], RetVal (TRef RString))
  ; "string_cat",       ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     ([TRef RString],  RetVoid)
  ; "print_int",        ([TInt], RetVoid)
  ; "print_bool",       ([TBool], RetVoid)
  ]

(* binary operation types --------------------------------------------------- *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)
  | Eq | Neq -> failwith "typ_of_binop called on polymorphic == or !=" 
  (*we only know operand types at RT*)

(* unary operation types ---------------------------------------------------- *)
let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* subtyping ---------------------------------------------------------------- *)
(* Decides whether H |- t1 <: t2 
    - assumes that H contains the declarations of all the possible struct types

    - you will want to introduce addition (possibly mutually recursive) 
      helper functions to implement the different judgments of the subtyping
      relation. We have included a template for subtype_ref to get you started.
      (Don't forget about OCaml's 'and' keyword.)
*)
let rec subtype (c : Tctxt.t) (t1 : Ast.ty) (t2 : Ast.ty) : bool = 
  match (t1, t2) with 
    |(TInt, TInt) -> true
    |(TBool, TBool) -> true
    |(TNullRef r1, TNullRef r2) -> subtype_ref c r1 r2
    |(TRef rt1, TRef rt2) -> subtype_ref c rt1 rt2
    |(TRef r1, TNullRef r2) -> subtype_ref c r1 r2
    |_ -> false


(* Decides whether H |-r ref1 <: ref2 *)
and subtype_ref (c : Tctxt.t) (t1 : Ast.rty) (t2 : Ast.rty) : bool = 
  match (t1, t2) with 
    |(RString, RString) -> true
    |(RArray t1, RArray t2) -> t1 = t2
    |(RStruct id1, RStruct id2) -> helper_struct c (lookup_struct id1 c) (lookup_struct id2 c)
    | (RFun (ts1, r1), RFun (ts2, r2)) -> (helper_funs c ts1 ts2) && (subtype_rt c r1 r2)
    |_ -> failwith "unimplemented"

and helper_struct (c : Tctxt.t) (s1 : Ast.field list) (s2 : Ast.field list) : bool = 
  match (s1, s2) with 
    | ([], []) -> true
    | ([], _) -> false
    | (_, []) -> true
    | (({fieldName = n1; ftyp = t1} :: xs), ({fieldName = n2; ftyp = t2} :: ys)) -> 
      n1 = n2 && t1 = t2 && (helper_struct c xs ys)
  
and helper_funs (c : Tctxt.t) (l1: Ast.ty list) (l2: Ast.ty list) : bool = 
  match (l1, l2) with
    | ([], []) -> true
    | (_ , []) -> false
    | ([], _ ) -> false
    | (t1 :: xs, t2 :: ys) -> subtype c t2 t1 && helper_funs c xs ys

and subtype_rt (c : Tctxt.t) (r1 : Ast.ret_ty) (r2 : Ast.ret_ty) : bool = 
  match (r1, r2) with 
    | (Ast.RetVoid, Ast.RetVoid) -> true
    | (Ast.RetVal t1, Ast.RetVal t2) -> subtype c t1 t2
    | _ -> false
(* well-formed types -------------------------------------------------------- *)
(* Implement a (set of) functions that check that types are well formed according
   to the H |- t and related inference rules

    - the function should succeed by returning () if the type is well-formed
      according to the rules

    - the function should fail using the "type_error" helper function if the 
      type is not well-formed

    - l is just an ast node that provides source location information for
      generating error messages (it's only needed for the type_error generation)

    - tc contains the structure definition context
 *)
let rec typecheck_ty (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ty) : unit =
  match t with 
    | TInt -> ()
    | TBool -> ()
    | TRef t -> typecheck_r l tc t
    | TNullRef t -> typecheck_r l tc t

and typecheck_r (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.rty) : unit = 
  match t with
    | RString -> ()
    | RArray t -> typecheck_ty l tc t
    | RStruct id -> begin try ignore(lookup_struct id tc) with Not_found -> (type_error l id) end
    | RFun (ts, rt) -> (helper_funs_tc l tc ts); (typecheck_rt l tc rt)

and typecheck_rt (l : 'a Ast.node) (tc : Tctxt.t) (t : Ast.ret_ty) : unit = match t with 
  | RetVoid -> ()
  | RetVal t -> typecheck_ty l tc t
and helper_funs_tc (l : 'a Ast.node) (tc : Tctxt.t) (ts : Ast.ty list) : unit = match ts with 
  | [] -> ()
  | t::xs -> typecheck_ty l tc t; helper_funs_tc l tc xs

(* typechecking expressions ------------------------------------------------- *)
(* Typechecks an expression in the typing context c, returns the type of the
   expression.  This function should implement the inference rules given in the
   oad.pdf specification.  There, they are written:

       H; G; L |- exp : t

   See tctxt.ml for the implementation of the context c, which represents the
   four typing contexts: H - for structure definitions G - for global
   identifiers L - for local identifiers

   Returns the (most precise) type for the expression, if it is type correct
   according to the inference rules.

   Uses the type_error function to indicate a (useful!) error message if the
   expression is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   Notes: - Structure values permit the programmer to write the fields in any
   order (compared with the structure definition).  This means that, given the
   declaration struct T { a:int; b:int; c:int } The expression new T {b=3; c=4;
   a=1} is well typed.  (You should sort the fields to compare them.)

*)
let rec typecheck_exp (c : Tctxt.t) (e : Ast.exp node) : Ast.ty = 

  match e.elt with 
  | (CNull rty) -> TNullRef rty
  | CBool _ -> TBool
  | CInt _ -> TInt
  | CStr _ -> TRef RString
  | Id id -> 
      (match lookup_option id c with 
        | Some t -> t 
        | None -> type_error e id)

  | CArr (ty, exps) -> 
      typecheck_ty e c ty;
      List.iter (fun exp ->
        let ti = typecheck_exp c exp in
        if not (subtype c ti ty)
        then type_error exp "CArr failed"
        ) exps;
        TRef (RArray ty)
    
  | NewArr (ty, exp1, id, exp2) ->
      (*check that the element type is well-formed*)
      typecheck_ty e c ty;
      (*size of new array is indeed int*)
      let t1 = typecheck_exp c exp1 in
      if t1 <> TInt then type_error e "NewArr failed: contained in L";
      (*check if id is already bound locally*)
      let opt = lookup_local_option id c in
      (match opt with 
        |Some t -> type_error e "NewArr failed: id already bound"
        |None -> ());
      (*check if we initialize with types that are allowed*)
      let tprime = typecheck_exp (add_local c id TInt) exp2 in
      if not (subtype c tprime ty) then type_error e 
        "NewArr failed: initialized to illegal value";
      (*finally return the type*)
      (TRef (RArray ty))
  
  | Index (e1, e2) -> 
      let t = typecheck_exp c e1 in
      let t2 = typecheck_exp c e2 in 
      if t2 <> TInt then type_error e "Index failed: t2 not int";
      t
  
  | Length e -> 
      let t = typecheck_exp c e in
      (match t with 
        | TRef (RArray _) -> TInt
        | _ -> type_error e "Length called on non array")

  | CStruct (id, list) -> 
      (*does this struct template exist?*)
      (match lookup_struct_option id c with
        | Some _ -> ()
        | None -> type_error e "Struct template doesn't exist");
        (*if it exists, do the field names and types match of the template and the given list?*)
        (*do both lists have equal length? -> try zipping*)
        let s = lookup_struct id c in
        (*compile expressions to types for comparison with template struct*)
        let compiled_list = List.map (fun (id, expr) -> (id, typecheck_exp c expr)) list in
        (try ((let zipped_fields = List.combine s compiled_list in 
        (*iterate through template and given list to verify that names (id) and types are ok*)
        List.iter 
        (fun (field, (id, ty)) ->
          if not (field.fieldName = id) then type_error e "Struct: field name mismatch";
          if not (subtype c ty field.ftyp) then type_error e "not all types of struct are matching") 
          zipped_fields);
          (TRef (RStruct id)))
        with Invalid_argument _ -> type_error e "struct failed: different length")

  | Proj (exp, id) -> 
    (*compile expression to (hopefully) a struct type*)
    let ty = typecheck_exp c exp in
    (match ty with
      | (TRef (RStruct ids)) -> 
        (*check if field exists and if struct has been defined in context*)
        (match lookup_field_option ids id c with 
            | Some ty -> ty
            | None -> type_error e "Struct type is not defined or field doesn't exist")
      | _ -> type_error e "expression is not representing a struct") 
  
  | Call (exp, list) -> 
    (*what's the function type?*)
    let ty = typecheck_exp c exp in 
    (match ty with 
      | TRef (RFun (tys, ret_ty)) -> 
        (*do we (1) provide the exact amount of args*)
        let compiled_list = List.map (fun expr -> typecheck_exp c expr) list in
        if not ((List.length compiled_list) = (List.length tys)) then type_error e "too much/few args provided";
        (*and (2) are arguments of legal type (subtype of defined i-th function arg)*)
        List.iter (fun (t1, t2) -> 
          if not (subtype c t1 t2) then type_error e "some args are not subtypes") 
            (List.combine compiled_list tys);
        (*then return the ret type*)
        (match ret_ty with 
          | RetVoid -> failwith "cannot call void function inside of expression"
          | RetVal ty -> ty)
      | _ -> type_error e "call expr is not a function")

  | Bop (op, exp, exp2) -> 
    let ty1 = typecheck_exp c exp in
    let ty2 = typecheck_exp c exp2 in
    (match op with 
      | Eq | Neq -> if (subtype c ty1 ty2) && (subtype c ty2 ty1) then TBool
                    else type_error e "not same type" 
      | _ -> 
        let (l, r, ret) = typ_of_binop op in 
        if (ty1 = l && ty2 = r) then ret 
        else type_error e "bop error")

  | Uop (unop, exp) -> 
      let comp_t = typecheck_exp c exp in 
      let (input, output) = typ_of_unop unop in
      if input = comp_t then output
      else type_error e "not matchinggg"

(* statements --------------------------------------------------------------- *)

(* Typecheck a statement 
   This function should implement the statement typechecking rules from oat.pdf.  

   Inputs:
    - tc: the type context
    - s: the statement node
    - to_ret: the desired return type (from the function declaration)

   Returns:
     - the new type context (which includes newly declared variables in scope
       after this statement
     - A boolean indicating the return behavior of a statement:
        false:  might not return
        true: definitely returns 

        in the branching statements, both branches must definitely return

        Intuitively: if one of the two branches of a conditional does not 
        contain a return statement, then the entier conditional statement might 
        not return.
  
        looping constructs never definitely return 

   Uses the type_error function to indicate a (useful!) error message if the
   statement is not type correct.  The exact wording of the error message is
   not important, but the fact that the error is raised, is important.  (Our
   tests also do not check the location information associated with the error.)

   - You will probably find it convenient to add a helper function that implements the 
     block typecheck rules.
*)
let rec typecheck_stmt (tc : Tctxt.t) (s:Ast.stmt node) (to_ret:ret_ty) : Tctxt.t * bool =
  match s.elt with 
    | Decl (id, exp) -> 
      let ty = typecheck_exp tc exp in
      (match lookup_local_option id tc with 
        | Some _ -> type_error exp "variable already defined in Locals"
        | None -> (add_local tc id ty, false))
    | Assn (e1, e2) -> 
      let t1 = typecheck_exp tc e1 in 
      let t2 = typecheck_exp tc e2 in
      (match e1.elt with 
        | Id id -> 
          (*make sure we're not overwriting a global function*)
          (match lookup_local_option id tc with 
            | Some _ -> ()
            | None -> 
              (match lookup_global_option id tc with 
                | Some (TRef (RFun _)) -> type_error e1 "tried to overwrite global function"
                | Some _ -> ()
                | _ -> type_error e1 "variable not found")
          )
        | _ -> ());
      (*make sure rhs is subtype of lhs*)
      if not (subtype tc t2 t1) then type_error e1 "subtype check failed in Assn"
      else (tc, false)
    | SCall (e1, list) -> 
      let fun_ty = typecheck_exp tc e1 in
      (match fun_ty with 
        | TRef (RFun (args, RetVoid)) -> 
          let compiled_list = List.map (typecheck_exp tc)  list in
          (try (
              if (List.exists (fun (t1, t2) -> not (subtype tc t1 t2)) (List.combine compiled_list args)) 
              then type_error e1 "subtyper failed in SCall"
              else (tc, false))
          with Invalid_argument _-> 
              type_error e1 "different length in Scall")
        | _ -> type_error e1 "non void function or not even a function")

    | If (cnd, b1, b2) -> 
      let ty_cnd = typecheck_exp tc cnd in
      if ty_cnd <> TBool then type_error cnd "not a bool in cnd field of IF"
      else
      let (_, ret1) = typecheck_block tc b1 to_ret in
      let (_, ret2) = typecheck_block tc b2 to_ret in
      (tc, ret1 && ret2)
      
    | Cast (ref, id, e, b1, b2) -> 
      let t1 = typecheck_exp tc e in 
      (match t1 with 
        |TNullRef (rt) -> 
          if (not (subtype_ref tc rt ref)) then type_error s "Cast: not subtype"
          else 
            let ctxt1 = add_local tc id (TRef ref) in
            let (_, r1) = typecheck_block ctxt1 b1 to_ret in
            let (_, r2) = typecheck_block tc b2 to_ret in
            (tc, r1 && r2)
        | _ -> type_error s "invalid cast")
    | While (cnd, b) -> 
      let t = typecheck_exp tc cnd in
      if(t <> TBool) then type_error s "while condition not bool"
      else 
        let (_, _) = typecheck_block tc b to_ret in
        (tc, false)
    | For (vdecls, e_cond_opt, s_update_opt, b_body) -> failwith "ToDo"

    | Ret (Some exp) ->
      let t = typecheck_exp tc exp in  
      (match to_ret with 
        | RetVoid -> type_error s "got non void return val"
        | RetVal x -> 
          if (not (subtype tc t x)) then type_error s "return types dont match"
          else (tc, true))

    | Ret (None) -> 
      match to_ret with 
        |RetVoid -> tc, true
        |_ -> type_error s "expected return value, got void"

and typecheck_block (tc : Tctxt.t) (sts : Ast.stmt node list) (to_ret : ret_ty) : Tctxt.t * bool = 
  match sts with 
    | [] -> (tc, false)
    | s::zs -> 
      let (new_ctxt, returns) = typecheck_stmt tc s to_ret in 
      if not returns then typecheck_block new_ctxt zs to_ret 
      else 
        (match zs with 
          | [] -> (new_ctxt, true)
          | _-> type_error s "code after guaranteed return")

(* struct type declarations ------------------------------------------------- *)
(* Here is an example of how to implement the TYP_TDECLOK rule, which is 
   is needed elswhere in the type system.
 *)

(* Helper function to look for duplicate field names *)
let rec check_dups fs =
  match fs with
  | [] -> false
  | h :: t -> (List.exists (fun x -> x.fieldName = h.fieldName) t) || check_dups t

let typecheck_tdecl (tc : Tctxt.t) id fs  (l : 'a Ast.node) : unit =
  if check_dups fs
  then type_error l ("Repeated fields in " ^ id) 
  else List.iter (fun f -> typecheck_ty l tc f.ftyp) fs

(* function declarations ---------------------------------------------------- *)
(* typecheck a function declaration 
    - extends the local context with the types of the formal parameters to the 
      function
    - typechecks the body of the function (passing in the expected return type
    - checks that the function actually returns
*)
let rec check_dups_args args = 
  match args with 
    | [] -> false 
    | y::ys -> (List.exists (fun x -> snd x = snd y) ys) || check_dups_args ys
let typecheck_fdecl (tc : Tctxt.t) (f : Ast.fdecl) (l : 'a Ast.node) : unit =
  (*add args to context*)
  let new_ctxt = List.fold_left (fun acc (ty, id) -> add_local acc id ty) tc f.args in
  (*check uniqueness of argument names*)
  if (check_dups_args f.args) then type_error l "argument names are not distinct";
  (*iterate through each statement of body with newest context, if no errors raised return void*)
  if not (snd (typecheck_block new_ctxt f.body f.frtyp)) then type_error l "block check failed, not guaranteed return"
  else ()

(* creating the typchecking context ----------------------------------------- *)

(* The following functions correspond to the
   judgments that create the global typechecking context.

   create_struct_ctxt: - adds all the struct types to the struct 'H'
   context (checking to see that there are no duplicate fields

     H |-s prog ==> H'


   create_function_ctxt: - adds the the function identifiers and their
   types to the 'G' context (ensuring that there are no redeclared
   function identifiers)

     H ; G1 |-f prog ==> G2


   create_global_ctxt: - typechecks the global initializers and adds
   their identifiers to the 'G' global context

     H ; G1 |-g prog ==> G2    


   NOTE: global initializers may mention function identifiers as
   constants, but can't mention other global values *)

let create_struct_ctxt (p:Ast.prog) : Tctxt.t =
  List.fold_left (fun ctxt decl -> 
    (match decl with 
      | Gtdecl tdecl -> 
        (*check if struct is already defined in current ctxt*)
        (match (lookup_struct_option (fst tdecl.elt) ctxt) with 
          | Some _ -> type_error tdecl "struct has been declared before"
          (*new struct thus add it to ctxt and move to next declaration*)
          | _ -> add_struct ctxt (fst tdecl.elt) (snd tdecl.elt))
      (*not a struct declaration thus skip*)
      | _ -> ctxt)
    
    ) Tctxt.empty p   

let create_function_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  List.fold_left (fun ctxt decl -> 
    (match decl with 
      | Gfdecl ({elt = f} as l) -> 
        (*check uniqueness of fname*)
        (match lookup_global_option f.fname ctxt with 
          | Some _ -> type_error l "the function has already been declared somehwere"
          | None -> 
            (*add function definition to context and move to next declaration*)
            let arg_tys = List.map fst f.args in
            let fun_ty = (TRef (RFun (arg_tys, f.frtyp))) in
            (*make sure func signature is well formed*)
            typecheck_ty l ctxt fun_ty; 
            add_global ctxt f.fname fun_ty)
      (*skip struct and global vars*)
      | _ -> ctxt)
    ) tc p



let create_global_ctxt (tc:Tctxt.t) (p:Ast.prog) : Tctxt.t =
  List.fold_left (fun ctxt decl ->
    (match decl with 
      |Gvdecl ({elt = g} as l) -> 
        (*type check exp using the tc, this allows us to only use local variables and not reference
         any other globals which is not allowed*)
        let exp_ty = typecheck_exp tc g.init in 
        (*check if label exists already*)
        (match lookup_global_option g.name ctxt with 
          | Some _ -> type_error l "this global var already exists"
          | None -> add_global ctxt g.name exp_ty)
      | _ -> ctxt)
        ) tc p
(* This function implements the |- prog and the H ; G |- prog 
   rules of the oat.pdf specification.   
*)
let typecheck_program (p:Ast.prog) : unit =
  let sc = create_struct_ctxt p in
  let fc = create_function_ctxt sc p in
  let tc = create_global_ctxt fc p in
  List.iter (fun p ->
    match p with
    | Gfdecl ({elt=f} as l) -> typecheck_fdecl tc f l
    | Gtdecl ({elt=(id, fs)} as l) -> typecheck_tdecl tc id fs l 
    | _ -> ()) p
