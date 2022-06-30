open Ast
open Exceptions
open Lisptype

module VariableMap = Map.Make (struct
    type t = string

    let compare = compare

  end)
(**Maps Funcion Parameters to Types for each parameter*)
module ParameterMap = Map.Make (struct type t = string

    let compare = compare
  end)

type variableEnv = lisptype VariableMap.t



let print_vmap v =
  VariableMap.iter (fun k x -> Printf.printf "%s:%s\n" k (type_to_string x)) v



module ScopeEnv = struct
  type t = {
    scopetype : lisptype;
    (*Sent downwards and upwards -| ^ inner exp type | V scope type *)
    toinfer : variableEnv;
    (*Sent upwards - ^ variable names are added to the map but then are updated on the way down  *)
    venv : variableEnv;
    (*Sent downwards - V variables are only available in the current scope and inner scopes*)
  }
  let empty = 
    {
      scopetype = TypeUndefined;
      toinfer = VariableMap.empty;
      venv = VariableMap.empty;
    }

  let update sc scopetype toinfer venv =
    {
      scopetype = scopetype;
      toinfer = toinfer;
      venv = venv;
    }
  let update_scopetype v sc = 
    update sc v sc.toinfer sc.venv
  let update_toinfer v sc = 
    update sc sc.scopetype v sc.venv
  let update_venv v sc = 
    update sc sc.scopetype sc.toinfer v 
  let print_toinfer sc=
    print_vmap sc.toinfer
  let print_venv sc=
    print_vmap sc.venv
    let merge_variable_map a b =
      VariableMap.merge (
        fun k xo yo -> 
          match xo,yo with
          | Some TypeUndefined, Some a when a <> TypeUndefined -> yo
          | Some a, Some TypeUndefined when a <> TypeUndefined -> xo
          | Some x, Some y -> yo
          | None, yo -> yo
          | xo, None -> xo) a b

  let merge_venv a b =
    update_venv (merge_variable_map a.venv b.venv) a

  (** `merge_toinfer a b` merges a's toinfer map with b's toinfer map and returns a with the merged map *)
  let merge_toinfer a b =
    update_toinfer (merge_variable_map a.toinfer b.toinfer) a

end
