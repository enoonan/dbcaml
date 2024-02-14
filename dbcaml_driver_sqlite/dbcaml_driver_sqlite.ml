open Sqlite3

let ( let* ) = Result.bind

module Sqlite = struct
  type config = {
    name: string;
    vfs: string option;
    cache: [ `SHARED | `PRIVATE ] option;
    mutex: [ `NO | `FULL ] option;
    memory: bool option;
    uri: bool option;
    mode: [ `READONLY | `NO_CREATE ] option;
  }

  let connect config =
    let mode = config.mode in
    let uri = config.uri in
    let memory = config.memory in
    let mutex = config.mutex in
    let cache = config.cache in
    let vfs = config.vfs in

    let c = db_open ?mode ?uri ?memory ?mutex ?cache ?vfs config.name in

    let execute (conn : db) (params : Dbcaml.Param.t list) query :
        ( Dbcaml.Row.t list,
          Dbcaml.ErrorMessages.execution_error )
        Dbcaml.ErrorMessages.result =
      let prepared = prepare conn query in

      (*
         Questions about this: Dbcaml.Param.t only specifies string types.
         Sqlite3 package allows for others - bool, float, int, blob etc.
         So, what do?
      *)
      let bind_params idx text =
        match bind_text prepared (idx + 1) text with
        | Rc.OK
        | Rc.DONE ->
          ()
        | x -> failwith (Rc.to_string x)
      in

      List.iteri bind_params params;

      let query_type =
        match List.nth_opt (String.split_on_char ' ' query) 0 with
        | None -> ""
        | Some x ->
          (match String.lowercase_ascii x with
          | "select" -> "do select"
          | _ -> "do others")
      in

      (* Execute query based on query type - iterate selects *)
      query_type
      (* How to handle errors etc *)
      (* How to get results into Dbcaml.Row.t list format *)
    in

    let* conn = Dbcaml.Connection.make ~conn:c ~execute () in
    Ok conn
end
