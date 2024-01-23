open Riot

type query = string

type Message.t += Query of query

type Message.t += ReadyStatus of string

module Logger = Logger.Make (struct
  let namespace = ["dbcaml"]
end)

module PgPool = struct
  type t = { max_connections: int }

  let default = { max_connections = 10 }

  let connect ~conninfo =
    let pid =
      spawn (fun () ->
          let c =
            match Connection.connect conninfo with
            | Ok c -> c
            | Error e -> failwith e
          in

          match receive () with
          | Query query ->
            let _ = Connection.send_message c query in
            Printf.printf "Got result: %s\n" query;
            ()
          | ReadyStatus status ->
            Logger.debug (fun f -> f "Got status: %s" status)
          | _ -> failwith "unknown message")
    in

    pid

  (*
    This function should create a connection pool and return the pool so we can interact with it.
    *)
  let connect ?(max_connections = 10) conninfo =
    let connection_manager_pid =
      spawn (fun () ->
          let connections =
            Array.make max_connections (connect ~conninfo) |> Array.to_list
          in

          Logger.debug (fun f ->
              f "Created %d connections" (List.length connections));
          (*
          * Recive a job and execute it. later on return the value back to the connection manager which will send it back to the client
          *)
          match receive () with
          | Query query ->
            let c = List.hd connections in
            Logger.debug (fun f -> f "Sending query to connection: %a" Pid.pp c);
            send c (Query query)
          | ReadyStatus status ->
            Logger.debug (fun f -> f "Got status: %s" status)
          | _ -> failwith "unknown message")
    in

    Logger.debug (fun f ->
        f "Booting Connection manager with PID: %a" Pid.pp (self ()));

    send connection_manager_pid (Query "SELECT * FROM users")
end