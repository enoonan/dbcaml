module Connection = Connection
module Driver = Driver
module Row = Row
module ErrorMessages = Error
module Param = Param

module Dbcaml = struct
  let start_link (d : Driver.t) =
    match Driver.connect d with
    | Ok connection -> Ok connection
    | Error e -> Error e

  let fetch_one connection ?params query =
    match Connection.execute connection (Param.params params) query with
    | Ok rows ->
      (match rows with
      | [] -> Error ErrorMessages.NoRows
      | r -> Ok (List.hd r))
    | Error e -> Error e

  let fetch_many connection ?params query =
    match Connection.execute connection (Param.params params) query with
    | Ok rows -> Ok rows
    | Error e -> Error e
end
