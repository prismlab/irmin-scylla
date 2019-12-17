(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix

type cassSession

type cassStatement

type cassCluster

type cassFuture

type cassError

type cassResult

type cassRow

type cassValue

type cassIterator

external ml_cass_session_new : unit -> cassSession = "cass_session_new"

external ml_cass_cluster_new : unit -> cassCluster = "cass_cluster_new"

external ml_cass_cluster_set_contact_points : cassCluster -> string -> unit
  = "cass_cluster_set_contact_points"

external ml_cass_session_connect : cassSession -> cassCluster -> cassFuture
  = "cass_session_connect"

external ml_cass_future_wait : cassFuture -> unit = "cass_future_wait"

external ml_cass_future_error_code : cassFuture -> cassError
  = "cass_future_error_code"

external ml_cass_cluster_free : cassCluster -> unit = "cass_cluster_free"

external ml_cass_session_free : cassSession -> unit = "cass_session_free"

external ml_cass_statement_new : string -> int -> cassStatement
  = "cass_statement_new"

external ml_cass_session_execute : cassSession -> cassStatement -> cassFuture
  = "cass_session_execute"

external ml_cass_future_free : cassFuture -> unit = "cass_future_free"

external ml_cass_statement_free : cassStatement -> unit = "cass_statement_free"

external ml_cass_statement_bind_string : cassStatement -> int -> string -> unit
  = "cass_statement_bind_string"

external ml_cass_future_get_result : cassFuture -> cassResult
  = "cass_future_get_result"

external ml_cass_result_first_row : cassResult -> cassRow
  = "cass_result_first_row"

external ml_cass_row_get_column : cassRow -> int -> cassValue
  = "cass_row_get_column"

external ml_cass_iterator_from_result : cassResult -> cassIterator
  = "cass_iterator_from_result"

external ml_cass_result_row_count : cassResult -> int = "cass_result_row_count"

external ml_cass_iterator_next : cassIterator -> bool = "cass_iterator_next"

external ml_cass_iterator_get_row : cassIterator -> cassRow
  = "cass_iterator_get_row"

external ml_cass_row_get_column_by_name : cassRow -> string -> cassValue
  = "cass_row_get_column_by_name"

external ml_cass_session_close : cassSession -> cassFuture
  = "cass_session_close"

external cstub_get_string : cassValue -> string = "get_string"

external cstub_convert : int -> int = "convert"

external cstub_match_enum : cassError -> cassFuture -> bool = "match_enum"

external cstub_convert_to_bool : bool -> bool = "convert_to_bool"

external cstub_convert_to_ml : int -> int = "convert_to_ml"

let get_error_code future statement =
  let rc = ml_cass_future_error_code future in
  let response = cstub_match_enum rc future in
  ml_cass_future_free future;
  ml_cass_statement_free statement;

  response

let create_cluster hosts =
  let cluster = ml_cass_cluster_new () in
  ml_cass_cluster_set_contact_points cluster hosts;
  cluster

let connect_session sess cluster =
  let future = ml_cass_session_connect sess cluster in
  ml_cass_future_wait future;
  let rc = ml_cass_future_error_code future in
  let response = cstub_match_enum rc future in
  response

let tns_stmt session query keyStr testStr setStr =
  let valCount = cstub_convert 3 in
  let statement = ml_cass_statement_new query valCount in
  ml_cass_statement_bind_string statement (cstub_convert 0) setStr;
  ml_cass_statement_bind_string statement (cstub_convert 1) keyStr;
  ml_cass_statement_bind_string statement (cstub_convert 2) testStr;

  let future = ml_cass_session_execute session statement in
  ml_cass_future_wait future;

  get_error_code future statement

let del_stmt session query keyStr =
  let valCount = cstub_convert 1 in
  let statement = ml_cass_statement_new query valCount in
  ml_cass_statement_bind_string statement (cstub_convert 0) keyStr;

  let future = ml_cass_session_execute session statement in
  ml_cass_future_wait future;

  get_error_code future statement

let cx_stmt session query keyStr valStr =
  let valCount = cstub_convert 2 in
  let statement = ml_cass_statement_new query valCount in
  ml_cass_statement_bind_string statement (cstub_convert 0) keyStr;
  ml_cass_statement_bind_string statement (cstub_convert 1) valStr;

  let future = ml_cass_session_execute session statement in
  ml_cass_future_wait future;

  get_error_code future statement

let src = Logs.Src.create "irmin.mem" ~doc:"Irmin in-memory store"

module Log = (val Logs.src_log src : Logs.LOG)

let config () = Irmin.Private.Conf.empty

module Read_only (K : Irmin.Type.S) (V : Irmin.Type.S) = struct
  type key = K.t

  type value = V.t

  type 'a t = { mutable t : cassSession }

  let v _config =
    let sess = ml_cass_session_new () in
    let hosts = "127.0.0.1" in
    let cluster = create_cluster hosts in
    let response = connect_session sess cluster in
    match response with
    | false ->
        ml_cass_cluster_free cluster;
        ml_cass_session_free sess;
        let map = { t = sess } in
        Lwt.return map
    | true ->
        let map = { t = sess } in
        Lwt.return map

  let close t =
    let future = ml_cass_session_close t.t in
    ml_cass_future_wait future;

    ignore @@ ml_cass_future_free future;

    Lwt.return_unit

  let pp_key = Irmin.Type.pp K.t

  let find { t; _ } key =
    Log.debug (fun f -> f "find %a" pp_key key);

    let keyStr = Irmin.Type.to_string K.t key in
    print_string ("\nRO.find: key = " ^ keyStr ^ "\n");

    let query =
      "select value from employee.table1 where key = '" ^ keyStr ^ "'"
    in
    let statement = ml_cass_statement_new query (cstub_convert 0) in
    let future = ml_cass_session_execute t statement in
    ml_cass_future_wait future;

    let rc = ml_cass_future_error_code future in
    let response = cstub_match_enum rc future in
    if response then
      let result = ml_cass_future_get_result future in
      let rowcount = ml_cass_result_row_count result in
      if cstub_convert_to_ml rowcount > 0 then (
        let row = ml_cass_result_first_row result in
        let value = ml_cass_row_get_column row (cstub_convert 0) in
        let valStr = cstub_get_string value in
        ml_cass_future_free future;
        ml_cass_statement_free statement;

        match Irmin.Type.of_string V.t valStr with
        | Ok s -> Lwt.return_some s
        | _ -> Lwt.return_none )
      else (
        ml_cass_future_free future;
        ml_cass_statement_free statement;

        Lwt.return_none )
    else (
      ml_cass_future_free future;
      ml_cass_statement_free statement;

      Lwt.return_none )

  let mem { t; _ } key =
    Log.debug (fun f -> f "mem %a" pp_key key);
    (let map = { t } in
     find map key)
    >>= fun v ->
    match v with Some _ -> Lwt.return true | None -> Lwt.return false

  let cast t = (t :> [ `Read | `Write ] t)

  let batch t f = f (cast t)
end

module Append_only (K : Irmin.Type.S) (V : Irmin.Type.S) = struct
  include Read_only (K) (V)

  let add t key value =
    Log.debug (fun f -> f "add -> %a" pp_key key);

    let keyStr = Irmin.Type.to_string K.t key in
    let valStr = Irmin.Type.to_string V.t value in
    let query = "INSERT INTO employee.table1 (key, value) VALUES (?, ?)" in
    ignore @@ cx_stmt t.t query keyStr valStr;
    Lwt.return_unit
end

module Atomic_write (K : Irmin.Type.S) (V : Irmin.Type.S) = struct
  module RO = Read_only (K) (V)
  module W = Irmin.Private.Watch.Make (K) (V)
  module L = Irmin.Private.Lock.Make (K)

  exception WatchNotImplemented

  type t = { t : unit RO.t; w : W.t; lock : L.t }

  (* argument for t (unit) is irrelevant, 
										but the value passed should be a valid constructor*)

  type key = RO.key

  type value = RO.value

  type watch = W.watch

  let watches = W.v () (*raise WatchNotImplemented*)

  let lock = L.v ()

  (* { t = sess} *)
  let v config = RO.v config >>= fun t -> Lwt.return { t; w = watches; lock }

  (*equivalent of what is returned above is: {t = { t = sess}; w = watches; lock} *)
  let close t = W.clear t.w >>= fun () -> RO.close t.t

  let aw_find t key =
    (* Log.debug (fun f -> f "find %a" pp_key key); *)
    let keyStr = Irmin.Type.to_string K.t key in
    let query =
      "select value from employee.office where key = '" ^ keyStr ^ "'"
    in
    let statement = ml_cass_statement_new query (cstub_convert 0) in
    let future = ml_cass_session_execute t statement in
    ml_cass_future_wait future;

    let rc = ml_cass_future_error_code future in
    let response = cstub_match_enum rc future in
    if response then
      let result = ml_cass_future_get_result future in
      let rowcount = ml_cass_result_row_count result in
      if cstub_convert_to_ml rowcount > 0 then (
        let row = ml_cass_result_first_row result in
        let value = ml_cass_row_get_column row (cstub_convert 0) in
        let valStr = cstub_get_string value in
        ml_cass_future_free future;
        ml_cass_statement_free statement;

        match Irmin.Type.of_string V.t valStr with
        | Ok s -> Lwt.return_some s
        | _ -> Lwt.return_none )
      else (
        ml_cass_future_free future;
        ml_cass_statement_free statement;

        Lwt.return_none )
    else (
      ml_cass_future_free future;
      ml_cass_statement_free statement;

      Lwt.return_none )

  let find t = aw_find t.t.t

  let aw_mem t key =
    aw_find t key >>= fun v ->
    match v with Some _ -> Lwt.return true | None -> Lwt.return false

  let mem t = aw_mem t.t.t

  let watch_key t =
    ignore t;
    raise WatchNotImplemented

  let watch t =
    ignore t;
    raise WatchNotImplemented

  let unwatch t =
    ignore t;
    raise WatchNotImplemented

  let rec func rows =
    match cstub_convert_to_bool (ml_cass_iterator_next rows) with
    | true -> (
        let row = ml_cass_iterator_get_row rows in
        let key_col = ml_cass_row_get_column_by_name row "key" in
        let st = cstub_get_string key_col in
        match Irmin.Type.of_string K.t st with
        | Ok s -> s :: func rows
        | _ -> [] )
    | false -> []

  let list t =
    print_string "\nAW.list: fetches the list of keys in office table\n";
    Log.debug (fun f -> f "list");

    let valCount = cstub_convert 0 in
    let query = "select key from employee.office" in
    let statement = ml_cass_statement_new query valCount in
    let future = ml_cass_session_execute t.t.t statement in
    ml_cass_future_wait future;

    let future = ml_cass_session_execute t.t.t statement in
    ml_cass_future_wait future;

    let rc = ml_cass_future_error_code future in
    let response = cstub_match_enum rc future in
    if response = true then (
      let result = ml_cass_future_get_result future in
      let rows = ml_cass_iterator_from_result result in
      let lst = func rows in
      print_int (List.length lst);

      ml_cass_future_free future;
      ml_cass_statement_free statement;

      lst |> Lwt.return )
    else Lwt.return []

  let set t key value =
    L.with_lock t.lock key (fun () ->
        let query = "INSERT INTO employee.office (key, value) VALUES (?, ?)" in
        let keyStr = Irmin.Type.to_string K.t key in
        let valStr = Irmin.Type.to_string V.t value in
        ignore @@ cx_stmt t.t.t query keyStr valStr;

        Lwt.return_unit)

  let remove t key =
    L.with_lock t.lock key (fun () ->
        let keyStr = Irmin.Type.to_string K.t key in
        let query = "DELETE from employee.office WHERE key = ?" in
        ignore @@ del_stmt t.t.t query keyStr;

        Lwt.return_unit)

  let test_and_set t key ~test ~set =
    Log.debug (fun f -> f "test_and_set");

    L.with_lock t.lock key (fun () ->
        let keyStr = Irmin.Type.to_string K.t key in
        let testStr =
          match test with Some v -> Irmin.Type.to_string V.t v | None -> ""
        in
        let setStr =
          match set with Some v -> Irmin.Type.to_string V.t v | None -> ""
        in
        let tns =
          match setStr with
          | "" ->
              let query = "DELETE from employee.office WHERE key = ?" in
              let response = del_stmt t.t.t query keyStr in
              if response then Lwt.return true else Lwt.return false
          | _ ->
              if (*IF makes the transaction light weight*)
                 testStr = "" then (
                let query =
                  "INSERT INTO employee.office (key, value) VALUES (?, ?)"
                in
                ignore @@ cx_stmt t.t.t query keyStr setStr;
                Lwt.return true )
              else
                let query =
                  "UPDATE employee.office SET value = ? WHERE key = ? IF \
                   value = ?"
                in
                let _ = tns_stmt t.t.t query keyStr testStr setStr in
                (*do it more efficiently*)
                find t key >>= fun findval ->
                let valu =
                  match findval with
                  | Some v -> Irmin.Type.to_string V.t v
                  | None -> ""
                in
                if valu = setStr then Lwt.return true else Lwt.return false
        in
        tns)
end

module Make =
  Irmin.Make (Irmin.Content_addressable (Append_only)) (Atomic_write)
module KV (C : Irmin.Contents.S) =
  Make (Irmin.Metadata.None) (C) (Irmin.Path.String_list) (Irmin.Branch.String)
    (Irmin.Hash.BLAKE2B)
