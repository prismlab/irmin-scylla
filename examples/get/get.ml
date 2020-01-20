open Lwt.Infix

module Scylla_kvStore = Irmin_scylla.KV(Irmin.Contents.String)

let _ =

let conf = Irmin_scylla.config "172.17.0.3" in
Scylla_kvStore.Repo.v conf >>= fun repo ->
	Scylla_kvStore.master repo >>= fun b_master ->
		Scylla_kvStore.get b_master ["foo";"bar"] >>= fun item ->
		print_string item;
		Lwt.return_unit
