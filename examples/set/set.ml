open Lwt.Infix

module Scylla_kvStore = Irmin_scylla.KV(Irmin.Contents.String)

let _ =

let conf = Irmin_scylla.config "172.17.0.2" in
Scylla_kvStore.Repo.v conf >>= fun repo ->
	Scylla_kvStore.master repo >>= fun b_master ->
		Scylla_kvStore.set_exn ~info:(fun () -> Irmin.Info.empty) 
		b_master ["foo";"bar"] "testing 1234"