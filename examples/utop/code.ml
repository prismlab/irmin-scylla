Irmin KV eg
#require "digestif.ocaml";;
#require "lwt.unix";;
#require "irmin";;
#require "irmin-scylla";;
#require "checkseum.ocaml";;
#require "irmin-unix";;
#require "lwt.unix";;

(* Creating a repo *)
module Scylla_kvStore = Irmin_scylla.KV(Irmin.Contents.String);;      (*creating the module with the set of functions*)
let t = Irmin_scylla.config "172.17.0.2";;   (*configuration values related to irmin_scylla store*)   (*val t : Irmin.config = <abstr>*)
let repo = Lwt_main.run @@ Scylla_kvStore.Repo.v t;;   (*kvrepo is like .git, which is initialized with the configurations specified in t. *)  (*val repo : Scylla_kvStore.repo = <abstr>*)
let branch_master = Lwt_main.run @@ Scylla_kvStore.master repo;;   (*KVStore.t*) (*creating master branch in the repo. It will be pointed and used by the string name "branch_master"*)  (*val branch_master : Scylla_kvStore.t = <abstr>*)
Scylla_kvStore.Branch.list repo;;  (*printing the list of all branches yet, result is empty*)
(*Scylla_kvStore.set_exn ~info:(Irmin_unix.info "entering data into master") branch_master ["foor";"barr"] "testing 123";;   (*setting some key and value in master branch*)*)
Scylla_kvStore.set_exn ~info:(fun () -> Irmin.Info.empty) branch_master ["foor";"barr"] "testing 123";;
Scylla_kvStore.get branch_master ["foor";"barr"];;   (*printing "testing 123"*)
Scylla_kvStore.Branch.list repo;;  (*printing the list of all branches yet, master is returned*)
Scylla_kvStore.clone branch_master "branch1" ;; (*clones the master branch into a new branch called as "branch1" *)  (*Scylla_kvStore.t = <abstr>*)
Scylla_kvStore.Branch.list repo;;  (*printing the list of all branches yet, master and branch1 are returned*)
Scylla_kvStore.get branch1 ["foor";"barr"];; (*branch1 is the name of the branch but a pointer is needed to access it.*)
let cloned_branch = Lwt_main.run @@ Scylla_kvStore.clone branch_master "branch1" ;;  (*val cloned_branch : Scylla_kvStore.t = <abstr>*)
Scylla_kvStore.Branch.list repo;;  (*verifying the effect of above command, nothing changes*)
Scylla_kvStore.get cloned_branch ["foor";"barr"];; (*printing output same as master branch*)
Scylla_kvStore.set_exn ~info:(Irmin_unix.info "update1 to master") branch_master ["key_in_master";] "master value";;
Scylla_kvStore.set_exn ~info:(Irmin_unix.info "update1 to branch1") cloned_branch ["key_in_clone";] "clone value";;
Scylla_kvStore.get cloned_branch ["key_in_clone"];; (*prints "clone value"*)
Scylla_kvStore.get cloned_branch ["key_in_master"];; (*Exception: (Invalid_argument "Irmin.Tree.get: /key_in_master not found")*)
Scylla_kvStore.get branch_master ["key_in_clone"];;  (*Exception: (Invalid_argument "Irmin.Tree.get: /key_in_clone not found")*)
Scylla_kvStore.get branch_master ["key_in_master"];; (*prints "master value"*)
Scylla_kvStore.Branch.list repo;;  (*checking the list of branches before operating on them*)
Scylla_kvStore.merge_into ~info:(Irmin_unix.info "merging branch1 into master") cloned_branch ~into:branch_master;; (*only branch pointers can be used for any operations*)
Scylla_kvStore.Branch.list repo;; (*still prints two branches*)
(*is it possible to get the list of keys in a branch*)
(*print clones data from master*)
Scylla_kvStore.get branch_master ["key_in_clone"];;  (*prints "clone value"*)
Scylla_kvStore.get cloned_branch ["key_in_master"];;  (*still gives error like earlier*)
(*create conflict and merge*)
(*whole tree in the repo has to be committed. So get the tree first*)
let tree = Lwt_main.run @@ Scylla_kvStore.tree branch_master;;  (*val tree : Scylla_kvStore.tree = `Node <abstr>*)
let commit = Lwt_main.run @@ Scylla_kvStore.Commit.v repo ~info:(Irmin.Info.empty) ~parents:[] tree;;  (*val commit : Scylla_kvStore.commit = <abstr>*)  (*kv1 from the test code*)
-----
Scylla_kvStore.Branch.set repo "foo" commit;;
Scylla_kvStore.Branch.find repo "foo";; (*Scylla_kvStore.commit option = Some <abstr>*)
Scylla_kvStore.Branch.list repo;;
-----
------
let ins = Lwt_main.run @@ Scylla_kvStore.get_tree branch_master [];;
Scylla_kvStore.Tree.inspect ins;;
------

Scylla_kvStore.Branch.get repo "master";; (*I think its returning hte pointer to the brnach*)
Scylla_kvStore.Branch.scylla repo "branch1";; (*checks if the branch is the part of the repo*) (*returns true*)

Scylla_kvStore.Branch.set repo "set1" commit;; (*It is checking out a new branch named "set1" which is a copy of committed branch*)(*returns unit*)
Scylla_kvStore.Branch.list repo;;  (*string list = ["set1"; "master"; "branch1"]*)
let branch_set1 = Lwt_main.run @@ Scylla_kvStore.Branch.get repo "set1";;
Scylla_kvStore.get branch_set1 ["key_in_clone"];;
Scylla_kvStore.get cloned_branch ["key_in_master"];;

