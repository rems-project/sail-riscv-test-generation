open Riscv
open Sail_lib

let remove_compressed g =
  let instrs =
    List.filter (fun s -> String.length s < 3 || s.[0] <> 'C' || s.[1] <> '_')
    constructors_zast
  in
  let mk_ast g =
    build_zast g (rand_choice instrs)
  in
  { g with gen_zast = mk_ast }
