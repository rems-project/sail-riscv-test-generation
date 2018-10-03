open Riscv
open Sail_lib

(* Loop until we get an instruction supported by the encoder *)
let rec generate () =
  try
    let instr = rand_gens.gen_zast rand_gens in
    print_endline (string_of_bits (zencdec_forwards instr) ^ " " ^ zassembly_forwards instr)
  with Match_failure _ -> generate ()
;;

Random.self_init ();;
generate ();;
