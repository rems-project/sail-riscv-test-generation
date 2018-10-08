open Riscv
open Sail_lib

let frequency l =
  let total = List.fold_left (+) 0 (List.map fst l) in
  if total = 0 then failwith "frequency: nothing to choose between" else
    let n = Random.int total in
    let rec pick n = function
      | [] -> assert false
      | (m,h)::t -> if n < m then h else pick (n-m) t
    in pick n l

let remove_compressed g =
  let instrs =
    List.filter (fun s -> String.length s < 3 || s.[0] <> 'C' || s.[1] <> '_')
    constructors_zast
  in
  let mk_ast g =
    build_zast g (rand_choice instrs)
  in
  { g with gen_zast = mk_ast }

let restrict_registers g =
  (* Make sure there's overlap between the full register suite and those
     representable in the compressed format. *)
  let reg_list =
    [1, 0;
     2, 1;
     2, 8;
     2, 9] in
  let creg_list =
    [1, 0;
     1, 1;
     1, 2]
  in
  let regs = List.map (fun (fq,i) -> (fq, bits_of_int 16 i)) reg_list in
  let cregs = List.map (fun (fq,i) -> (fq, bits_of_int 4 i)) creg_list in
  { g with gen_zregbits = (fun _ -> frequency regs);
           gen_zcregbits = (fun _ -> frequency cregs) }
