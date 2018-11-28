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

let rec any x = function
  | [] -> false
  | p::ps -> p x || any x ps

let restrict_instructions restrictions g =
  let pred s = not (any s restrictions) in
  let instrs = List.filter pred constructors_zast in
  let mk_ast g =
    build_zast g (rand_choice instrs)
  in
  { g with gen_zast = mk_ast }

let startswith s s' =
  if String.length s' >= String.length s then
    String.sub s' 0 (String.length s) = s
  else false

let unsupported = [
    (* Arbitrary nonsense *)
    String.equal "ILLEGAL"; String.equal "C_ILLEGAL";
    (* AMO *)
    startswith "AMO"; String.equal "LOADRES"; String.equal "STORECON";
    (* RMEM sentinels *)
    String.equal "STOP_FETCHING"; String.equal "THREAD_START";
    (* CSR are expected to be different *)
    String.equal "CSR"
  ]

let remove_unsupported g =
  restrict_instructions unsupported g

let remove_compressed g =
  restrict_instructions (startswith "C_"::unsupported) g

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
