open Riscv
open Sail_lib
open Generators

let set_bytes bytes off v =
  let rec build acc v =
    match v with
    | [] -> acc
    | b0::b1::b2::b3::b4::b5::b6::b7::t ->
       build (char_of_int (Nat_big_num.to_int (uint [b0;b1;b2;b3;b4;b5;b6;b7]))::acc) t
    | _ -> failwith "Bitlist length not a multiple of 8"
  in
  let rec set off =
    function
    | [] -> ()
    | h::t ->
       Bytes.set bytes off h;
       set (off+1) t
  in set off (build [] v)
;;

let gens, socket, num =
  let gens = ref rand_gens in
  let port = ref None in
  let num = ref 1 in
  let remove_compressed = ref false in
  let open Arg in
  let () = parse
    (align
       ["-p", String (fun p -> port := Some p), "<port> Provide an instruction generator service on this port";
        "-no_compressed", Set remove_compressed, " Generate non-compressed instructions";
        "-n", Set_int num, "<number> Number of instructions to generate";
        "-restrict_registers", Unit (fun () -> gens := restrict_registers !gens), " Apply a simple-minded restriction on the choice of registers"])
    (fun _ -> raise (Bad "Unexpected argument"))
    "RISC-V instruction generator"
  in (if !remove_compressed
     then Generators.remove_compressed rand_gens
     else Generators.remove_unsupported rand_gens),
     (match !port with
      | Some port ->
        let open Unix in
        let addr::_ = getaddrinfo "localhost" port [AI_FAMILY PF_INET; AI_SOCKTYPE SOCK_STREAM] in
        let sock = socket ~cloexec:true PF_INET SOCK_STREAM 0 in
        let () = setsockopt sock SO_REUSEADDR true in
        let () = bind sock addr.ai_addr in
        let () = listen sock 1 in
        Some sock
      | None -> None),
     !num
;;

let rec generate_one () =
  (* Loop until we get an instruction supported by the encoder *)
  let rec aux () =
    try
      let instr = gens.gen_zast gens in
      let bits =
        try zencdec_forwards instr
        with Match_failure _ -> zencdec_compressed_forwards instr
      in
      let asm =
        try zassembly_forwards instr
        with Match_failure _ -> "<not supported by assembler>"
      in
      let () = print_endline (string_of_bits bits ^ " " ^ asm) in
      instr, bits
    with Match_failure _ -> aux ()
  in aux ()
;;

let rec generate n =
  if n = 0 then () else
  let _ = generate_one () in
  generate (n-1)
;;

let rand_init_bytes buf =
  let rec aux i n =
    let n = int_of_char (Bytes.get buf i) + n * 256 in
    if i = 0 then n else aux (i-1) n
  in Random.init (aux 3 0)

let server sock =
     let buf = Bytes.make 4 '\000' in
     let rec aux () =
       if Unix.recv sock buf 0 4 [] <> 4
       then prerr_endline "Unable to read full seed"
       else
         let () = rand_init_bytes buf in
         let instr, bits = generate_one () in
(*         let bytes = Bytes.make 8 '\000' in
         let () = Bytes.set bytes 4 '\001' in
         let () = set_bytes bytes 0 bits in
         if Unix.send sock bytes 0 8 [] <> 8*)
         let bytes = Bytes.make 4 '\000' in
         let () = set_bytes bytes 0 bits in
         if Unix.send sock bytes 0 4 [] <> 4
         then prerr_endline "Unable to send full instruction"
         else aux ()
     in aux ()
;;

match socket with
| None ->
   let () = Random.self_init () in
   generate num
| Some sock ->
   let () = print_endline "Waiting for connection..." in
   let sock', _ = Unix.accept sock in
   let () = server sock' in
   Unix.shutdown sock' Unix.SHUTDOWN_ALL;
   Unix.shutdown sock Unix.SHUTDOWN_ALL
;;
