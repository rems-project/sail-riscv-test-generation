open Riscv
open Sail_lib

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

let socket =
  match Sys.argv with
  | [| _; host; port |] ->
     let open Unix in
     let addr::_ = getaddrinfo host port [AI_FAMILY PF_INET; AI_SOCKTYPE SOCK_STREAM] in
     let sock = socket ~cloexec:true PF_INET SOCK_STREAM 0 in
     let () = connect sock addr.ai_addr in
     Some sock
  | _ -> None
;;

(* Loop until we get an instruction supported by the encoder *)
let generate () =
  let instr, bits =
    let rec aux () =
      try
        let instr = rand_gens.gen_zast rand_gens in
        let bits =
          try zencdec_forwards instr
          with Match_failure _ -> zencdec_compressed_forwards instr
        in
        let () = print_endline (string_of_bits bits ^ " " ^ zassembly_forwards instr) in
        instr, bits
      with Match_failure _ -> aux ()
    in aux ()
  in
  match socket with
  | None -> ()
  | Some sock ->
     let bytes = Bytes.make 8 '\000' in
     let () = Bytes.set bytes 4 '\001' in
     let () = set_bytes bytes 0 bits in
     if Unix.send sock bytes 0 8 [] <> 8
     then prerr_endline "Unable to send full instruction"
     else ()
;;

Random.self_init ();;
generate ();;

match socket with
| None -> ()
| Some sock -> Unix.shutdown sock Unix.SHUTDOWN_ALL
;;
