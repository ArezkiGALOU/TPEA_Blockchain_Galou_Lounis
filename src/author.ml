open Messages
open Letter
open Crypto

let make_letter_on_hash sk pk level head_hash letter : letter =
  Letter.make ~letter ~head:head_hash ~level ~pk ~sk

let make_letter_on_block sk pk level block letter : letter =
  let head_hash = hash block in
  make_letter_on_hash sk pk level head_hash letter

let random_char letter_bag = List.nth letter_bag (Random.int (List.length letter_bag)) 

let send_new_letter sk pk level store letter_bag =
  (* Get blockchain head *)
  Option.iter
    (fun head ->
      (* Create new random letter *)
      let letter =
        make_letter_on_block
          sk
          pk
          level
          (Word.to_bigstring head)
          (random_char letter_bag)
      in
      (* Send letter *)
      let message = Messages.Inject_letter letter in
      Client_utils.send_some message)
    (Consensus.head ~level:(level) store)

let run ?(max_iter = 0) () =
  (* Generate public/secret keys *)
  let (pk, sk) = Crypto.genkeys () in

  (* Register to the server *)
  let reg_msg = Messages.Register pk in
  Client_utils.send_some reg_msg ;

  let letter_bag = match Client_utils.receive () with
  | Messages.Letters_bag b -> b
  | _ -> assert false  
  in 

  (* Get initial wordpool *)
  let getpool = Messages.Get_full_wordpool in
  Client_utils.send_some getpool ;
  let wordpool =
    match Client_utils.receive () with
    | Messages.Full_wordpool wordpool -> wordpool
    | _ -> assert false
  in

  (* Generate initial blocktree *)
  let store = Store.init_words () in
  Store.add_words store wordpool.words ;

  (* Create and send first letter *)
  send_new_letter sk pk wordpool.current_period store letter_bag ;

  (* start listening to server messages *)
  Client_utils.send_some Messages.Listen ;

  (* start main loop *)
  let level = ref wordpool.current_period in
  let first_received = ref true in
  let rec loop max_iter =
    if max_iter = 0 then ()
    else (
      ( match Client_utils.receive () with
      | Messages.Inject_word w ->
          Option.iter
            (fun head ->
              if Word.hash head = w.head && !first_received then (
                first_received := false;
                Store.add_word store w ;  
                Log.log_info "Head updated to incoming word %a@." Word.pp w ;
                send_new_letter sk pk !level store letter_bag)
              else Log.log_info "incoming word %a not a new head@." Word.pp w)
            (Consensus.head ~level:(!level - 1) store)
      | Messages.Next_turn p -> level := p ; first_received := true
      | Messages.Inject_letter _ | _ -> () ) ;
      loop (max_iter - 1) )
  in
  loop max_iter

let _ =
  let main =
    Random.self_init () ;
    let () = Client_utils.connect () in
    run ~max_iter:(-1) ()
  in
  main
