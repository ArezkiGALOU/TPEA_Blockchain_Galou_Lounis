(* open Messages *)
open Word
open Crypto
exception Impossible_to_form_word
exception No_word_in_any_dict
exception No_word_to_suggest

(* all paths *)

let dictionnaries = "dict/dict_100000_1_10.txt"::"dict/dict_100000_25_75.txt"::"dict/dict_100000_50_200"::"dict/dict_100000_5_15.txt"::[]

let dict1_path = "dict/dict_100000_1_10.txt"
(* let dict1_path = "dict/dict1.txt" *)
let dict2_path = "dict/dict_100000_25_75.txt"
let dict3_path = "dict/dict_100000_50_200"
let dict4_path = "dict/dict_100000_5_15.txt"

let dict1 = ref []
let dict2 = ref []
let dict3 = ref []
let dict4 = ref []


type politician = { sk : Crypto.sk; pk : Crypto.pk } [@@deriving yojson, show]

type state = {
  politician : politician;
  word_store : Store.word_store;
  letter_store : Store.letter_store;
  next_words : word list;
}

let make_word_on_hash level letters politician head_hash : word =
  let head = head_hash in
  Word.make ~word:letters ~level ~pk:politician.pk ~sk:politician.sk ~head

let make_word_on_blockletters level letters politician head : word =
  let head_hash = hash (Word.to_bigstring head) in
  make_word_on_hash level letters politician head_hash

(* got from internet !!! very smart --> falling in love with Ocaml when i read such peice of art ! *)
let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let get_hash_of_last_word level word_store = 
  let last_word = Consensus.head ~level:(level - 1) word_store in 
    match last_word with 
    | None -> failwith ("got None for hash of last word for level "^(string_of_int (level - 1))^" !!! why ? .")
    | Some w -> Log.log_info "the last word is %a@" Crypto.pp_hash (Word.hash w); Word.hash w

let hashtbl_letters_to_list tbl = 
  let letters_list = ref [] in 
    Hashtbl.iter (fun _ v -> letters_list := !letters_list@[v]) tbl; 
    !letters_list

let tbl_contain_letter tbl letter = 
  let the_letter = ref None in 
    Hashtbl.iter (fun _ v -> if v.Letter.letter = letter then the_letter := Some v) tbl;
    ! the_letter


(* return list of letter that form the word_to_form *)
let forme_word htbl word_to_forme = 
  let rec forme_word_rec tbl word idx letters = 
    let letter = tbl_contain_letter tbl (String.get word idx) in
      match letter with 
      | None -> []
      | Some l -> if idx = (String.length word) - 1 then letters@[l] else forme_word_rec tbl word (idx + 1) letters@[l] 
      in
      forme_word_rec htbl word_to_forme 0 []

let author_not_used_yet the_letter authors_used = 
  let rec author_not_used_yet_rec letter authors = 
    match letter with 
    | [] -> None
    | hd::tl -> if List.exists (fun elt -> hd.Letter.author = elt) authors then author_not_used_yet_rec tl authors else Some hd 
  in author_not_used_yet_rec the_letter authors_used

let form_word letter_list word_to_forme = 
  let rec form_word_rec letter_l word idx letters authors = 
    if idx = String.length word then
      letters
    else
      try 
        let letters_to_use = List.find_all (fun l -> l.Letter.letter = (String.get word idx)) letter_l in
          let letter = author_not_used_yet letters_to_use authors in
            match letter with 
            | None -> raise Impossible_to_form_word
            | Some l -> form_word_rec letter_l word (idx+1) (letters@[l]) (authors@[l.Letter.author])
      with Not_found | Impossible_to_form_word -> raise Impossible_to_form_word
  in form_word_rec letter_list word_to_forme 0 [] []




let list_letters_to_words lst level politician head =
  let rec list_letters_words_rec liste word_lst = 
    match liste with 
    | [] -> word_lst
    | hd::tl -> (
      let word = make_word_on_blockletters level hd politician head in 
        list_letters_words_rec tl (word_lst@[word])
    )in 
  list_letters_words_rec lst []

let print_list lst = 
  let rec print_list_rec lst_ = 
    match lst_ with 
    | [] -> Log.log_info "end of list\n"
    | hd::tl -> Log.log_info "value : %s\n" hd ; print_list_rec tl
  in print_list_rec lst

let compute_best_word words_letter_list level politician head st = 
  let words_list = list_letters_to_words words_letter_list level politician head in
    let rec compute_best_word_rec words_l best_word = 
      match words_l with 
      | [] -> best_word
      | hd::tl -> if (Consensus.fitness st hd) > (Consensus.fitness st best_word) then compute_best_word_rec tl hd else compute_best_word_rec tl best_word
    in try compute_best_word_rec words_list (List.hd words_list) with _ -> raise No_word_to_suggest

let print_list_letter liste = 
  if liste = [] then Log.log_info "the list is empty\n" else
  List.iter (fun value -> Log.log_info "can use letter : %a@" Letter.pp value) liste


(* shuffle the dictionnaries just to introduce some randomness ! *)
let generate_new_word level politician (head : word) letter_store word_store = 
  Log.log_info "the last word is %a@" Crypto.pp_hash (Word.hash head);
  Log.log_info "the last word is %a@" Word.pp (head);

  let letters_to_use = Store.get_letters letter_store (Word.hash head) in 
    print_list_letter letters_to_use;
    let shuffeled_dict = shuffle dictionnaries in 
      let word_to_form = ref [] in   
        let rec read_from_dict dictionnary = 
          match dictionnary with 
          | [] -> () (*Log.log_info "no word nowhere\n"; raise No_word_in_any_dict*)
          | hd::tl -> (
              Log.log_info "trying for %s \n" hd;
              let input = open_in hd in 
                let stream_of_file = 
                  Stream.from ( fun _ -> try Some (input_line input) with End_of_file -> raise End_of_file ) in  (* somthing else than none*)
                      try ( 
                        Stream.iter (fun line -> try word_to_form := !word_to_form@[form_word letters_to_use line] with Impossible_to_form_word -> ()) stream_of_file ; 
                        close_in input
                      ) with e -> Log.log_info "the exception is : %s" (Printexc.to_string e); close_in input; read_from_dict tl
          ) in 
        read_from_dict shuffeled_dict; 
      try compute_best_word !word_to_form level politician head word_store with _ -> failwith "not competetiv"



let send_new_word st level politician letter_store word_store  =
  Option.iter 
    (fun head -> 
      let word = generate_new_word
        level 
        politician
        head
        letter_store
        word_store
      in 
      let message = Messages.Inject_word word in 
      Client_utils.send_some message)
      (Consensus.head ~level:(level - 1) st)


let load_dictionnary path dictionnary = 
  let input = open_in path in 
  try
    while true do
      let line = input_line input in
        dictionnary := !dictionnary@[line]
    done
  with End_of_file -> close_in input

let laod_all_dictionnaries () =
  Log.log_info "\nloading dict 1 may take a long time - be patiente\n";
  load_dictionnary dict1_path dict1;
  Log.log_info "\nloading dict 2\n";
  load_dictionnary dict2_path dict2;
  Log.log_info "\nloading dict 3\n";
  load_dictionnary dict3_path dict3;
  Log.log_info "\nloading dict 4\n";
  load_dictionnary dict4_path dict4

  (* to del printer of stores *)
let print_word_store_boucle st = 
  Store.iter_words (fun _ value -> Log.log_info "%a@." Word.pp value) st

let print_word_store st level = 
  Log.log_info "in the store at level %d" level;
  print_word_store_boucle st

let print_letter_store_boucle st = 
  Store.iter_letters (fun _ value -> Log.log_info "%a@." Letter.pp value) st

let print_letter_store st level = 
  Log.log_info "in the store of letters at level %d" level;
  print_letter_store_boucle st

  (* to del printer of stores [end] *)

let get_letters_to_use_for_level level = 
  let get_letter_pool = Messages.Get_letterpool_since (level - 1) in 
    Client_utils.send_some get_letter_pool; 
    match Client_utils.receive () with 
    | Messages.Diff_letterpool { since = _ ; letterpool = letterpool} -> letterpool
    | _ -> assert false

let get_word_choosed_by_authors words hash= 
  match hash with 
  | None -> failwith "le hash est none bizare"
  | Some h ->
  (let rec loop wordpool = 
    match wordpool with 
    | [] -> failwith "pas trouvé de mot bizare"
    | hd::tl -> match hd with | (_ , word) ->(if Word.hash word = h then word else loop tl) in 
    loop words
  )
let run ?(max_iter = 0) () =

  (* laod_all_dictionnaries(); *)

  (* Generate public/secret keys *)
  let (pk, sk) = Crypto.genkeys () in
  let pol = {sk = sk ; pk = pk} in 
  
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


  (* Get initial letterpool *)
  let getletterpool = Messages.Get_full_letterpool in
  Client_utils.send_some getletterpool ;

  let letterpool =
    match Client_utils.receive () with
    | Messages.Full_letterpool letterpool -> letterpool
    | _ -> assert false
  in
  (* Generate initial letterpool *)
  let letterstore = Store.init_letters () in
  Store.add_letters letterstore letterpool.letters ;
  Log.log_info "sending";

  (* Create and send first word *)
  send_new_word store wordpool.current_period pol letterstore store;

  (*start listening to server messages *)
  Client_utils.send_some Messages.Listen ;
  
  (*  main loop *) 
  let level = ref wordpool.current_period in 
  let last_hash = ref None in
  let rec loop max_iter = 
    if max_iter = 0 then () 
    else (
      (match Client_utils.receive () with 
      | Messages.Next_turn p -> (level := p ; 
                            let diff_letter_pool = Messages.Get_letterpool_since (p-1) in 
                            Client_utils.send_some diff_letter_pool)
      | Messages.Inject_letter l -> Store.add_letter letterstore l
      | Messages.Diff_wordpool {since=  _ ; wordpool=wordpool } -> 
          let the_word = get_word_choosed_by_authors wordpool.words !last_hash in 
            Store.add_word store  the_word ; 
            send_new_word store !level pol letterstore store
        
      | Messages.Diff_letterpool {since = _ ; letterpool = letterpool} -> 
                                  (try let l = List.hd letterpool.letters in last_hash := Some l.head with _ -> failwith "aucun lettre injecté ???");
                                  let diff_word_pool = Messages.Get_wordpool_since (!level-1) in 
                                    Client_utils.send_some diff_word_pool
      | _ -> () 
      ) ; 
      loop (max_iter - 1)
    )
    in 
    loop max_iter

let _ =
  let main =
    Random.self_init () ;
    let () = Client_utils.connect () in
    run ~max_iter:(-1) ()
  in
  main
