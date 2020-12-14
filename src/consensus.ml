open Word
open Constants

(* ignoring unused variables - to be removed *)
let _ = ignore genesis

let score_table = Hashtbl.create 26

(*start filling score table*)
let _ = 
Hashtbl.add score_table 'a' 1;
Hashtbl.add score_table 'e' 1;
Hashtbl.add score_table 'i' 1;
Hashtbl.add score_table 'l' 1;
Hashtbl.add score_table 'n' 1;
Hashtbl.add score_table 'r' 1;
Hashtbl.add score_table 's' 1;
Hashtbl.add score_table 't' 1;
Hashtbl.add score_table 'u' 1;
Hashtbl.add score_table 'o' 1;
Hashtbl.add score_table 'd' 2;
Hashtbl.add score_table 'g' 2;
Hashtbl.add score_table 'm' 2;
Hashtbl.add score_table 'b' 3;
Hashtbl.add score_table 'c' 3;
Hashtbl.add score_table 'p' 3;
Hashtbl.add score_table 'f' 4;
Hashtbl.add score_table 'h' 4;
Hashtbl.add score_table 'v' 4;
Hashtbl.add score_table 'j' 8;
Hashtbl.add score_table 'q' 8;
Hashtbl.add score_table 'k' 10;
Hashtbl.add score_table 'w' 10;
Hashtbl.add score_table 'x' 10;
Hashtbl.add score_table 'y' 10;
Hashtbl.add score_table 'z' 10

(*end filling score table*)

(* end ignoring unused variables - to be removed *)

let word_score { word; _ } : int =
  let rec score_of_word the_word score = 
    match the_word with 
    | [] -> 0
    | hd::tl -> score_of_word tl (score + (Hashtbl.find score_table hd.Letter.letter))
    in 
  score_of_word word 0
  

let fitness st word =
  ignore st ;
  word_score word

(* TODO *)

let head ?level (st : Store.word_store) =
  match level with 
  | None -> Some genesis_word
  | Some a when a <= 1 -> Some genesis_word
  | Some a when a > 1 -> ( 
    let the_word = ref None in 
      let select_the_word _ word_value = 
          if word_value.level = a then the_word := Some word_value
        in Store.iter_words select_the_word st;
        !the_word
  )
  | Some _ -> Some genesis_word  (* better put an exception : failwith "impossible"*)


