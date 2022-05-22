(* Echauffement *)

(* Question 1 *)

(* Definition of the type string_builder : a node contains the string, his length, the left side and the right side of the following *)
type string_builder =
    | Leaf of int * string
    | Node of int * string_builder * string_builder ;;

(* string_builder tests *)
let sb_1 = Leaf (10, "OCaml test") ;; (* val sb_1 : string_builder = Leaf (10, "OCaml test") *)
let sb_2 = Node (9, Leaf (5, "OCaml"), Leaf (4, "test")) ;; (* val sb_2 : string_builder = Node (9, Leaf (5, "OCaml"), Leaf (4, "test")) *)
let sb_3 = Node(32, Node (9, Leaf(5, "OCaml"), Leaf (4, "is a")), Node (23, Leaf (16, "nice programming"), Leaf (7, "langage"))) ;; 
(* val sb_3 : string_builder =
    Node (32, Node (9, Leaf (5, "OCaml"), Leaf (4, "is a")),
      Node (23, Leaf (16, "nice programming"), Leaf (7, "langage"))) *)

(*  word : string -> string_builder
    @param a string str
    @return a string_builder containing only one leaf *)
let word str = Leaf (String.length str, str) ;;

(* word tests *)
let word_1 = word "OCaml" ;; (* Leaf(5, "OCaml") *)
let word_2 = word "is" ;; (* Leaf(2, "is") *)
let word_3 = word "a" ;; (* Leaf(1, "a") *)
let word_4 = word "nice" ;; (* Leaf(4, "nice") *)
let word_5 = word "programming" ;; (* Leaf(11, "programming") *)
let word_6 = word "langage" ;; (* Leaf(7, "langage") *)

(*  length : string_builder -> int
    @param a string_builder sb
    @return returns the length of sb *)
let length sb =
    match sb with
    | Leaf (len, _) -> len
    | Node (len, _, _) -> len ;;

(* length tests *)
let len_1 = length sb_1 ;; (* int = 10 *)
let len_2 = length sb_2 ;; (* int = 9 *)
let len_3 = length sb_3 ;; (* int = 9 *)

(*  concat : string_builder -> string_builder -> string_builder
    @param two string builders sb1 and sb2
    @return the concatenation of sb1 and sb2 *)
let concat sb1 sb2 = Node ((length sb1) + (length sb2), sb1, sb2) ;;

(* concat tests *)
let sentence = concat (concat word_1 word_2) (concat (concat word_3 word_4) (concat word_5 word_6)) ;;
(*  Node (30, Node (7, Leaf (5, "OCaml"), Leaf (2, "is")),
        Node (23, Node (5, Leaf (1, "a"), Leaf (4, "nice")),
            Node (18, Leaf (11, "programming"), Leaf (7, "langage")))) *)


(* Question 2 *)

(*  char_at : int -> string_builder -> string
    @param an interger i and a string_buider sb (i is lower than the length of the word represented by sb)
    @return the character at the position i of the word represented by sb *)
let rec char_at sb i =
    match sb with
    | Leaf (_, str) -> String.get str i
    | Node (len, sb_left, sb_right) -> 
        let len_left = (length sb_left) in
            if i < len_left then char_at sb_left i
            else char_at sb_right (i - len_left) ;;

(* char_at tests on a leaf *)
let char_at_word_0 = char_at word_1 0 ;; (* char = 'O' *)
let char_at_word_4 = char_at word_1 4 ;; (* char = 'l' *)

(* char_at tests on a node *)
let char_at_sentence_0 = char_at sentence 0 ;; (* char = 'O' *)
let char_at_sentence_1 = char_at sentence 1 ;; (* char = 'C' *)
let char_at_sentence_10 = char_at sentence 10 ;; (* char = 'c' *)
let char_at_sentence_20 = char_at sentence 20 ;; (* char = 'i' *)


(* Question 3 *)

(*  sub_string : int -> int -> string_builder -> string_builder
    @param two intergers i and m and a string_builder sb (i + m is lower than the length of the word represented by sb)
    @return the sub-string from the position i of length m *)
let rec sub_string sb i m =
    match sb with
    | Leaf (_, str) -> Leaf (m, (String.sub str i m))
    | Node (_, sb_left, sb_right) ->
        let len_left = (length sb_left) in
            if i < len_left then
                if (i + m) <= len_left 
                    then sub_string sb_left i m
                    else concat (sub_string sb_left i (len_left - i)) (sub_string sb_right 0 (m - len_left + i))
                else sub_string sb_right (i - len_left) m ;;

(* sub_string tests on a leaf *)
let sub_string_word = sub_string word_5 4 3 ;; (* Leaf (3, "ram") *)
(* sub_string tests on a node *)
let sub_string_sentence = sub_string sentence 10 7 ;; (* Node (7, Leaf (2, "ce"), Leaf (5, "progr")) *)


(* Equilibrage *)

(* Question 4 *)

(*  cost : string_builder -> int
    @param a string_builder sb
    @return the cost of the string_builder sb *)
let cost sb =
    let rec cost_aux sb depth =
    match sb with
    | Leaf (len, _) -> len * depth
    | Node (_, sb_left, sb_right) -> (cost_aux sb_left (depth + 1)) + (cost_aux sb_right (depth + 1))
    in cost_aux sb 0 ;;

(* cost tests on a leaf *)
let cost_word = cost word_5 ;; (* int = 0 *)
(* cost tests on a node *)
let cost_sentence = cost sentence ;; (* int = 83 *)


(* Question 5 *)

(*  random_word : unit -> string
    @param nothing
    @return a random word of random length between 0 and 10 using *)
let random_word () =
    let rec random_word_aux n = 
        if n == 0 then ""
        else Char.escaped ((Char.chr (65 + Random.int 26))) ^ (random_word_aux (n - 1))
    in random_word_aux (Random.int 11) ;;

(*  random_leaf : unit -> string_builder
    @param nothing
    @return a leaf with a random word of random length *)
let random_leaf () =
    let word = random_word () in Leaf (String.length word, word) ;;

(*  random_string : int -> string_builder
    @param an integer i
    @return a random tree of a string_builder based on a random string *)
let random_string i =
    let rec random_string_aux i depth =
        if depth < i then
            let choice = Random.int 3 in
                if choice = 0 then
                    concat (random_string_aux i (depth + 1)) (random_string_aux i (depth + 1))
                else if choice = 1 then
                    concat (random_string_aux i (depth + 1)) (random_leaf ())
                else
                    concat (random_leaf ()) (random_string_aux i (depth + 1))
        else random_leaf ()
    in random_string_aux i 0 ;;
    
(* random_string tests *)
let random_string_1 = random_string 1 ;; (* random string builder of depth 1 *)
let random_string_5 = random_string 3 ;; (* random string builder of depth 3 *)
let random_string_10 = random_string 5 ;; (* random string builder of depth 5 *)

(* Question 6 *)

(*  list_of_string : string_builder -> string list
    @param a string_builder sb
    @return the list of all the string in sb in the same order as the tree *)
let rec list_of_string sb =
    match sb with
    | Leaf (_, str) -> [str]
    | Node (_, sb_left, sb_right) -> (list_of_string sb_left) @ (list_of_string sb_right) ;;

(* list_of_string tests on a leaf *)
let list_word = list_of_string word_5 ;; (* ["programming"] *)
(* list_of_string tests on a node *)
let list_sentence = list_of_string sentence ;; (* ["OCaml"; "is"; "a"; "nice"; "programming"; "langage"] *)

(* Question 7 *)

(*  init_min : string_builder list -> int 
    @param a string_builder list sb_leaf
    @return the cost of the concatenation of the two first elements of sb_leaf *)
let init_min sb_leaf = cost (concat (List.nth sb_leaf 0) (List.nth sb_leaf 1)) ;;

(*  least_index_cost : string_builder list -> int
    @param a string_builder list sb_leaf and two intergers a minimum min and a position i
    @return the position of the first of two successive elements with least cost of concatenation *)
let least_index_cost sb_leaf =
    let rec least_index_cost_aux list min i j = 
    match list with
        | [] -> failwith "the list of leaves is empty"
        | [leaf] -> j
        | leaf1 :: leaf2 :: list -> 
            let least_cost = cost (concat leaf1 leaf2) in
                if least_cost < min
                    then least_index_cost_aux (leaf2 :: list) least_cost (i + 1) i
                    else least_index_cost_aux (leaf2 :: list) min (i + 1) j
    in least_index_cost_aux sb_leaf (init_min sb_leaf) 0 0 ;;

(* least_index_cost test *)
let l1 = "AAA"::"BB"::"C"::"D"::"EEEEE"::[] ;;
let l1' = List.map word l1 ;;
let index_l1 = least_index_cost l1' ;; (* int = 2 *)

let l2 = "AA"::"BBB"::"CCCC"::"DDDD"::"E"::"F"::"GG"::"HHH"::[] ;;
let l2' = List.map word l2 ;;
let index_l2 = least_index_cost l2' ;; (* int = 4 *)

(*  concat_least_cost : string_builder list -> int -> string_builder list
    @param a string_builder list sb_leaf and an integer i
    @return a string_builder list which contains the concatenation of the two successive elements with the first at position i *)
let rec concat_least_cost sb_leaf i =
    match sb_leaf, i with
    | [], _ -> failwith "the list of leaves is empty"
    | [x], _ -> failwith "the length of list is 1"
    | leaf1 :: leaf2 :: list, 0 -> (concat leaf1 leaf2) :: list
    | leaf1 :: leaf2 :: list, i -> leaf1 :: (concat_least_cost (leaf2 :: list) (i - 1)) ;;

(*  balance : string_builder -> string_builder
    @param a string_builder sb
    @return a balanced string_builder *)
let balance sb = 
    let sb_leaf = List.map word (list_of_string sb) in
    let rec aux_balance sb_leaf =
        match sb_leaf with
        | [] -> failwith "the list of leaves is empty"
        | [x] -> [x]
        | list -> aux_balance (concat_least_cost list (least_index_cost sb_leaf))
    in List.hd (aux_balance sb_leaf);;

(* balance tests *)
let balanced_word = balance word_5 ;; (* no changes *)
let balanced_sentence_1 = balance sentence ;; (* should concat "is" with "a" *)


(* Question 8 *)

(*  random_generator : unit -> string_builder list
    @param nothing
    @return a random list of size between 1 and 100 of string builders of depth between 0 and 5 *)
let random_generator () = 
    let rec random_generator_aux n =
        if (n == 0) then (random_string (Random.int 6)) :: []
        else (random_string (1 + Random.int 11)) :: (random_generator_aux (n - 1))
    in let n = (1 + Random.int 101) in (random_generator_aux n) ;;

(*  list_cost : string_builder -> int list
    @param a string_builder
    @return an int list of the cost of the string builders *)
let rec list_cost sb_list =
    match sb_list with
    | [] -> []
    | sb :: list -> (cost sb) :: (list_cost list) ;;  

(*  list_cost_balanced : string_builder -> int list
    @param a string_builder
    @return an int list of the cost of the balanced string builders *)
let rec list_cost_balanced sb_list =
    match sb_list with
    | [] -> []
    | sb :: list -> (cost (balance sb)) :: (list_cost_balanced list) ;;

(*  sum_list : int list -> int
    @param an int list
    @return the sum of the elements of the list *)
let sum_list list = (List.fold_left (fun a b -> a + b) 0 list) ;;

(*  max_list : int list -> int
    @param an int list
    @return the maximum of the list *)
let max_list list = List.fold_left (fun a b -> if a > b then a else b) (List.hd list) list ;;

(*  min_list : int list -> int
    @param an int list
    @return the minimum of the list *)
let min_list list = List.fold_left (fun a b -> if a < b then a else b) (List.hd list) list ;;

(*  avg_list : int list -> int
    @param an int list
    @return the average of the list *)
let avg_list list = (Float.of_int (sum_list list)) /. (Float.of_int (List.length list)) ;;

(*  med_list : int list -> int
    @param an int list
    @return the median of the list *)
let med_list list = let n = List.length list in
	if (n mod 2) == 1 
    then Float.of_int (List.nth list ((n-1)/2))
	else ((Float.of_int (List.nth list (n/2)-1)) +. (Float.of_int (List.nth list (n/2)))) /. 2.0 ;;

(* functions on int list tests *)
let int_list = 1::2::3::4::3::5::6::7::8::9::[] ;;
let sum = sum_list int_list ;; (* int = 48 *)
let min = min_list int_list ;; (* int = 1 *)
let max = max_list int_list ;; (* int = 9 *)
let avg = avg_list int_list ;; (* float = 5 *)
let med = med_list int_list ;; (* float = 4.5 *)

(*  pnl : int -> int -> int -> int -> int
    @param nothing
    @return returns min, max, average and median of the cost of balanced random string builders *)
let pnl () = 
    let random_sb_list = random_generator () in
        let list_cost = list_cost random_sb_list in
        let list_cost_balanced = list_cost_balanced random_sb_list in
            max_list (list_cost), min_list (list_cost), avg_list (list_cost), med_list (list_cost), (* not balanced string builder list *)
            max_list (list_cost_balanced), min_list (list_cost_balanced), avg_list (list_cost_balanced), med_list (list_cost_balanced), (* balanced string builder list *)
            (sum_list list_cost) - (sum_list list_cost_balanced) ;; (* gain beetween balanced and non balanced *)

(* pnl tests *)
let test_1 = pnl () ;;
let test_2 = pnl () ;;
let test_3 = pnl () ;;
let test_4 = pnl () ;;
let test_5 = pnl () ;;


