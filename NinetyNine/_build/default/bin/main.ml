(* Q1: Tail of List *)
let strs = ["a" ; "b" ; "c" ; "d"] (* Answer: d *);;
let blnk = [] (* Answer: None *);;
let sngl= ["a"] (* Answer: None *);;

let rec last = function
    | []        -> None
    | [x]       -> Some x
    | _ :: t    -> last t ;;

let print_res x = 
    match x with 
    | None      -> print_string "None\n"
    | Some v    -> print_string (v ^ "\n") ;;

Printf.printf "This is Question 1\n";;
print_res (last strs);;
print_res (last blnk);;


(* Q2: Last two elements*)
let print_res_tuple x = 
    match x  with 
    | None          -> print_string "None\n"
    | Some (x,y)    -> Printf.printf "(%s, %s)\n" x y ;;


let rec tuple_last = function
    | [] | [_]  -> None
    | [x;y]     -> Some (x,y)
    | _ :: t    -> tuple_last t;;

Printf.printf "This is Question 2\n";;
print_res_tuple (tuple_last strs);;
print_res_tuple (tuple_last sngl);;



(* Q3 Find Nth Element *)
(* List.nth["a";"b";"c";"d";"e"] 2;; *)
(* List.nth ["a"] 2;; *)


let rec search_lst lst idx = 
    match lst with 
    | [] -> None
    | h::t -> if idx = 0 then Some h else search_lst t (idx -1) ;;

Printf.printf "This is Question 3\n";;
print_res(search_lst ["a";"b";"c";"d";"e"] 2);;
print_res(search_lst ["a"] 2);;

(* Q4: Length of List *)
let length = ["a";"b";"c";"d";"e"] ;; (* 5 *)
let lng = [];; (* 0 *)

let rec cnt lst acc = 
    match lst with
    | [] -> acc
    | _::t -> cnt t (acc + 1);;

let top lst =
    let rec aux n lst = 
        match lst with 
        | [] -> n
        | _::t -> aux (n+1) t
    in 
    aux 0 lst;;
Printf.printf "This is Question 4\n";;
Printf.printf "%d\n" (cnt length 0);;
Printf.printf "%d\n" (cnt lng 0);;

Printf.printf "%d\n" (top length);;
Printf.printf "%d\n" (top lng);;
(* Q5: Reverse a List *)
let rev = ["a";"b";"c"];; (*res = ["c";"b";"a"]*)

let rev_list lst = 
    let rec rev lst acc = 
        match lst with
        | [] -> acc
        | h::t -> rev t (h::acc)
    in 
    rev lst [];;

Printf.printf "This is Question 5\n";;
List.iter (Printf.printf "%s ") (rev_list rev);;


(* Q6: Palindrom *)
let is_palindrome = ["x"; "a"; "m"; "a"; "x"];; (* True *)
let is_not_palindrome = ["a"; "b"];; (* False *)

let test_palindrome lst =
    lst = rev_list lst;;


Printf.printf "\nThis is Question 6\n";;
List.iter (Printf.printf "%s ") (rev_list is_palindrome);;
Printf.printf "%B\n" (test_palindrome is_palindrome);;
List.iter (Printf.printf "%s ") (rev_list is_not_palindrome);;
Printf.printf "%B\n" (test_palindrome is_not_palindrome);;

(* Q7: Unpack a type*)
type 'a node = 
    | One of 'a
    | Many of 'a node list 

let flatten = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;

let to_list nd_lst = 
    let rec strip nd_lst acc =
        match nd_lst with
        | [] -> acc
        | One x::t -> strip t (x::acc)
        | Many xs::t -> strip t (strip xs acc) 
    in 
    rev_list(strip nd_lst []);;


Printf.printf "\nThis is Question 7\n";;
List.iter (Printf.printf "%s ") (to_list flatten);;
Printf.printf"\n"

(* Q8: Eliminate Duplicates *)
let compress = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

let set_mkr lst =
    let rec reduce_dup lst acc =
        match lst with
        | []        -> acc 
        | [x]       -> reduce_dup [] (x::acc)
        | h::m::t   -> if h = m then reduce_dup (h::t) acc else reduce_dup (m::t) (h::acc)
    in 
    rev_list(reduce_dup lst []);;

let rec compressor = function
    | a :: (b :: _ as t) -> if a = b then compressor t else a :: compressor t
    | smaller -> smaller;;


Printf.printf "\nThis is Question 8\n";;
List.iter (Printf.printf "%s ") (set_mkr compress);;
List.iter (Printf.printf "%s ") (compressor compress);;
Printf.printf"\n";;

(* Q9: Pack Consecutive Duplicates *)
let pack = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;

let packer lst = 
    let rec pck_it lst collector acc = 
        match lst with
    | []                    -> [] 
    | [x]                   -> (x::collector)::acc
    | h::(m::_ as t)        -> if h = m then pck_it t (h::collector) acc
                               else pck_it t [] ((h::collector)::acc) in 
    rev_list(pck_it lst [] []);;

let troof = (packer pack) = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]];;
Printf.printf "\nThis is Question 9\n";;
Printf.printf "%B\n" troof;;
Printf.printf"\n";;



(* Q10: Run-Lentgh Encoding: No Types *)
let encode_10 = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

let result_10 = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")];;

let rle_normal lst =
    let rec cycle cnt acc lst =
        match lst with
        | []                -> acc 
        | [x]               -> (cnt+1 , x)::acc 
        | h::(m::_ as t)    -> if h = m then cycle (cnt + 1) acc t 
                               else cycle 0 ((cnt+1 , h)::acc) t
    in 
    rev_list(cycle 0 [] lst);;

let rle_print tpl = 
        match tpl with
        | (cnt, x) -> Printf.printf "(%d, %s); "cnt x;;

let troof11 = (rle_normal encode_10) = result_10;;
List.iter rle_print (rle_normal encode_10);;
Printf.printf "\nThis is Question 10\n";;
Printf.printf "\n%B\n" troof11;;
(* Q11: Run-Lentgh Encoding:Types*)
type 'a rle = 
    | One of 'a
    | Many of int * 'a

let encode = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

let result = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")];;

let run_lngth_encode lst =
    let rle cnt x = if cnt = 0 then One x else Many (cnt + 1, x) in
    let rec cnt_ele lst cnt acc =
        match lst with 
        | []                -> acc
        | [x]               -> rle cnt x :: acc 
        | h::(m::_ as t)    -> if h = m  then cnt_ele t (cnt + 1) acc
                                else cnt_ele t 0 (rle cnt h :: acc) 
    in 
    rev_list(cnt_ele lst 0 []);;

let rle_printer = function
    | One x -> Printf.printf "One %s; " x
    | Many (n,x) -> Printf.printf "Many (%d %s); " n x;;


let troofTwo = (run_lngth_encode encode) = result;;
Printf.printf"\n";;
Printf.printf "\nThis is Question 11\n";;
List.iter rle_printer (run_lngth_encode encode);;
Printf.printf"\n";;
List.iter rle_printer (result);;
Printf.printf "\n%B\n" troofTwo;;
Printf.printf"\n";;


(* Q12: Decode a Run-Length Encoded List *)

let decode_12 = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
let result_12 = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;



let decode lst = 
    let rec help n x acc = 
        if n = 0 then acc else help (n-1) x (x::acc) 
    in
    let rec aux lst acc =
        match lst with 
        | [] -> acc
        | One x::t -> aux t (x::acc)
        | Many(n,x)::t -> aux t (help n x acc)
    in 
    aux (rev_list lst) [];;
Printf.printf "\nThis is Question 12\n";;
Printf.printf "%B\n" ((decode decode_12) = result_12);;
List.iter (Printf.printf "%s") (decode decode_12);;

(* Q13: Run-Length Encoding of a List (Direct Solution) *)
(* Use encode_10 *)
let  encode_13 = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
let res_13 = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")];;

let rle_ds lst = 
    let helper cnt x = if cnt = 0 then One x else Many (cnt+1, x) in
    let rec aux cnt acc lst = 
        match lst with
        | []                -> acc 
        | [x]               -> ((helper cnt x)::acc)
        | h::(m::_ as t)    -> if h = m then aux (cnt + 1) acc t 
                               else aux 0 ((helper cnt h)::acc) t 
    in 
    aux 0 [] (rev_list lst);;


let troofTwo = (rle_ds encode_13) = res_13;;
Printf.printf"\n";;
Printf.printf "\nThis is Question 13\n";;
Printf.printf "\n%B\n" troofTwo;;
List.iter rle_printer (rle_ds encode_13);;



(* Q14: Duplicate the Elements of a List *)
let duplicate = ["a"; "b"; "c"; "c"; "d"];;
let res_14 = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"];;

let dup_it lst = 
    let rec aux acc = function 
        | [] -> acc 
        | h::t -> aux (h::h::acc) t
    in 
    aux [] (rev_list lst);;


let troof_14 = dup_it duplicate = res_14;;
Printf.printf"\n";;
Printf.printf"\n";;
Printf.printf"\n";;
Printf.printf "This is Question 14\n";;
Printf.printf "\n%B\n" troof_14;;
List.iter (Printf.printf "%s") (dup_it duplicate);;


(* Q15: *Replicate the Elements of a List a Given Number of Times *)
let replicate = ["a"; "b"; "c"];;
let cnt = 3;;
let res_15 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"];;

let rep_it lst cnt = 
    let rec helper x cnt acc = if cnt = 0 then  acc else helper x (cnt-1) (x::acc) in
    let rec aux acc = function 
        | [] -> acc 
        | h::t -> aux (helper h cnt acc) t
    in 
    aux [] (rev_list lst);;

let troof_15 = (rep_it replicate cnt) = res_15;;
Printf.printf "This is Question 15\n";;
Printf.printf "\n%B\n" troof_15;;
List.iter (Printf.printf "%s") (rep_it replicate cnt);;


(* Q16: Drop Every N'th Element From a List*)
let drop_16 = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"];;
let list_16 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"];;


let dropper lst cnt = 
    let rec aux n acc lst = 
        match lst with
        | []        -> acc
        | h::t      -> if cnt = n then aux 1 acc t else aux (n+1) (h::acc) t 
    in 
    rev_list(aux 1 [] lst);;


let troof_16 = (dropper drop_16 3) = list_16;;
Printf.printf "This is Question 16\n";;
Printf.printf "\n%B\n" troof_16;;
List.iter (Printf.printf "%s") (dropper drop_16 3);;


(* Q17 Split a List Into Two Parts; The Length of the First Part Is Given *)
let split_17 = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"];;
let nm_17 = 3;;
let res_17 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]);;

let split_2_17 = ["a"; "b"; "c"; "d"];;
let nm_2_17 = 5;;
let res_2_17 = (["a"; "b"; "c"; "d"], []);;

let tup_split lst n =
    let rec aux acc1 acc2 i = function
        | []        -> (rev_list(acc1), rev_list(acc2))
        | h::t      -> if n = i then aux acc1 (h::acc2) i t 
                       else aux (h::acc1) acc2 (i+1) t
        in 
        aux [] [] 0 lst;;

let troof_1_17 = (tup_split split_17 nm_17) = res_17;;
let troof_2_17 = (tup_split split_2_17 nm_2_17) = res_2_17;;

Printf.printf "\n";;
Printf.printf "This is Question 17\n";;
Printf.printf "\n%B" troof_1_17;;
Printf.printf "\n%B" troof_2_17;;

(* Q18: Extract a Slice From a List *)

let slice_18 = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"];;
let frst_18 = 2;;
let scnd_18 = 6;;
let res_18 = ["c"; "d"; "e"; "f"; "g"];;

let slice lst s f = 
    let rec aux cnt acc = function
        | []        ->  rev_list(acc)
        | h::t      -> if cnt >= s && cnt <= f then aux (cnt + 1) (h::acc) t
                       else aux (cnt + 1) acc t
    in 
    aux 0 [] lst;;


let troof_1_18 = (slice slice_18 frst_18 scnd_18) = res_18;;

Printf.printf "\n";;
Printf.printf "This is Question 18\n";;
Printf.printf "\n%B" troof_1_18;;
Printf.printf "\n";;
List.iter (Printf.printf "%s") (slice slice_18 frst_18 scnd_18);;


(* Q19: Rotate a List N Places to the Left *)

let rotate_19 = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"];;
let rotate_n_19 = 3;;
let res_19 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"];;


let rotate_n lst n = 
    let rec aux c acc n lst = 
        match lst with
        | []    -> acc
        | h::t  -> if c = n then h::t @ rev_list(acc)
                   else aux (c+1) (h::acc) n t
    in 
    aux 0 [] n lst;;


let troof_1_19 = (rotate_n rotate_19 rotate_n_19) = res_19;;

Printf.printf "\n";;
Printf.printf "This is Question 19\n";;
Printf.printf "\n%B" troof_1_19;;
Printf.printf "\n";;
List.iter (Printf.printf "%s") (rotate_n rotate_19 rotate_n_19);;

        
(* Q20: Remove the K'th Element From a List*)
let remove_20 = ["a"; "b"; "c"; "d"];;
let remove_pos_20 = 2;;

let remove_it lst x = 
    let rec aux cnt acc = function
        | []        -> rev_list(acc)
        | h::t      -> if cnt = x then rev_list(acc) @ t 
                       else aux (cnt + 1) (h::acc) t
    in 
    aux 0 [] lst;;

let troof_1_20 = (remove_it remove_20 remove_pos_20) = ["a";"b";"d"];;
Printf.printf "\n";;
Printf.printf "This is Question 20\n";;
Printf.printf "\n%B" troof_1_20;;
Printf.printf "\n";;
List.iter (Printf.printf "%s") (remove_it remove_20 remove_pos_20);;


(* Q21: Insert an Element at a Given Position Into a List*)

(* let insert_21 = "alfa";; *)
(* let insert_pos_21 = 1;; *)
(* let insert_list = ["a"; "b"; "c"; "d"];; *)
(* let res_21 = ["a"; "alfa"; "b"; "c"; "d"];; *)

(* let insert_at ele pos lst =  *)
(*     let rec aux cnt acc = function *)
(*         | [] -> acc  *)
(*         | h::t -> if cnt = pos then acc *)
