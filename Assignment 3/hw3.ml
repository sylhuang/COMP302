(* Question 1 - Unfolding *)

(* This is the function unfold, take some time to compare it it fold.
   If fold_left and fold_right take lists to generate a value, this
   function takes a value that will generate a list. The relation
   between folds and unfolds is the beginning of a wonderful tale in
   computer science. But let's not get ahead of ourselves.

   Unfold takes a function that from a seed value it generates a new
   element of the list, and a the seed for the next element, another
   function to stop the generation, and an initial seed.
*)

let rec unfold (f: 'seed -> ('a * 'seed)) (stop : 'b -> bool) (b : 'seed) : 'a list =
  if stop b then []
  else let x, b' = f b in
       x :: (unfold f stop b')

let nats max = unfold (fun b -> b, b + 1) (fun x -> x > max) 0

(* Q1.1: Return the even numbers up-to max *)
let evens max = unfold (fun b -> b, b + 2) (fun x -> x > max) 0

(* Q1.2: Return the Fibonacci sequence up-to max *)
let fib max =
  let comp l = match l with
  |[] -> false
  |h::_ -> h > max
  in
  unfold  (fun [h;t] -> h, [t;h+t]) comp [1;1]

(* Q1.3: Return the list of rows of the Pascal triangle that are shorter than max *)
let pascal max =
  let row l1 =
    let l2 = [0]@l1@[0]
    in
    let rec add l = match l with
      |[] -> []
      |h::t -> match t with
        |[] -> []
        |s::t' -> (h+s)::add(s::t')
    in
    l1, (add l2)
  in
  unfold row (fun x -> List.length(x) > (max-1)) [1]

let rec zip (l1 : 'a list) (l2 : 'b list) :  ('a * 'b) list =
match l1, l2 with
| [], _ -> []
| _, [] -> []
| x::xs, y::ys -> (x, y):: zip xs ys

(* (Extra credit) Optional: implement zip with a single call to unfold *)
let zip' (l1: 'a list) (l2: 'b list) : ('a * 'b) list =
  let nl = List.hd l1
  in
  let extract l = match l with
    | [] -> (nl,nl), [[];[]]
    | _::[] -> (nl,nl), [[];[]]
    | xl::yl::t -> match xl,yl with
                | [], _ -> (nl,nl), [[];[]]
                | _, [] -> (nl,nl), [[];[]]
                | x::xs,y::ys -> (x,y), [xs;ys]
  in
  let comp l = match l with
    | [] -> true
    | _::[] -> true
    | xl::yl::t -> match xl,yl with
                | [], _ -> true
                | _, [] -> true
                | x::xs,y::ys -> false
  in
  unfold extract comp [l1;l2]

(* Question 2 *)

let ugly x =
  let rec ackermann m n = match (m , n) with
    | 0 , n -> n+1
    | m , 0 -> ackermann (m-1) 1
    | m , n -> ackermann (m-1) (ackermann m (n-1))
  in
  ackermann 3 x

let memo_zero (f : 'a -> 'b) : 'a -> 'b = f

(* Q2.1: Write a function that memoizes the last value called. *)
let memo_one (f : 'a -> 'b) : ('a -> 'b) =
  let inp = ref None
  in
  let res = ref None
  in
  let check =
    fun x ->
      let write = fun y -> (inp := Some y; res:= Some(f y); !res)
      in
      match !inp with
      | None -> write x
      | Some y -> if x = y then !res
                  else write x
  in
  fun z -> match check z with
  | None -> raise (Invalid_argument "Invalid input")
  | Some x' -> x'

(* Example usage:

let ugly' = memo_one ugly

let u1 = ugly' 3                (* this one calls ugly with 3 *)
let u2 = ugly' 3                (* this one uses the stored value *)
let u3 = ugly' 1                (* the stored value is for 3 so it calls ugly *)
let u4 = ugly' 2                (* the stored value is for 1 so it calls ugly *)
let u5 = ugly' 10               (* the stored value is for 2 so it calls ugly and takes a couple of seconds *)
let u6 = ugly' 10               (* the one uses the stored value and returns immediately *)

 *)

(* Q2.2: Write a function that memoizes the last value called. *)
let memo_many (n : int) (f : 'a -> 'b) : 'a -> 'b =
  let inp = Array.make n None
  in
  let res = Array.make n None
  in
  let counter = ref 0
  in
  let check =
    fun x ->
      let write = fun y ->
        let z = f y in
        (counter := ((!counter+1) mod n));
        (Array.set inp !counter (Some y));
        (Array.set res !counter (Some z));
        Some z
      in
      let rec find ind =
        if ind = n then write x
        else
          match Array.get inp ind with
          | None -> find (ind + 1)
          | Some x' -> if x = x' then Array.get res ind
                       else find (ind + 1)
      in
      find 0
  in fun a -> match check a with
  | None -> raise (Invalid_argument "Invalid input")
  | Some a' -> a'

(* Question 3: Doubly-linked circular lists  *)

(* Circular doubly linked lists *)

(* The type of a cell (a non-empty circular list) *)
type 'a cell = { mutable p : 'a cell; data : 'a ; mutable n : 'a cell}

(* The type of possibly empty circular lists *)
type 'a circlist = 'a cell option

(* An empty circular list *)
let empty :'a circlist = None

(* A singleton list that contains a single element *)
let singl (x : 'a) : 'a circlist =
  let rec pointer = {p = pointer ; data = x ; n = pointer} in
  Some pointer

(* Rotate a list to next element *)
let next : 'a circlist -> 'a circlist = function
  | None -> None
  | Some cl -> Some (cl.n)

(* Rotate a list to previous element *)
let prev : 'a circlist -> 'a circlist = function
  | None -> None
  | Some cl -> Some (cl.p)

(* Q3.1: Write a function that add a new element at the beginning of a list *)
let cons (x : 'a)  (xs : 'a circlist) : 'a circlist =
  match xs with
  | None -> singl x
  | Some cl -> let ncl = {p = cl.p; data = x; n = cl} in
               let () = cl.p.n <- ncl in
               let () = cl.p <- ncl in
               Some cl

(* Q3.2: Write a function that computes the length of a list (Careful with the infinite loops)  *)
let rec length (l : 'a circlist) : int =
  match l with
  | None -> 0
  | Some cl -> let rec count current acc=
                 if current == cl then acc
                 else count current.n (acc+1)
               in count cl.n 1


(* Q3.3: Write a function that produces an immutable list from a circular list *)
let to_list (l : 'a circlist)  : 'a list =
  match l with
  | None -> []
  | Some cl -> let rec tlist current acc =
                  if current == cl then acc
                  else tlist current.n (acc@[current.data])
              in
              tlist cl.n [cl.data]

(* Once you've written cons you can use this function to quickly populate your lists *)
let rec from_list : 'a list -> 'a circlist = function
  | [] -> empty
  | x::xs -> cons x (from_list xs)

(* Q3.4: Write a function that reverses all the directions of the list *)
let rev (l : 'a circlist) : 'a circlist =
  match l with
  | None -> None
  | Some cl -> let rec rev' current nlist=
                 if current == cl then nlist
                 else
                   rev' current.p (cons current.p.data nlist)
               in rev' cl.p (singl cl.p.data)


(* (Extra credit) OPTIONAL: Write the map function as applied to lists *)
let map (f : 'a -> 'b) : 'a circlist -> ' b circlist =
  fun list ->
    let rec map' f' (l: 'a circlist) n (l': 'b circlist) =
      match l with
      | None -> None
      | Some cl -> if n = length l then l'
                   else
                     map' f' (next l) (n+1) (cons (f' (cl.data)) l')
    in map' f list 0 None

(* Some possibly useful functions (Wink, wink!) *)

(* A function that returns the Greatest Common Denominator of two numbers *)
let rec gcd (u : int) (v : int) : int =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)

(* A function that returns the least common multiple of two numbers *)
let lcm (m : int) (n : int) : int  =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)


(* (Extra credit) OPTIONAL A function that compares two lists ignoring the rotation *)
let eq (l1 : 'a circlist) (l2 : 'a circlist) : bool =
  match l1, l2 with
  | None, None -> true
  | None, Some _ -> false
  | Some _, None -> false
  | Some cl1, Some cl2 ->   let long = max (length l1) (length l2) in
                            let rec comp c1 c2 (n:int) (count:int) =
                              if count = long then true
                              else if n = 2*long then false
                              else
                                if c1.data = c2.data then comp c1.n c2.n (n+1) (count+1)
                                else
                                  comp c1 c2.n (n+1) 0
                            in comp cl1 cl2 0 0


(* Some examples *)
(*
let ex = cons 12 (cons 43 (cons 34 (singl 3)))
let lex = to_list ex

let l1 = from_list [true; true ; false]
let l3 = from_list [true; true ; false ; true; true ; false]

let l4 = from_list ['a'; 'b'; 'a'; 'b']
let l5 = from_list ['a'; 'b'; 'a'; 'b'; 'a'; 'b']

let l6 = from_list ['a'; 'a']
let l7 = from_list ['a'; 'a'; 'a']

let l8 = from_list [1 ; 2 ; 3]
let l9 = from_list [3 ; 1 ; 2]  (* eq l8 l9 = true *)

let l10 = from_list [1 ; 2 ; 3]
let l11 = from_list [3 ; 2 ; 1]  (* eq l10 l11 = false *)
*)
