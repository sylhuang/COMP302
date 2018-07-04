(* Student information:

   Enter your name, and if you chose to work in pairs, the name of the
   student you worked with (both students MUST submit the solution to
   myCourses):

   Name: Qianyu Huang
   McGill ID: 260669624

   If you worked in pairs, the name of the other student.

   Name: Siyu Ma
   McGill ID: 260686818


 *)

(* Notice: by submitting as part of team, you declare that you worked
   together on the solution. Submissions in pairs are allowed to
   foster team work, they have to be developed by both students *)

(* Homework 1 - Questions 2 and 3 *)

(* First, some utility functions and declarations that you can use. Be
   sure to check Ocaml's documentation to find more functions
   available to you.

   You can start checking the documentation at:
   https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
 *)

(* the value of pi *)
let pi : float =  acos ~-.1.0

(* a function to compare floats that allows for some imprecision *)
let cmp n m = abs_float (n -. m) < 0.0001

(* a simple test of positivity *)
let positive a = a > 0.0

(* a function to check if a is multiple of b *)
let is_multiple_of (a : float) (b : float) : bool =
  let m = a /. b in
  cmp (m -. floor m) 0.0

(* a function to check if a is between plus/minus b *)
let abs_btwn a b = a < b && a > ~-.b

(* Question 2: Triangles are the best *)

type side = float

type tr_by_sides = side * side * side

type tr_kind
  = Scalene
  | Equilateral
  | Isosceles

(* Question 2.1 *)
let well_formed_by_sides (a, b, c : tr_by_sides) : bool =
  let rec compare ((s1:float),(s2:float),(s3:float),n) =
    if n = 0 then true
    else if s1 <= 0.0 then false
    else if (s1 +. s2) <= s3 then false
    else compare (s2,s3,s1,n-1)
  in compare (a,b,c,3)
                  (* write your solutions in place of all the "assert false" *)


(* Question 2.2 *)
let create_triangle (kind : tr_kind) (area : float) : tr_by_sides =
  let sc_side (area:float) =
    let height = Random.float (2.0 *. area) in
    let base = 2.0 *. area /. height in
    let base_x = Random.float (base) in
    let (side1:float) = sqrt (height ** 2.0 +. base_x ** 2.0) in
    let (side2:float) = sqrt (height ** 2.0 +. (base -. base_x) ** 2.0) in
    (side1,side2,base : tr_by_sides)
  in
  let eq_side (area:float) =
    let side = 2.0 *. sqrt (area /. (sqrt 3.0)) in
    (side, side, side)
  in
  let is_side (area:float) =
    let height = Random.float (2.0 *. area) in
    let base = 2.0 *. area /. height in
    let side = sqrt (height ** 2.0 +. (base /. 2.0) ** 2.0) in
    (side, side, base)
  in
  match kind with
  | Scalene -> sc_side area
  | Equilateral -> eq_side area
  | Isosceles -> is_side area


(* Question 2.3 *)
type angle = float

type tr_by_angle = side * side * angle

let well_formed_by_angle (a, b, gamma) : bool =
  (positive a && positive b && positive gamma) &&
    (not (is_multiple_of gamma pi))

let sides_to_angle (a, b, c : tr_by_sides) : tr_by_angle option =
  if well_formed_by_sides (a, b, c) then Some (a, b, acos ((a ** 2.0 +. b ** 2.0 -. c ** 2.0) /. (2.0 *. a *. b)))
  else None

let angle_to_sides (a, b, gamma) =
  if well_formed_by_angle (a, b, gamma) then Some (a, b, sqrt (a ** 2.0 +. b ** 2.0 -. 2.0 *. a *. b *. cos(gamma)))
  else None

(* Now that you implemented Q2.2 and saw the new representation of
   triangles by two sides and the angle between them, also ponder on
   how one represents Q2.2 using this representation. The idea is to
   think about how the representation helps make illegal states hard
   to represent and how easy and hard it is to implement the
   algorithm. *)

(* Question 3: Flexing recursion and lists *)

let even (n : int) : bool = n mod 2 = 0

(* Question 3.1 *)
let evens_first (l : int list) : int list =
  let (even_list, odd_list) = List.partition (fun x -> even x) l in
  List.append even_list odd_list

let ex_1 = evens_first [7 ; 5 ; 2; 4; 6; 3; 4; 2; 1]
(* val ex_1 : int list = [2; 4; 6; 4; 2; 7; 5; 3; 1] *)

(* Question 3.2 *)
let even_streak (l : int list) : int =
  let rec uninter_even (l : int list) (longest : int) (count : int) : int =
    match l with
    | [] -> max longest count
    | h::t -> if even h then uninter_even t longest (count+1)
              else
                uninter_even t (max longest count) 0
  in
  uninter_even l 0 0

let ex_2 = even_streak [7; 2; 4; 6; 3; 4; 2; 1]

(* val ex_2 : int = 3 *)


(* Question 3.3 *)

type nucleobase = A | G | C | T

let compress (l : nucleobase list) : (int * nucleobase) list =
  let rec group (l : nucleobase list) (current : (int*nucleobase) list) : (int * nucleobase) list =
    match l with
    | [] -> List.rev current
    | h::t ->
      let rec collect (l : nucleobase list) (n : nucleobase) (c : int) : int*nucleobase*(nucleobase list) =
        match l with
        | [] -> (c,n,[])
        | h::t -> if h = n then collect t n (c+1)
                  else (c,n,l)
      in let (c,n,l2) = collect t h 1
      in group l2 ((c,n)::current)
  in group l []


let rec decompress (l : (int * nucleobase) list) : nucleobase list =
  let rec unpack (l : (int * nucleobase) list) (current : nucleobase list) : nucleobase list =
    match l with
    | [] -> current
    | (c,n)::t ->
      let rec duplicate ((c,n) : (int * nucleobase)) (acc : nucleobase list) : nucleobase list =
        if c = 0 then acc
        else duplicate (c-1,n) (n::acc)
      in let dup = duplicate (c,n) []
      in unpack t (List.append current dup)
  in unpack l []

let sample_dna : nucleobase list = [A;A;A;A;G;G;A;T;T;T;C;T;C]

let ex_3 = compress sample_dna

let ex_4 = decompress ex_3

let res_3_4 = sample_dna = ex_4 (* This should be true if everything went well *)
