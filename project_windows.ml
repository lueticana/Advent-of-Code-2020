let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

module List = struct
  include List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let lines = String.split_on_char '\n'
end

module type Solver = sig
  val naloga1 : string -> string

  val naloga2 : string -> string -> string
end

module Solver0 : Solver = struct
  let cost_fun x = (x / 3) - 2

  let rec full_cost x =
    let c_cost = cost_fun x in
    if c_cost <= 0 then 0 else c_cost + full_cost c_cost

  let naloga1 data =
    let lines = List.lines data in
    lines |> List.int_list
    |> List.fold_left (fun s x -> s + cost_fun x) 0
    |> string_of_int

  let naloga2 data _part1 =
    data |> List.lines |> List.int_list |> List.map full_cost |> List.sum
    |> string_of_int
end

(* Tukaj re-definirajte funkcijo naloga1 in naloga2 *)
module Solver1 : Solver = struct
  let naloga1 vsebina_datoteke =
    let odrezana = String.trim vsebina_datoteke in
    let lines = List.lines odrezana in
    let stevila = List.int_list lines in
    let rec prva_rekurzija l =
        match l with
        | [] -> failwith "Ni taksnih stevil."
        | x :: xs -> 
            let rec druga_rekurzija li =
                match li with
                | [] -> prva_rekurzija xs
                | y :: ys -> if x + y = 2020 then x * y else druga_rekurzija ys
            in
            druga_rekurzija xs
    in
    string_of_int (prva_rekurzija stevila)

  let naloga2 vsebina_datoteke _part1=
    let odrezana = String.trim vsebina_datoteke in
    let lines = List.lines odrezana in
    let stevila = List.int_list lines in
    let rec prva_rekurzija l =
        match l with
        | [] -> failwith "Ni taksnih stevil."
        | x :: xs -> 
            let rec druga_rekurzija l2 =
                match l2 with
                | [] -> prva_rekurzija xs
                | y :: ys -> 
                  let rec tretja_rekurzija l3 =
                    match l3 with
                    | [] -> druga_rekurzija ys
                    | z :: zs -> if x + y + z = 2020 then x * y * z else tretja_rekurzija zs
                  in
                  tretja_rekurzija ys
            in
            druga_rekurzija xs
    in
    string_of_int (prva_rekurzija stevila)  
end

module Solver2 : Solver = struct
  let naloga1 vsebina_datoteke =
    let vrstice = List.lines vsebina_datoteke in
    let rec pravilni seznam stevilo_pravilnih =
      match seznam with
      | [] -> stevilo_pravilnih
      | vrstica :: ostale ->
        let vrstica = String.split_on_char ' ' vrstica in
        match vrstica with
        | [] -> failwith "Napaka"
        | a :: rest -> match String.split_on_char '-' a with
          | [] -> failwith "Napaka"
          | min :: b -> let min = int_of_string min in match b with
            | [] -> failwith "Napaka"
            | max :: _ -> let max = int_of_string max in
          match rest with
          | [] -> failwith "Napaka"
          | c :: ostanek_geslo -> let crka = String.get c 0 in 
            match ostanek_geslo with
            | [] -> failwith "Napaka"
            | geslo :: _ -> let geslo = geslo in
      let rec stetje str n =
        let dolzina = String.length str in
          if String.get str 0 = crka then
            begin
            if dolzina > 1 then stetje (String.sub str 1 (dolzina - 1)) (n + 1)
            else if min <= (n + 1) && (n + 1) <= max then (pravilni ostale (stevilo_pravilnih + 1))
            else pravilni ostale stevilo_pravilnih
            end
          else if dolzina > 1 then stetje (String.sub str 1 (dolzina - 1)) n
          else if min <= n && n <= max then (pravilni ostale (stevilo_pravilnih + 1))
          else pravilni ostale stevilo_pravilnih
      in
      stetje geslo 0
    in
    string_of_int (pravilni vrstice 0)

          

  let naloga2 vsebina_datoteke _part1 =
    let vrstice = List.lines vsebina_datoteke in
    let rec pravilni seznam stevilo_pravilnih =
      match seznam with
      | [] -> stevilo_pravilnih
      | vrstica :: ostale ->
        let vrstica = String.split_on_char ' ' vrstica in
        match vrstica with
        | [] -> failwith "Napaka"
        | a :: rest -> match String.split_on_char '-' a with
          | [] -> failwith "Napaka"
          | prvi :: b -> let prvi = int_of_string prvi - 1 in match b with
            | [] -> failwith "Napaka"
            | drugi :: _ -> let drugi = int_of_string drugi - 1 in
          match rest with
          | [] -> failwith "Napaka"
          | c :: ostanek_geslo -> let crka = String.get c 0 in 
            match ostanek_geslo with
            | [] -> failwith "Napaka"
            | geslo :: _ -> let geslo = geslo in
      if ((String.get geslo prvi = crka || String.get geslo drugi = crka)
            && not (String.get geslo prvi = crka && String.get geslo drugi = crka))
        then pravilni ostale (stevilo_pravilnih + 1)
      else pravilni ostale stevilo_pravilnih
    in 
    string_of_int (pravilni vrstice 0)
end

(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | _ -> failwith "Ni še rešeno"

let main () =
  let day = Sys.argv.(1) in
  print_endline ("Solving DAY: " ^ day);
  let (module Solver) = choose_solver day in
  let input_data = preberi_datoteko ("data/day_" ^ day ^ ".in") in
  let p1_start = Sys.time () in
  let part1 = Solver.naloga1 input_data in
  let t1_time = Sys.time () -. p1_start in
  print_endline "PART 1:";
  print_endline part1;
  print_endline ("Taken: " ^ string_of_float t1_time ^ "s");
  let p2_start = Sys.time () in
  let part2 = Solver.naloga2 input_data part1 in
  let t2_time = Sys.time () -. p2_start in
  print_endline "PART 2:";
  print_endline part2;
  print_endline ("Taken: " ^ string_of_float t2_time ^ "s");
  print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");
  izpisi_datoteko ("out/day_" ^ day ^ "_1.out") part1;
  izpisi_datoteko ("out/day_" ^ day ^ "_2.out") part2;
  ()

let _ = main ()