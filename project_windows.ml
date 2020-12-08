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

module Solver3 : Solver = struct
  let naloga1 data =
    let vrste = List.lines data in
    let dolzina = String.length (List.hd vrste) in
    let rec stevilo_dreves seznam stevec index =
      match seznam with
      | [] -> stevec
      | vrsta :: rest ->
        if String.get vrsta index = '#'
          then stevilo_dreves rest (stevec + 1) ((index + 3) mod dolzina)
        else stevilo_dreves rest stevec ((index + 3) mod dolzina)
    in
    string_of_int (stevilo_dreves vrste 0 0)



  let naloga2 data _part1 =
    let vrste = List.lines data in
    let dolzina = String.length (List.hd vrste) in
    let rec stevilo_dreves seznam stevec premik_dol premik_desno index =
      match seznam with
      | [] -> stevec
      | vrsta :: spuscena :: rest when premik_dol = 2 ->
        if String.get vrsta index = '#'
          then stevilo_dreves rest (stevec + 1) premik_dol premik_desno ((index + premik_desno) mod dolzina)
        else stevilo_dreves rest stevec premik_dol premik_desno ((index + premik_desno) mod dolzina)
      | vrsta :: rest when premik_dol = 2 ->
        if String.get vrsta index = '#'
          then stevec + 1
        else stevec
      | vrsta :: rest ->
        if String.get vrsta index = '#'
          then stevilo_dreves rest (stevec + 1) premik_dol premik_desno ((index + premik_desno) mod dolzina)
        else stevilo_dreves rest stevec premik_dol premik_desno ((index + premik_desno) mod dolzina)
    in
    string_of_int (
      stevilo_dreves vrste 0 1 1 0*
      stevilo_dreves vrste 0 1 3 0*
      stevilo_dreves vrste 0 1 5 0*
      stevilo_dreves vrste 0 1 7 0*
      stevilo_dreves vrste 0 2 1 0)
end

module Solver4 : Solver = struct
  let naloga1 data =
    let lines = List.lines data in
    let rec stevilo_valid seznam valid vsebuje_cid stevec=
      match seznam with
      | [] -> valid
      | "" :: rest ->
        if  stevec = 8 || (stevec = 7 && not vsebuje_cid)
          then stevilo_valid rest (valid + 1) false 0
        else stevilo_valid rest valid false 0
      | vrstica :: rest -> 
        let loceni = String.split_on_char ' ' vrstica in
        let rec stetje li vsebuje st =
          match li with
          | [] -> stevilo_valid rest valid vsebuje st
          | x :: xs ->
            if List.hd (String.split_on_char ':' x) = "cid"
              then stetje xs true (st + 1)
            else stetje xs vsebuje (st + 1)
        in
        stetje loceni vsebuje_cid stevec
    in
    string_of_int (stevilo_valid lines 0 false 0)

  let naloga2 data _part1 =
    let lines = List.lines data in
    let rec stevilo_valid seznam valid stevec=
      match seznam with
      | [] -> valid
      | "" :: rest ->
        if  stevec = 7
          then stevilo_valid rest (valid + 1)  0
        else stevilo_valid rest valid  0
      | vrstica :: rest -> 
        let loceni = String.split_on_char ' ' vrstica in
        let rec stetje li st =
          match li with
          | [] -> stevilo_valid rest valid st
          | x :: xs ->
            let par = (String.split_on_char ':' x) in
            match par with
              | [] -> stetje xs st
              | "byr" :: vrednost ->
                let vred = int_of_string (List.hd vrednost) in
                if 1920 <= vred && vred <= 2002
                  then stetje xs (st + 1)
                else stevilo_valid rest valid 0
              | "iyr" :: vrednost ->
                let vred = int_of_string (List.hd vrednost) in
                if 2010 <= vred && vred <= 2020
                  then stetje xs (st + 1)
                else stevilo_valid rest valid 0
              | "eyr" :: vrednost ->
                let vred = int_of_string (List.hd vrednost) in
                if 2020 <= vred && vred <= 2030
                  then stetje xs (st + 1)
                else stevilo_valid rest valid 0
              | "hgt" :: vrednost ->
                let vred = List.hd vrednost in
                if ( String.length vred = 5 &&
                  vred.[3] = 'c' &&
                  vred.[4] = 'm' &&
                  150 <= int_of_string (String.sub vred 0 (String.length vred - 2)) &&
                  int_of_string (String.sub vred 0 (String.length vred - 2)) <= 193) ||
                  (String.length vred = 4 &&
                  vred.[2] = 'i' &&
                  vred.[3] = 'n' &&
                  59 <= int_of_string (String.sub vred 0 (String.length vred - 2)) &&
                  int_of_string (String.sub vred 0 (String.length vred - 2)) <= 76)
                  then stetje xs (st + 1)
                else stevilo_valid rest valid 0
              | "hcl" :: vrednost ->
                let vred = List.hd vrednost in
                if vred.[0] = '#' then
                  begin
                  let rec preveri_znak str index =
                    if index > (String.length str - 1)
                      then stetje xs (st + 1)
                    else if List.mem str.[index] ['1';'2';'3';'4';'5';'6';'7';'8';'9';'0';'a';'b';'c';'d';'e';'f']
                      then preveri_znak str (index + 1)
                    else stevilo_valid rest valid 0
                  in
                  preveri_znak vred 1
                  end
                else stevilo_valid rest valid 0
              | "ecl" :: vrednost ->
                let vred = List.hd vrednost in
                if vred = "amb" || vred = "blu" || vred = "brn" || vred = "gry" || vred = "grn" || vred = "hzl" || vred = "oth"
                  then stetje xs (st + 1)
                else stevilo_valid rest valid 0
              | "pid" :: vrednost ->
                let vred = List.hd vrednost in
                if String.length vred = 9
                  then stetje xs (st + 1)
                else stevilo_valid rest valid 0   
              | _ :: _ -> stetje xs st   
        in
        stetje loceni stevec
    in
    string_of_int (stevilo_valid lines 0 0)
end

module Solver5 : Solver = struct
  let naloga1 data =
    let lines = List.lines data in
    let rec max_id sez_vrst najvecji =
      match sez_vrst with
      | [] -> najvecji
      | vrstica :: ostale ->
        let rec dobimo_vrsto str min max index =
          if index = 7 then min
          else if str.[index] = 'F' then
            dobimo_vrsto str min ((min + max - 1) / 2) (index + 1)
          else
            dobimo_vrsto str ((min + max + 1) / 2) max (index + 1)
        in
        let vrsta = dobimo_vrsto vrstica 0 127 0 in
        let rec dobimo_sedez str min max index =
          if index = 3 then min
          else if str.[index + 7] = 'L'then
            dobimo_sedez str min ((min + max - 1) / 2) (index + 1)
          else
            dobimo_sedez str ((min + max + 1) / 2) max (index + 1)
        in
        let sedez = dobimo_sedez vrstica 0 7 0 in
        let zmnozek = vrsta * 8 + sedez in
        max_id ostale (max najvecji zmnozek)
    in
    string_of_int (max_id lines 0)
      

  let naloga2 data _part1 = 
    let lines = List.lines data in
    let rec sum sez_vrst vsota =
      match sez_vrst with
      | [] -> vsota
      | vrstica :: ostale ->
        let rec dobimo_vrsto str min max index =
          if index = 7 then min
          else if str.[index] = 'F' then
            dobimo_vrsto str min ((min + max - 1) / 2) (index + 1)
          else
            dobimo_vrsto str ((min + max + 1) / 2) max (index + 1)
        in
        let vrsta = dobimo_vrsto vrstica 0 127 0 in
        let rec dobimo_sedez str min max index =
          if index = 3 then min
          else if str.[index + 7] = 'L'then
            dobimo_sedez str min ((min + max - 1) / 2) (index + 1)
          else
            dobimo_sedez str ((min + max + 1) / 2) max (index + 1)
        in
        let sedez = dobimo_sedez vrstica 0 7 0 in
        let zmnozek = vrsta * 8 + sedez in
        sum ostale (vsota + zmnozek)
    in
    let rec sestej do_ sum stevilo =
      if stevilo = do_ then (sum + stevilo)
      else sestej do_ (sum + stevilo) (stevilo + 1)
    in
    string_of_int ((sestej 818 0 48) - (sum lines 0))

end

module Solver6 : Solver = struct
  let naloga1 data =
    let lines = List.lines data in
    let rec rekurzija list vsota seznam =
      match list with
      | [] -> (vsota + List.length seznam)
      | "" :: rest -> rekurzija rest (vsota + List.length seznam) []
      | vrstica :: rest ->
        let rec for_loop vrstica index seznam =
          if index = String.length vrstica
            then rekurzija rest vsota seznam
          else if List.mem vrstica.[index] seznam
            then for_loop vrstica (index + 1) seznam
          else for_loop vrstica (index + 1) (vrstica.[index] :: seznam) 
        in
        for_loop vrstica 0 seznam
    in
    string_of_int (rekurzija lines 0 [])

  let naloga2 data _part1 =
    let lines = "" :: List.lines data in
    let string_to_list s =
      let rec rek i l =
        if i < 0 then l
        else rek (i - 1) (s.[i] :: l) 
      in
      rek (String.length s - 1) []
    in
    let rec rekurzija list vsota seznam =
      match list with
      | [] -> (vsota + List.length seznam)
      | "" :: vrstica :: rest -> rekurzija rest (vsota + List.length seznam) (string_to_list vrstica)
      | vrstica :: rest ->
        let rec for_loop index nov_seznam =
          if index >= String.length vrstica
            then rekurzija rest vsota nov_seznam
          else if List.mem vrstica.[index] seznam && not (List.mem vrstica.[index] nov_seznam)
            then for_loop (index + 1) (vrstica.[index] :: nov_seznam)
          else for_loop (index + 1) nov_seznam 
        in
        for_loop 0 []
    in
    string_of_int (rekurzija lines 0 [])
end

module Solver7 : Solver = struct
  type vrstica = {vsebuje : string; vsebovane : string list}
  let naloga1 data =
    let lines = List.lines data in
    let rec predelane_vrstice sez predelane =
      match sez with
      | [] -> predelane
      | vrstica :: rest ->
        let locene = String.split_on_char ' ' vrstica in
        let vsebuje = List.hd locene ^ List.nth locene 1 in
        let rec naberi sez nabrane index =
          if index >= List.length sez then nabrane
          else naberi sez (((List.nth sez index) ^ (List.nth sez (index + 1))) :: nabrane) (index + 4)
        in
        let nabrane = naberi locene [] 5 in
        predelane_vrstice rest ({vsebuje = vsebuje; vsebovane = nabrane} :: predelane)
    in
    let predelane = predelane_vrstice lines [] in
    let rec poisci seznam str index lahko_vsebujejo =
      if index >= List.length seznam then lahko_vsebujejo
      else
        let vrstica = List.nth seznam index in
        if List.mem str vrstica.vsebovane && not (List.mem vrstica.vsebuje lahko_vsebujejo)
          then
          poisci seznam str (index + 1) (poisci seznam vrstica.vsebuje 0 (vrstica.vsebuje :: lahko_vsebujejo))
        else
          poisci seznam str (index + 1) lahko_vsebujejo
    in
    string_of_int (List.length (poisci predelane "shinygold" 0 []))

  let naloga2 data _part1 =
    let lines = List.lines data in
    let rec predelane_vrstice sez predelane =
      match sez with
      | [] -> predelane
      | vrstica :: rest ->
        let locene = String.split_on_char ' ' vrstica in
        let vsebuje = List.hd locene ^ List.nth locene 1 in
        let rec naberi sez nabrane index =
          if index >= List.length sez then nabrane
          else naberi sez (((List.nth sez index) ^ (List.nth sez (index + 1)) ^ (List.nth sez (index + 2))) :: nabrane) (index + 4)
        in
        let nabrane = naberi locene [] 4 in
        predelane_vrstice rest ({vsebuje = vsebuje; vsebovane = nabrane} :: predelane)
    in
    let predelane = predelane_vrstice lines [] in
    let rec prestej seznam str index stevec =
      if index >= List.length seznam then stevec
      else
        let vrstica = List.nth seznam index in
        if vrstica.vsebuje = str
          then
          let rec znotraj sez vsota =
            match sez with
            | [] -> vsota
            | a :: rest -> 
              if a = "nootherbags." then vsota
              else
              let st = int_of_string (Char.escaped a.[0]) in
              znotraj rest (vsota + (st * (prestej seznam (String.sub a 1 (String.length a - 1)) 0 1)))
          in
          znotraj vrstica.vsebovane stevec
        else prestej seznam str (index + 1) stevec
    in
    string_of_int (prestej predelane "shinygold" 0 0)




end

module Solver8 : Solver = struct
  let naloga1 data =
    let lines = List.lines data in
    let rec rekurzija acc index seznam_pozicij =
      if List.mem index seznam_pozicij then acc
      else
        let vrstica = List.nth lines index in
        let operacija = String.sub vrstica 0 3 in
        let stevilo = int_of_string (String.sub vrstica 4 (String.length vrstica - 4)) in
        match operacija with
        | "acc" -> rekurzija (acc + stevilo) (index + 1) (index :: seznam_pozicij) 
        | "jmp" -> rekurzija acc (index + stevilo) (index :: seznam_pozicij)
        | "nop" -> rekurzija acc (index + 1) (index :: seznam_pozicij)
        | _ -> failwith "Ni ukaz"
    in
    string_of_int (rekurzija 0 0 [])


  let naloga2 data _part1 =
    let lines = List.lines data in
    let rec rekurzija acc index seznam_pozicij spremenjen =
      if index >= List.length lines then acc
      else if List.mem index seznam_pozicij
        then rekurzija 0 0 [] (spremenjen + 1)
      else
        let vrstica = List.nth lines index in
        let operacija = String.sub vrstica 0 3 in
        let stevilo = int_of_string (String.sub vrstica 4 (String.length vrstica - 4)) in
        if index = spremenjen then
          match operacija with
          | "acc" -> rekurzija 0 0 [] (spremenjen + 1)
          | "jmp" -> rekurzija acc (index + 1) (index :: seznam_pozicij) spremenjen
          | "nop" -> rekurzija acc (index + stevilo) (index :: seznam_pozicij) spremenjen
          | _ -> failwith "Ni ukaz"
        else
          match operacija with
          | "acc" -> rekurzija (acc + stevilo) (index + 1) (index :: seznam_pozicij) spremenjen
          | "jmp" -> rekurzija acc (index + stevilo) (index :: seznam_pozicij) spremenjen
          | "nop" -> rekurzija acc (index + 1) (index :: seznam_pozicij) spremenjen
          | _ -> failwith "Ni ukaz"
    in
    string_of_int (rekurzija 0 0 [] 0)
end


module Solver10 : Solver = struct
  let naloga1 data = ""

  let naloga2 data _part1 = ""
end

(* Poženemo zadevo *)
let choose_solver : string -> (module Solver) = function
  | "0" -> (module Solver0)
  | "1" -> (module Solver1)
  | "2" -> (module Solver2)
  | "3" -> (module Solver3)
  | "4" -> (module Solver4)
  | "5" -> (module Solver5)
  | "6" -> (module Solver6)
  | "7" -> (module Solver7)
  | "8" -> (module Solver8)
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