open Avtomat
open Tipi
open Zagon

(* Pomozna funkcija za spremembo avtomata, ki uporabnika prosi za zacetno stanje in zacetni skladovni simbol.
 Funkcija preveri, ce je vnos veljaven, nato pa ponastavi avtomat.*)
let ponastavi_avtomat () =
  print_endline "Vnesi začetno stanje";
  print_string "> ";
  let zacetno_stanje = read_line () in

  let rec vnos_simbola arg =
    print_endline ("Vnesi " ^ arg);
    print_string "> ";
    let simb = read_line () in
    if (String.length simb) = 1 then
      simb.[0]
    else
      (
        print_endline "Napačen vnos, vnesi simbol";
        vnos_simbola arg
      )
  in
  let zacetni_skladovni_simbol = vnos_simbola "začetni skladovni simbol" in
  let prazni_simbol = vnos_simbola "simbol za prazni niz" in
  prazen_avtomat zacetno_stanje zacetni_skladovni_simbol prazni_simbol


(* Genericna pomozna funkcija za spremembo avtomata, ki vnese simbole dane abecede.*)
let vnesi_simbole f avtomat =
  print_endline "Vnesi niz z želenimi simboli";
  print_string "> ";
  let simboli = read_line () |> explode in
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (f x acc) xs
  in
  aux avtomat simboli

(* Genericna pomozna funkcija za spremembo avtomata, ki vnese zeleno stanje.*)
let vnesi_stanje f avtomat =
  print_endline "Vnesi želeno novo stanje";
  print_string "> ";
  let stanje = read_line () in
  f stanje avtomat


(* Pomozna funkcija za spremembo avtomata, ki vnese prehod. *)
let rec vnesi_prehod avtomat =
  let prazni_simbol = (String.make 1 avtomat.prazni_simbol) in
  (* Vnos prehoda razdelimo na dva dela; vhodni in izhodni del relacije. *)
  print_endline "Vnesi vhodno trojico oblike \"(stanje, vhodni simbol, skladovni simbol)\".";
  print_endline ("Za vnos praznega simbola uporabi simbol " ^ prazni_simbol ^ ".");
  print_string "> ";
  (* Z regexom preberemo vnos in se zapeljemo skozi vse mozne napacne vnose.
   Ce je vnos vhodne trojice sprejemljiv, nam vmesnik ponudi vnos izhodne dvojice *)
  let rg = Str.regexp "(\\(.+?\\), \\(.\\), \\(.\\))" in
  let str = read_line () in
  if Str.string_match rg str 0 then
    let stanje = Str.matched_group 1 str in
    let vhodni_simbol = (Str.matched_group 2 str) in
    let skladovni_simbol = (Str.matched_group 3 str) in
    if not (List.mem stanje avtomat.stanja) then
      (
        print_endline ("Avtomat ne vsebuje stanja " ^ stanje);
        vnesi_prehod avtomat
      )
    (* Ker sta skladovni in vhodni simbol lahko tudi prazni niz, vsakic izlocimo tudi to moznost. *)
    else if not ((List.mem vhodni_simbol.[0] avtomat.vhodna_abeceda) || (vhodni_simbol = prazni_simbol)) then
      (
        print_endline ("Vhodna abeceda avtomata ne vsebuje simbola " ^ vhodni_simbol);
        vnesi_prehod avtomat
      )
    else if not (List.mem skladovni_simbol.[0] avtomat.skladovna_abeceda || (skladovni_simbol = prazni_simbol)) then
      (
        print_endline ("Skladovna abeceda avtomata ne vsebuje simbola " ^ skladovni_simbol);
        vnesi_prehod avtomat
      )
    else
      let rec aux () =
        print_endline "Vnesi izhodno dvojico oblike \"(stanje, skladovni niz)\"";
        print_string "> ";
        let rg = Str.regexp "(\\(.+\\), \\(.+\\))" in
        let str = read_line () in
        if Str.string_match rg str 0 then
          let stanje = Str.matched_group 1 str in
          let skladovni_niz = Str.matched_group 2 str in
          let preveri_skladovni_niz =
            List.fold_left
              (fun acc x -> acc && (List.mem x avtomat.skladovna_abeceda))
              true
              (explode skladovni_niz)
          in
          if not (List.mem stanje avtomat.stanja) then
            (
              print_endline ("Avtomat ne vsebuje stanja " ^ stanje);
              aux ()
            )
          (* Tudi v skladovnem nizu se lahko pojavi prazni niz, a le ce je to edini element. *)
          else if not (preveri_skladovni_niz || (skladovni_niz = prazni_simbol)) then
            (
              print_endline "Skladovni niz vsebuje simbol zunaj skladovne abecede";
              aux ()
            )
          else
            (stanje, skladovni_niz)
        else
          (
            print_endline "Napačen vnos";
            aux ()
          )
      in
      let (stanje2, skladovni_niz) = aux () in
      let vhodni_simbol' = if vhodni_simbol = prazni_simbol then Eps () else Crka vhodni_simbol.[0] in
      let skladovni_simbol' = if skladovni_simbol = prazni_simbol then Eps () else Crka skladovni_simbol.[0] in
      let skladovni_niz' = if skladovni_niz = prazni_simbol then [] else explode skladovni_niz in
      dodaj_prehod stanje vhodni_simbol' skladovni_simbol' stanje2 skladovni_niz' avtomat
  else
    (
      print_endline "napačen vnos";
      vnesi_prehod avtomat
    )

(* Funkcija, ki nam izpise moznosti za vnos/spremembo trenutnega avtomata. *)
let rec izpisi_moznosti_vnosa avtomat =
  print_endline "Vnesi število od 0 do 6";
  print_endline "0) ponastavi avtomat";
  print_endline "1) dodaj vhodne simbole";
  print_endline "2) dodaj skladovne simbole";
  print_endline "3) dodaj sprejemno stanje";
  print_endline "4) dodaj nesprejemno stanje";
  print_endline "5) dodaj prehod";
  print_endline "6) shrani avtomat";
  print_endline "7) nazaj";
  print_string "> ";
  match read_line () with
  | "0" -> ZamenjajAvtomat (ponastavi_avtomat ())
  | "1" -> ZamenjajAvtomat (vnesi_simbole dodaj_vhodni_simbol avtomat)
  | "2" -> ZamenjajAvtomat (vnesi_simbole dodaj_skladovni_simbol avtomat)
  | "3" -> ZamenjajAvtomat (vnesi_stanje dodaj_sprejemno_stanje avtomat)
  | "4" -> ZamenjajAvtomat (vnesi_stanje dodaj_nesprejemno_stanje avtomat)
  | "5" -> ZamenjajAvtomat (vnesi_prehod avtomat)
  | "6" -> ShraniAvtomat avtomat
  | "7" -> ZamenjajVmesnik SeznamMoznosti
  | _ ->
     print_endline "Napačen vnos, vnesi število od 0 do 7";
     izpisi_moznosti_vnosa avtomat
