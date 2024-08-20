open Avtomat
open Tipi
open Ide_gas

(* Pomozna funkcija za spremembo avtomata, ki uporabnika prosi za zacetno stanje in zacetni skladovni simbol.
 Funkcija preveri, ce je vnos veljaven, nato pa ponastavi avtomat.*)
let ponastavi_avtomat () =
  print_endline "Vnesi zacetno stanje";
  print_string "> ";
  let zacetno_stanje = read_line () in

  let rec vnos_simbola arg =
    print_endline ("Vnesi" ^ arg);
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
  {
    avtomat = prazen_avtomat zacetno_stanje zacetni_skladovni_simbol;
    stanje_vmesnika = VnosAvtomata;
    prazni_simbol;
  }

(* Genericna pomozna funkcija za spremembo avtomata, ki vnese simbole dane abecede.*)
let vnesi_simbole f model =
  print_endline "Vnesi niz z želenimi simboli";
  print_string "> ";
  let simboli = read_line () |> explode in
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (f x acc) xs
  in
  {
    model with
    avtomat = aux model.avtomat simboli
  }

(* Genericna pomozna funkcija za spremembo avtomata, ki vnese zeleno stanje.*)
let vnesi_stanje f model =
  print_endline "vnesi želeno novo stanje";
  print_string "> ";
  let stanje = read_line () in
  {
    model with
    avtomat = f stanje model.avtomat
  }

(* Pomozna funkcija za spremembo avtomata, ki vnese prehod. *)
let rec vnesi_prehod model =
  let avtomat = model.avtomat in
  let prazni_simbol = (String.make 1 model.prazni_simbol) in
  (* Vnos prehoda razdelimo na dva dela; vhodni in izhodni del relacije. *)
  print_endline "Vnesi vhodno trojico oblike \"(stanje, vhodni simbol, skladovni simbol)\".";
  (* Pri tem tekstovnem vmesniku je crka E rezervirana za prazni simbol. Ni blo druge:(( *)
  print_endline ("Za vnos praznega simbola uporabi črko " ^ prazni_simbol ^ ".");
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
        vnesi_prehod model
      )
    (* Ker sta skladovni in vhodni simbol lahko tudi prazni niz, vsakic izlocimo tudi to moznost. *)
    else if not ((List.mem vhodni_simbol.[0] avtomat.vhodna_abeceda) || (vhodni_simbol = prazni_simbol)) then
      (
        print_endline ("Vhodna abeceda avtomata ne vsebuje simbola " ^ vhodni_simbol);
        vnesi_prehod model
      )
    else if not (List.mem skladovni_simbol.[0] avtomat.skladovna_abeceda || (skladovni_simbol = prazni_simbol)) then
      (
        print_endline ("Skladovna abeceda avtomata ne vsebuje simbola " ^ skladovni_simbol);
        vnesi_prehod model
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
      {
        model with
        avtomat = dodaj_prehod stanje vhodni_simbol' skladovni_simbol' stanje2 skladovni_niz' avtomat
      }
  else
    (
      print_endline "napačen vnos";
      vnesi_prehod model
    )

(* Funkcija, ki nam izpise moznosti za vnos/spremembo trenutnega avtomata. *)
let rec izpisi_moznosti_vnosa model =
  print_endline "0) ponastavi avtomat";
  print_endline "1) dodaj vhodne simbole";
  print_endline "2) dodaj skladovne simbole";
  print_endline "3) dodaj sprejemno stanje";
  print_endline "4) dodaj nesprejemno stanje";
  print_endline "5) dodaj prehod";
  print_endline "6) nazaj";
  print_string "> ";
  match read_line () with
  | "0" -> ponastavi_avtomat ()
  | "1" -> vnesi_simbole dodaj_vhodni_simbol model
  | "2" -> vnesi_simbole dodaj_skladovni_simbol model
  | "3" -> vnesi_stanje dodaj_sprejemno_stanje model
  | "4" -> vnesi_stanje dodaj_nesprejemno_stanje model
  | "5" -> vnesi_prehod model
  | "6" -> { model with stanje_vmesnika = SeznamMoznosti }
  | _ ->
     print_endline "Napačen vnos, vnesi število od 0 do 6";
     izpisi_moznosti_vnosa model
