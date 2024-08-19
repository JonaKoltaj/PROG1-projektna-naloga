open Koncni_avtomati.Avtomat
open Koncni_avtomati.Ide_gas

(* Avtomat sprejema nize oblike 0^n1^n in zavrača vse druge binarne nize. *)
let nicle_in_enice =
  prazen_avtomat "p" 'Z'
  |> dodaj_sprejemno_stanje "r"
  |> dodaj_nesprejemno_stanje "q"
  |> dodaj_vhodni_simbol '0'
  |> dodaj_vhodni_simbol '1'
  |> dodaj_skladovni_simbol 'A'
  |> dodaj_prehod "p" (Crka '0') (Crka 'Z') "p" ['A'; 'Z']
  |> dodaj_prehod "p" (Crka '0') (Crka 'A') "p" ['A'; 'A']
  |> dodaj_prehod "p" (Eps ()  ) (Crka 'Z') "q" ['Z']
  |> dodaj_prehod "p" (Eps ()  ) (Crka 'A') "q" ['A']
  |> dodaj_prehod "q" (Crka '1') (Crka 'A') "q" []
  |> dodaj_prehod "q" (Eps ()  ) (Crka 'Z') "r" ['Z']
(* let test = pozeni nicle_in_enice "00001111" *)

(* let () = print_endline (Bool.to_string test) *)

type stanje_vmesnika =
  | SeznamMoznosti
  | IzpisAvtomata
  | VnosAvtomata
  | BranjeNiza
  | RezultatPrebranegaNiza
  | OpozoriloONapacnemNizu

(* Model vsebuje podatke o avtomatu, koncnem stanju avtomata, skladu in stanju vmesnika. *)
type model = {
    avtomat : avtomat;
    stanje_avtomata : stanje;
    sklad : char list;
    stanje_vmesnika : stanje_vmesnika
  }

type msg =
  | PreberiNiz of string
  | ZamenjajVmesnik of stanje_vmesnika
  | VrniVPrvotnoStanje

let update model = function
  | PreberiNiz _ -> _
  | ZamenjajVmesnik stanje_vmesnika -> { model with stanje_vmesnika }
  | VrniVPrvotnoStanje ->
     {
       model with
       stanje_avtomata = model.avtomat.zacetno_stanje;
       sklad = [ model.avtomat.zacetni_skladovni_simbol ];
       stanje_vmesnika = SeznamMoznosti;
     }

(* Tekstoni vmesnik nam ponudi moznost trenuten avtomat spremeniti, ga izpisati, skozi avtomat pognati niz in ponastaviti stanje trenutnega avtomata. *)
(* TO DO Shrani avtomat, uploadaj avtomat *)
let rec izpisi_moznosti () =
  print_endline "0) spremeni avtomat";
  print_endline "1) izpiši avtomat";
  print_endline "2) beri niz";
  print_endline "3) ponastavi stanje avtomata";
  print_string "> ";
  match read_line () with
  | "0" -> ZamenjajVmesnik VnosAvtomata
  | "1" -> ZamenjajVmesnik IzpisAvtomata
  | "2" -> ZamenjajVmesnik BranjeNiza
  | "3" -> VrniVPrvotnoStanje
  | _ ->
     print_endline "Napačen vnos, vnesi 0, 1, 2 ali 3";
     izpisi_moznosti ()

(* Pomozna funkcija za spremembo avtomata, ki uporabnika prosi za zacetno stanje in zacetni skladovni simbol.
 Funkcija preveri, ce je vnos veljaven, nato pa ponastavi avtomat.*)
let ponastavi_avtomat () =
  print_endline "Vnesi zacetno stanje";
  print_string "> ";
  let zacetno_stanje = read_line () in

  let rec vnos_skld_simb () =
    print_endline "Vnesi zacetni skladovni simbol";
    print_string "> ";
    let simb = read_line () in
    if (String.length simb) = 1 then
      simb.[0]
    else
      (
        print_endline "Napačen vnos, vnesi simbol";
        vnos_skld_simb ()
      )
  in
  let zacetni_skladovni_simbol = vnos_skld_simb () in
  {
    avtomat = prazen_avtomat zacetno_stanje zacetni_skladovni_simbol;
    stanje_avtomata = zacetno_stanje;
    sklad = [ zacetni_skladovni_simbol ];
    stanje_vmesnika = VnosAvtomata;
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
  (* Vnos prehoda razdelimo na dva dela; vhodni in izhodni del relacije. *)
  print_endline "Vnesi vhodno trojico oblike \"(stanje, vhodni simbol, skladovni simbol)\".";
  (* Pri tem tekstovnem vmesniku je crka E rezervirana za prazni simbol. Ni blo druge:(( *)
  print_endline "Za vnos praznega simbola uporabi črko E.";
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
    else if not ((List.mem vhodni_simbol.[0] avtomat.vhodna_abeceda) || (vhodni_simbol = "E")) then
      (
        print_endline ("Vhodna abeceda avtomata ne vsebuje simbola " ^ vhodni_simbol);
        vnesi_prehod model
      )
    else if not (List.mem skladovni_simbol.[0] avtomat.skladovna_abeceda || (skladovni_simbol = "E")) then
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
          else if not (preveri_skladovni_niz || (skladovni_niz = "E")) then
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
      let vhodni_simbol' = if vhodni_simbol = "E" then Eps () else Crka vhodni_simbol.[0] in
      let skladovni_simbol' = if skladovni_simbol = "E" then Eps () else Crka skladovni_simbol.[0] in
      let skladovni_niz' = if skladovni_niz = "E" then [] else explode skladovni_niz in
      {
        model with
        avtomat = dodaj_prehod stanje vhodni_simbol' skladovni_simbol' stanje2 skladovni_niz' avtomat
      }
  else
    (
      print_endline "napačen vnos";
      vnesi_prehod model
    )


let testni_model =
  {
    avtomat = nicle_in_enice;
    stanje_avtomata = nicle_in_enice.zacetno_stanje;
    sklad = [ nicle_in_enice.zacetni_skladovni_simbol ];
    stanje_vmesnika = VnosAvtomata
  }

let test = vnesi_prehod testni_model
(* let test2 = *)
(*   let rg = Str.regexp "(\\(.+?\\), \\(.\\), \\(.\\))" in *)
(*   let _ = Str.string_match rg "(p, 0, Z)" 0 in *)
(*   print_endline (Str.matched_group 1 "(p, 0, Z)") *)




(* To je funkcija, ki nam izpise moznosti za vnos/spremembo trenutnega avtomata. *)
let rec izpisi_moznosti_vnosa model =
  print_endline "0) ponastavi avtomat";
  print_endline "1) dodaj vhodne simbole";
  print_endline "2) dodaj skladovne simbole";
  print_endline "3) dodaj sprejemno stanje";
  print_endline "4) dodaj nesprejemno stanje";
  print_endline "5) dodaj prehod";
  print_string "> ";
  match read_line () with
  | "0" -> ponastavi_avtomat ()
  | "1" -> vnesi_simbole dodaj_vhodni_simbol model
  | "2" -> vnesi_simbole dodaj_skladovni_simbol model
  | "3" -> vnesi_stanje dodaj_sprejemno_stanje model
  | "4" -> vnesi_stanje dodaj_nesprejemno_stanje model
  | "5" ->
