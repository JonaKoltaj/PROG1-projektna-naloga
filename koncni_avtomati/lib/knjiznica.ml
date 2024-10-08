open Avtomat
open Zagon
open Tipi
open Izpis_avtomata

(* Datoteka pomožnih funkcij za shranjevanje in nalaganje avtomata, uporabljenih v tekstovnem vmesniku. *)

(* Avtomat sprejema nize oblike 0^n1^n in zavrača vse druge binarne nize. *)
let nicle_in_enice =
  prazen_avtomat "p" 'Z' 'E'
  |> dodaj_sprejemno_stanje "r"
  |> dodaj_stanje "q"
  |> dodaj_vhodni_simbol '0'
  |> dodaj_vhodni_simbol '1'
  |> dodaj_skladovni_simbol 'A'
  |> dodaj_prehod "p" (Crka '0') (Crka 'Z') "p" ['A'; 'Z']
  |> dodaj_prehod "p" (Crka '0') (Crka 'A') "p" ['A'; 'A']
  |> dodaj_prehod "p" (Eps ()  ) (Crka 'Z') "q" ['Z']
  |> dodaj_prehod "p" (Eps ()  ) (Crka 'A') "q" ['A']
  |> dodaj_prehod "q" (Crka '1') (Crka 'A') "q" []
  |> dodaj_prehod "q" (Eps ()  ) (Crka 'Z') "r" ['Z']

(* Sledeče funkcije so namenjene nalaganju avtomata iz datoteke v mapi knjznica-avtomatov v trenutni model. *)

(* Funkcija sprejme tabelo imen datotek v mapi knjiznica-avtomatov in uporaniku prepusti izbiro, vrne pa niz imena izbrane datoteke. *)
let rec izberi_datoteko array_avtomatov =
  let n = Array.length array_avtomatov in
  print_endline ("Vnesi število od 0 do " ^ Int.to_string (n - 1));
  for i=0 to (n-1) do
    print_int i;
    print_string ") ";
    print_endline array_avtomatov.(i)
  done;
  print_string "> ";
  let vnos = read_line () in
  try
    let j = int_of_string vnos in
    array_avtomatov.(j)
  with
  | _ -> print_endline ("Napačen vnos, vnesi število od 0 do " ^ Int.to_string (n - 1));
         izberi_datoteko array_avtomatov


(* Pomozna funkcija, ki vzame string oblike "a b c" in ga spremeni v seznam simbolov [a,b,c] *)
let string_to_list_of_char str =
  let list = explode str in
  let f a = if a == ' ' then None else Some a in
  List.filter_map f list

let string_to_list str = String.split_on_char ' ' str

(* Sprejme string oblike (a,b,c,d,e) in doda prehod (torej vrne avtomat) *)
let string_to_prehod str avtomat =
  let rg = Str.regexp "(\\(.+?\\),\\(.\\),\\(.\\),\\(.+?\\),\\(.+?\\))" in
  (* Regex je treba najprej pognati, da lahko gledamo matched groups, ta if ne služi ničemer drugem. *)
  if Str.string_match rg str 0 then
    let stanje1 = Str.matched_group 1 str in
    let vhodni_simbol = Str.matched_group 2 str in
    let vhodni_simbol' = if vhodni_simbol.[0] = avtomat.prazni_simbol then Eps () else Crka vhodni_simbol.[0] in
    let skladovni_simbol = Str.matched_group 3 str in
    let skladovni_simbol' = if skladovni_simbol.[0] = avtomat.prazni_simbol then Eps () else Crka skladovni_simbol.[0] in
    let stanje2 = Str.matched_group 4 str in
    let skladovni_niz = Str.matched_group 5 str in
    let skladovni_niz' = if skladovni_niz.[0] = avtomat.prazni_simbol then [] else explode skladovni_niz in
    dodaj_prehod stanje1 vhodni_simbol' skladovni_simbol' stanje2 skladovni_niz' avtomat
  else avtomat

(* Pomožna funkcija, ki prebere datoteko in vrne seznam vseh vrstic. *)
let read_lines file =
  let ic = open_in file in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec aux acc = match try_read () with
    | Some s -> aux (s :: acc)
    | None -> close_in ic; List.rev acc in
  aux []

(* Preberemo datoteko, jo spremenimo v tabelo dolžine 8 (to bo vedno res, saj je avtomat tako definiran) *)
(* Postopoma dodamo vse komponente v avtomat. *)
let preberi_datoteko str = 
  (* Vrstice bodo vedno dolzine 8 *)
  let vrstice = Array.of_list (read_lines str) in
  let stanja = string_to_list vrstice.(0) in
  let vhodna_abeceda = string_to_list_of_char vrstice.(1) in
  let skladovna_abeceda = string_to_list_of_char vrstice.(2) in
  let zacetno_stanje = vrstice.(3) in
  let zacetni_skladovni_simbol = (vrstice.(4)).[0] in
  let prazni_simbol = (vrstice.(5)).[0] in
  let sprejemna_stanja = string_to_list vrstice.(6) in
  let prehodi = string_to_list vrstice.(7) in (* niso se parsed, so sam usak posebi svoj string*)
  let rec dodaj f list acc = match list with
    | [] -> acc
    | x :: xs -> dodaj f xs (f x acc)
  in
  prazen_avtomat zacetno_stanje zacetni_skladovni_simbol prazni_simbol
  |> dodaj dodaj_sprejemno_stanje sprejemna_stanja
  |> dodaj dodaj_vhodni_simbol vhodna_abeceda
  |> dodaj dodaj_skladovni_simbol skladovna_abeceda
  |> dodaj dodaj_stanje stanja
  |> dodaj string_to_prehod prehodi

let nalozi_avtomat () =
  let array_avtomatov = Sys.readdir "./knjiznica-avtomatov" in
  let datoteka = izberi_datoteko array_avtomatov in
  IzpisiAvtomat (preberi_datoteko ("./knjiznica-avtomatov/" ^ datoteka))



(* Sledeče funkcije so namenjene shranjevanju avtomata, ki je v trenutnem modelu, v datoteko. *)

let prehod_to_str avtomat (st1,vs,ss,st2,sn) =
  "(" ^ st1 ^ "," ^ (crka_ali_eps_to_str avtomat vs) ^ "," ^ (crka_ali_eps_to_str avtomat ss) ^ "," ^ st2 ^ "," ^ (skladovni_niz_v_ta_pravi_niz avtomat sn) ^ ")"

(* Funkcija, ki spremeni avtomat v niz, primeren za zapis v datoteko. *)
(* Ta niz se sklada s tem, na kakšen način datoteke beremo. *)
let avtomat_to_content avtomat =
  let stanja_izpis = (String.concat " " avtomat.stanja) ^ "\n" in 
  let vhodna_abeceda_izpis = (String.concat " " (List.map (fun a -> String.make 1 a) avtomat.vhodna_abeceda)) ^ "\n" in
  let skladovna_abeceda_izpis = (String.concat " " (List.map (fun a -> String.make 1 a) avtomat.skladovna_abeceda)) ^ "\n" in
  let sprejemna_stanja_izpis = (String.concat " " avtomat.sprejemna_stanja) ^ "\n" in
  let prehodi_izpis = (String.concat " " (List.map (prehod_to_str avtomat) avtomat.prehodna_relacija)) in
  let prvi_del = stanja_izpis ^ vhodna_abeceda_izpis ^ skladovna_abeceda_izpis ^ avtomat.zacetno_stanje ^ "\n" in
  let drugi_del = (String.make 1 avtomat.zacetni_skladovni_simbol) ^ "\n" ^ (String.make 1 avtomat.prazni_simbol) ^ "\n" in
  let tretji_del = sprejemna_stanja_izpis ^ prehodi_izpis in
  prvi_del ^ drugi_del ^ tretji_del

(* Ko uporabnik shranjuje avtomat, vpiše ime danega avtomata. *)
(* Če datoteka s tem imenom že obstaja, se avtomat izpiše tja, drugače datoteko ustvari. *)
let shrani_avtomat avtomat =
  print_endline "Vnesi ime avtomata";
  print_string "> ";
  let ime = "./knjiznica-avtomatov/" ^ read_line () in
  let content = avtomat_to_content avtomat in
  Out_channel.with_open_text ime (fun oc -> Out_channel.output_string oc content)
