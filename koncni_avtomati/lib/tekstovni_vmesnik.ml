open Avtomat
open Zagon
open Tipi
open Vnos_avtomata
open Izpis_avtomata
open Knjiznica

(* Datoteka, ki požene celotni tekstovni vmesnik. *)

(* Funkcija prejme model in msg, vrne pa posodobljen model. *)
let update model = function
  | PreberiNiz niz -> (
    match pozeni model.avtomat niz with
    | true -> (
      print_endline "Niz je bil sprejet. Uspešne veje so označene z zvezdico.";
      { model with stanje_vmesnika = SeznamMoznosti }
    )
    | false -> (
      print_endline "Niz ni bil sprejet v nobeni od vej.";
      { model with stanje_vmesnika = SeznamMoznosti }
    )
  )
  | ZamenjajVmesnik stanje_vmesnika -> { model with stanje_vmesnika }
  | ZamenjajAvtomat avtomat -> izpisi_avtomat avtomat; { model with avtomat }
  | IzpisiAvtomat avtomat -> izpisi_avtomat avtomat; { avtomat; stanje_vmesnika = SeznamMoznosti }
  | ShraniAvtomat avtomat -> shrani_avtomat avtomat; { model with stanje_vmesnika = SeznamMoznosti }

(* Tekstovni vmesnik nam ponudi moznost trenuten avtomat spremeniti, ga izpisati, skozi avtomat pognati niz ali pa avtomat naložiti. *)
let rec izpisi_moznosti () =
  print_endline "Vnesi stevilo od 0 do 3";
  print_endline "0) spremeni avtomat";
  print_endline "1) izpiši avtomat";
  print_endline "2) beri niz";
  print_endline "3) naloži avtomat";
  print_string "> ";
  match read_line () with
  | "0" -> ZamenjajVmesnik VnosAvtomata
  | "1" -> ZamenjajVmesnik IzpisAvtomata
  | "2" -> ZamenjajVmesnik BranjeNiza
  | "3" -> ZamenjajVmesnik NalozitevAvtomata
  | _ ->
     print_endline "Napačen vnos, vnesi število od 0 do 3";
     izpisi_moznosti ()

let rec beri_niz avtomat =
  print_endline "Vnesi niz";
  print_string "> ";
  let str = read_line () in
  let preveri_simbole =
    List.fold_left
      (fun acc x -> acc && (List.mem x avtomat.vhodna_abeceda))
      true
      (explode str)
  in
  if not preveri_simbole then
    (
      print_endline "Niz vsebuje simbol zunaj vhodne abecede";
      beri_niz avtomat
    )
  else
    PreberiNiz str

(* Funkcija glede na stanje vmesnika požene funkcijo, ki vrne msg. *)
let view model =
  match model.stanje_vmesnika with
  | SeznamMoznosti -> izpisi_moznosti ()
  | IzpisAvtomata -> IzpisiAvtomat model.avtomat
  | VnosAvtomata -> izpisi_moznosti_vnosa model.avtomat
  | BranjeNiza -> beri_niz model.avtomat
  | NalozitevAvtomata -> nalozi_avtomat ()

let rec loop model =
  let msg = view model in
  let model' = update model msg in
  loop model'
