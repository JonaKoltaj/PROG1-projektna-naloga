open Avtomat
open Ide_gas
open Tipi
open Vnos_avtomata

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

(* Tekstoni vmesnik nam ponudi moznost trenuten avtomat spremeniti, ga izpisati, skozi avtomat pognati niz in ponastaviti stanje trenutnega avtomata. *)
(* TO DO Shrani avtomat, uploadaj avtomat *)
let rec izpisi_moznosti () =
  print_endline "0) spremeni avtomat";
  print_endline "1) izpiši avtomat";
  print_endline "2) beri niz";
  print_string "> ";
  match read_line () with
  | "0" -> ZamenjajVmesnik VnosAvtomata
  | "1" -> ZamenjajVmesnik IzpisAvtomata
  | "2" -> ZamenjajVmesnik BranjeNiza
  | _ ->
     print_endline "Napačen vnos, vnesi število od 0 do 2";
     izpisi_moznosti ()

let view model =
  match model.stanje_vmesnika with
  | SeznamMoznosti -> izpisi_moznosti ()
  | IzpisAvtomata -> _
  | VnosAvtomata -> izpisi_moznosti_vnosa ()
  | BranjeNiza
