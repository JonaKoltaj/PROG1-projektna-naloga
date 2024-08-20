open Avtomat

type stanje_vmesnika =
  | SeznamMoznosti
  | IzpisAvtomata
  | VnosAvtomata
  | BranjeNiza

(* Model vsebuje podatke o avtomatu, koncnem stanju avtomata, skladu in stanju vmesnika. *)
type model = {
    avtomat : avtomat;
    stanje_vmesnika : stanje_vmesnika;
  }

type msg =
  | PreberiNiz of string
  | ZamenjajVmesnik of stanje_vmesnika
  | ZamenjajAvtomat of avtomat
  | IzpisiAvtomat of avtomat
