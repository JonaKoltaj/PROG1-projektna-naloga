open Avtomat

type stanje_vmesnika =
  | SeznamMoznosti
  | IzpisAvtomata
  | VnosAvtomata
  | BranjeNiza
  | NalozitevAvtomata
  | ShranitevAvtomata

(* Model vsebuje podatke o avtomatu in stanju vmesnika. *)
type model = {
    avtomat : avtomat;
    stanje_vmesnika : stanje_vmesnika;
  }

type msg =
  | PreberiNiz of string
  | ZamenjajVmesnik of stanje_vmesnika
  | ZamenjajAvtomat of avtomat
  | IzpisiAvtomat of avtomat
