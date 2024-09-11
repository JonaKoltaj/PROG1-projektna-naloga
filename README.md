# PROG1 Projektna Naloga

Moj projekt vsebuje implementacijo končnega avtomata z skladom, ki je nadgradnja običajnega končnega avtomata ter nekaj primerov pri katerih je končni avtomat s skladom malo boljši od osnovnega avtomata, kot so naprimer palindromi ter iskanja zaporedov v nizih.

## Matematična definicijaje

Končni avtomat s skladom je definiran kot $M=(Q, \Sigma, \Gamma, \delta, q_{0}, Z, F)$
- $Q$ je končna množica stanj
- $\Sigma$ je končna množica, ki predstavlja vhodno abecedo
- $\Gamma$ je končna množica, ki predstavlja skladovno abecedo
- $\delta$ je podmnožica $Q \times (\Sigma \cup \{\varepsilon\}) \times \Gamma \times Q \times \Gamma*$ in je prehodna relacija
- $q_{0} \in Q$ je začetno stanje
- $Z \in \Sigma$ je začetni skladovni simbol
- $F \subseteq Q$ je množica sprejemnih stanj

Tu $\Gamma*$ predstavlja množico končnih nizov, sestavljenih iz simbolov $\Gamma$, $\varepsilon$ pa predstavlja prazni niz.

Element relacije $\delta$ je prehod, sestavljen iz trojice $\(p,a,A\) \subseteq Q \times (\Sigma \cup \{\varepsilon\}) \times \Gamma$ in para $\(q,\alpha\) \subseteq Q \times \Gamma*$, ki v stanju $p$ prebere $a$, v primeru, da je na vrhu sklada $A$, ter se premakne v stanje $q$, iz sklada vzame $A$ in nanj potisne niz $\alpha$.
Ker je $a \in \Sigma \cup \{\varepsilon\}$, to pomeni, da se lahko odločimo, ali bomo prebrali naslednji simbol v nizu, ali pa bomo na tem koraku niz pustili pri miru.

## Implementacija

V datoteki Avtomat definiramo avtomat podobno kot je definiran matematično, v datoteki Zagon pa je definiran nemudni opis, ki ga sestavljajo trenutno stanje, trenutni sklad in pa preostanek niza, ki ga beremo. Najprej je definirana funkcija en_korak, ki preveri, ali je podani prehod možen in primerno spremeni nemudni opis. Ker je avtomat s skladom nedeterminističen (na vsakem koraku niz lahko ali beremo ali ne), se funkcija, ki prebere celoten niz, spusti po vseh možnostih. Že če ena izmed vej sprejme dani niz, se niz šteje kot sprejet.

Implementacija tekstovnega vmesnika je v osnovi podobna implementaciji na https://github.com/matijapretnar/programiranje-1/tree/master/projekt.

## Navodila za uporabo

Avtomat se požene z primerom Ničle in Enice, ki sprejme nize oblike 0^n1^n.
Najprej se izpiše seznam možnosti, kjer lahko potem uporabnik avtomat tudi spremeni in/ali vnese svoj avtomat. Tega lahko tudi shrani v datoteko v mapi knjiznica\_avtomatov, iz katere potem lahko avtomate tudi nalozi. Možnost beri\_niz avtomat požene na danem nizu, možnost izpisi\_avtomat pa avtomat izpiše.
