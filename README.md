# PROG1 Projektna Naloga

Moj projekt vsebuje implementacijo končnega avtomata z skladom, ki je nadgradnja običajnega končnega avtomata ter nekaj primerov pri katerih je končni avtomat s skladom malo boljši od osnovnega avtomata, kot so naprimer palindromi ter iskanja zaporedov v nizih.

Avtomat se požene z primerom Ničle in Enice, ki sprejme nize oblike 0^n1^n.

## Matematična definicija

Končni avtomat s skladom je definiran kot $M=(Q, \Sigma, \Gamma, \delta, q_{0}, Z, F)$
- $Q$ je končna množica stanj
- $\Sigma$ je končna množica, ki predstavlja vhodno abecedo
- $\Gamma$ je končna množica, ki predstavlja skladovno abecedo
- $\delta$ je podmnožica $Q \times (\sigma \cup {\varepsilon}) \times \Gamma \times Q \times \Gamma$ in je prehodna relacija
- $q_{0} \in Q$ je začetno stanje
- $Z \in \Sigma$ je začetni skladovni simbol
- $F \subseteq Q$ je množica sprejemnih stanj

Element relacije \delta je oblike
