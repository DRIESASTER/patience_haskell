# patience_haskell
patience game written in  haskell


--explanation of my code :
-- mijn coordinatensysteem werkt als volgt, y=0 is de bovenste rij, x=0 is de meest linkse kolom, daarbuiten houd ik indexen bij van waar ik op de stack zit, dus selected index van selector is de index van de hoeveelste kaart op de stack geselecteerd is, en cardindex is de hoeveelste kaart op de stack waar de selector op zit
-- m verplaatst als mogelijk de geselecteerde stack/kaart naar de plaats waar de selector momenteel staat
-- r roteert door de pile per "rotationAmount" kaarten
-- s gooit de bovenste kaart van de pile op de eindstapel als dat mogelijk is
-- enter selecteert een stack/kaart
-- spatie gooit als mogelijk de geselecteerde kaart op de eindstapel
-- d neemt de bovenste kaart van de pile en legt deze onder de geselecteerde kaart als dat mogelijk is
-- pijltjes links en rechts bewgen vanzelfspreken naar links en rechts, pijltjes omhoog en omlaag bewegen naar boven en naar beneden in de stacks
