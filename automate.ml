random__init ( int_of_float (sys__time()) );;

(* Tri par fusion de societe en fonction de la position dans l'environnement / Vérifiée *);;
let fusion t d m f =
	let res = make_vect (vect_length t) (t.(0)) and i =ref d and j = ref (m+1) and k= ref d in begin
		while !i<= m && !j<=f do
			let (x,y,z) = t.(!i) and (x',y',z') = t.(!j) in begin
				if x < x' then begin res.(!k) <- t.(!i) ; i:=!i+1 ; k:= !k+1 end
				else begin res.(!k) <- t.(!j) ; j:=!j+1 ; k:= !k+1 end;
			end;
		done;
		if !i <= m then while !i <= m do res.(!k) <- t.(!i) ; i:=!i+1 ; k:=!k+1 done
		else while !j <= f do res.(!k) <- t.(!j) ; j:=!j+1 ; k:=!k+1 done ;
		for p = d to f do t.(p) <- res.(p) done;
	end;;

let rec trier_aux t d f =
	if d<f then let m = (d+f)/2 in begin
		trier_aux t d m ; trier_aux t (m+1) f ; fusion t d m f;
	end;;

let trier t =
	(trier_aux t 0 (vect_length(t)-1));;

(* Fonction qui crée une société aléatoire a l'aide des arguments / Vérifiée *);;

let creer_societe population nombre_contamines nombre_vaccines taille_environnement =
	let societe = make_vect population (-1,false,false) and taille = taille_environnement*taille_environnement in begin
		random__init ( int_of_float (sys__time()) );
		for i = 0 to population -1 do
			let pos = (random__int(taille)) in
				societe.(i) <- (pos,false,false)
		done;
		if nombre_contamines<=population then begin
			for i = 0 to nombre_contamines-1 do
				let (x,y,z) = societe.(i) in societe.(i) <- (x,true,false)
			done;
			if nombre_vaccines+nombre_contamines<=population then
				for i=nombre_contamines to nombre_contamines+nombre_vaccines-1 do
					let(x,y,z) = societe.(i) in societe.(i) <- (x,false,true)
				done
			else
				for i=nombre_contamines to population-1 do
					let(x,y,z) = societe.(i) in societe.(i) <- (x,false,true)
				done;
			end
		else
			for i = 0 to population-1 do
				let(x,y,z) = societe.(i) in societe.(i) <- (x,true,false)
			done;
		trier societe;
		societe;
	end;;

let concatene_societes(tab_societes,taille_commune_environnement) =
	let n = (int_of_float(sqrt(float_of_int(vect_length(tab_societes))))) and pop_totale = ref 0 in
	let populations = make_vect (n*n) 0 in begin
		for i=0 to n*n-1 do
			pop_totale:=!pop_totale+(vect_length tab_societes.(i));
			populations.(i) <- (vect_length tab_societes.(i))
		done;
		let societe = make_vect !pop_totale (-1,false,false) and u = ref 0 in begin
			for i=0 to n-1 do
				for j=0 to n-1 do
					let m = vect_length tab_societes.(n*i+j) in
					for k = 0 to m-1 do
						let (x,y,z) = tab_societes.(n*i+j).(k) in
						let colonne = (x mod taille_commune_environnement)+j*taille_commune_environnement
						and ligne = (x / taille_commune_environnement)+i*taille_commune_environnement in begin
							societe.(!u) <- ( colonne+n*taille_commune_environnement*ligne ,y,z);
							u:=!u+1
						end;
					done;
				done;
			done;
			trier societe;
			societe;
		end;
	end;;

let ville() =
	let tab_societes = [|[||];[||];[||];[||];[||];[||];[||];[||];[||]|] in begin
		for i = 0 to 3 do
			tab_societes.(i) <- creer_societe 400 0 0 33
		done;
		tab_societes.(4) <- creer_societe 2000 1 0 33;
		for i = 5 to 8 do
			tab_societes.(i) <- creer_societe 400 0 0 33
		done;
		concatene_societes(tab_societes,33)
	end;;
let villes() =
	let  tab_societes = [|[||];[||];[||];[||]|] in begin
		tab_societes.(0)<-creer_societe 4900 0 0 50;
		tab_societes.(1)<-creer_societe 100 0 0 50;
		tab_societes.(2)<-creer_societe 100 0 0 50;
		tab_societes.(3)<-creer_societe 4900 1 0 50;
		let societe = concatene_societes(tab_societes,50) in
		let n=vect_length(societe) in begin
			for i = 0 to 99 do
				societe.(i)<- (4900+i,false,false);
				societe.(n-i-1) <- (5000+i,false,false);
			done;
			trier societe
		end;
		societe;
	end;;
vect_length(villes());;
vect_length(creer_societe 10000 1 0 100);;

afficher_societe(villes(),100);;

let societe1 = creer_societe 4900 1 0 50;;
let societe2 = creer_societe 100 0 0 50;;
let societe3 = creer_societe 100 0 0 50;;
let societe4 = creer_societe 4900 0 0 50;;
let societe = concatene_societes([|societe1;societe2;societe3;societe4|],50);;
societe;;
societe.(198);;
creer_societe 200 40 40 20;;
afficher_societe((creer_societe 200 40 40 20),20);;
vect_length(societe);;
afficher_societe(societe,100);;



let compte_sains_malades_vaccines(societe) =
	let n=vect_length societe and s=ref 0 and i=ref 0 and r=ref 0 in begin
		for k = 0 to n-1 do
			let(x,y,z) = societe.(k) in
				if y then i:=!i+1
				else  if z then
						r:=!r+1
						else
							s:=!s+1
		done;
		(!s,!i,!r);
	end;;


(* Affichage à l'écran de la société dans son environnement.
Les points rouges sont les malades, les verts les sains *);;

#open "graphics";;
open_graph "1200x1200+0-0";;
let afficher_societe (societe, taille_environnement) =
	open_graph (string_of_int(6*taille_environnement)^"x"^string_of_int(6*taille_environnement)^"+0-0");
	set_window_title "Epidemie";
	let n =vect_length(societe) in begin
		for i = 0 to n-1 do
			let (x,y,z) = societe.(i) in
				let (x',y') = (x mod taille_environnement,x / taille_environnement) in begin
					if y then set_color red
					else	if z then  set_color blue
							else set_color green;
					fill_circle (6*x'+4) (6*y'+4) 4;
			end;
		done;
		wait_next_event [Button_down];
		clear_graph();
	end;;

let societe = creer_societe 10000 1 5 100;;
afficher_societe (societe, 100) ;;

(* Programme de recherche d'un individu dans l'environnement à la position élement / Vérifiée *);;

let rec dichot_aux (element, societe, i, j) =
	if i = j then let(x,y,z)=societe.(i) in
		if element = x	then true
							else false;
	else let(x,y,z)=societe.((i+j)/2) in
				if x<element 	then dichot_aux (element, societe, ((i+j)/2)+1, j)
									else dichot_aux (element, societe, i, ((i+j)/2));;
let dichot (element,societe,taille) =
	if element<0 or element>(taille*taille-1) then false
	else let n = vect_length societe in
		dichot_aux(element,societe,0,n);;
societe;;
dichot(250,societe,100);;

(* Fonctions de base de la structure de pile / Vérifiées *);;

let push element pile =
	pile := element::(!pile);;
let pop pile =
	let tete = hd !pile in begin
		pile := tl !pile;
		tete
	end;;

(* Renvoie le n-ième élément d'une liste / Vérifiée *);;

let rec nieme liste n =
	if n=0 then hd(liste)
	else nieme (tl(liste)) n-1;;

(* Renvoie la liste des cases libres autour de l'individu en position indiv / Vérifiée *);;

let cases_libres (i, taille_environnement) =
	let possibilites = [|i-taille_environnement;i-1;i;i+1;i+taille_environnement|] and compteur = ref 0 in begin
		for j = 0 to 4 do
			if possibilites.(j)>=0 or possibilites.(j)<taille_environnement then
				compteur:=!compteur+1;
		done;
		let resultat = make_vect !compteur 0 and position = ref 0 in begin
			for j = 0 to 4 do
				if possibilites.(j)>=0 or possibilites.(j)<taille_environnement then begin
					resultat.(!position) <- possibilites.(j);
					position := !position +1
				end;
			done;
			(resultat,!compteur);
		end;
	end;;
let case_de_deplacement (i, taille_environnement) =
	let cases,nb_possibilites = cases_libres (i, taille_environnement) in
		cases.(random__int(nb_possibilites));;


let deplacement (societe, taille_environnement, population) =
	let societe_aux = make_vect population (-1,false,false) in begin
		for i = 0 to population-1 do
			let (indiv,y,z) = societe.(i) in
			let case = case_de_deplacement (indiv, taille_environnement) in
				societe_aux.(i)<-(case,y,z);
		done;
		societe_aux
	end;;
afficher_societe(!societe,100);;
societe:=deplacement(!societe, 100, 10000);;

(* Autour de la contamination *);;

let rec y_a_t_il_un_true (position,element,societe) =
	let n=vect_length societe in
	if position=n then false
	else	let (x,y,z)=societe.(position) in if x<>element then false
			else 	if y then true
					else y_a_t_il_un_true (position+1,element,societe);;

let rec premier (position, element, societe)=
	match position with
			0 -> let (x,y,z) = societe.(position) in	if x=element then 0
																	else 1
		|	_ -> let (x,y,z) = societe.(position) in	if x=element then premier(position-1, element, societe)
																	else position+1;;

let rec dichot2_aux (element, societe, i, j) =
	if i = j then let(x,y,z)=societe.(i) in
		if element = x	then y_a_t_il_un_true(premier(i,element,societe),element,societe)
							else false;
	else let(x,y,z)=societe.((i+j)/2) in
				if x<element 	then dichot2_aux (element, societe, ((i+j)/2)+1, j)
									else dichot2_aux (element, societe, i, ((i+j)/2));;


let dichot2 (element,societe,taille) =
	if element<0 or element>((taille*taille)-1) then false
	else let n = vect_length societe in
		dichot2_aux(element,societe,0,n);;

(* Retourne le nombre de contaminés dans les quatre cases autour de la case indiv / Vérifiée *);;

let contamines (indiv, societe, taille_environnement) =
	let res = ref 0 in begin
		try (if dichot2(indiv+1, societe, taille_environnement) && (indiv+1) mod taille_environnement<>0
			then res := !res+1;!res;
		if dichot2(indiv+1, societe, taille_environnement) && (indiv+1) mod taille_environnement<>0
			then res := !res+1;!res;
		if dichot2(indiv-1, societe, taille_environnement) && indiv mod taille_environnement<>0
			then res := !res+1;!res;
		if dichot2(indiv+taille_environnement, societe, taille_environnement)
			then res := !res+1;!res;
		if dichot2(indiv-taille_environnement, societe, taille_environnement)
			then res := !res+1;!res;
		if dichot2(indiv, societe, taille_environnement)
			then res:=!res+1;!res) with
				Invalid_argument "vect_item" -> !res;
				| _ -> !res;
		end;;

let petite_societe = ref [| (0,false);(2,true);(3,false);(4,false);(4,false);(4,true);(4,false);(4,false);(6,false);(8,false) |];;
dichot2(3-3,petite_societe,3);;
dichot2(3+1,petite_societe,3);;
contamines (3, !petite_societe, 3);;
dichot2(3, petite_societe, 3);;

(* Effectue la contamination de ceux qui sont au contact de malades *);;

let contamination (societe, taille_environnement, population, proba_contamination) =
	for i = 0 to population-1 do
		let (indiv, y, z) = societe.(i) in begin
			if not y && not z then
				let n = contamines (indiv, societe, taille_environnement) in
					for j = 0 to n-1 do
						let malade = random__int(1000) in
							if float_of_int(malade) < 1000.*.proba_contamination
								then societe.(i) <- (indiv,true,false)
					done;
		end;
	done;
	societe;;

contamination (villes(), 100, 10000, 1.);;

let societe = ref (creer_societe 10000 1 0 100);;
let societe = ref (villes());;
!societe.(0)<-(0,true);;
!societe;;
afficher_societe(!societe,100);;
contamination(!societe,100,10000,1.),100;;

afficher_societe(!petite_societe,3);;
petite_societe:=contamination(!petite_societe,3,6,1.);;
societe:=contamination(!societe,100,1000,1.);;

float_of_int(random__int(1000))<1000.*.1.0;;
let petite_societe = ref [| (0,false);(2,true);(3,false);(4,false);(6,true);(7,false);(8,false);(9,true) |];;
petite_societe:=contamination( !petite_societe, 3, 8, 1. );;
petite_societe;;
let societe = creer_societe 10000 5000 100;;
contamination( societe,100, 10000,1.);;
let new_societe = creer_societe 5000 0 100;;
new_societe.(5)<-(8,true);;
new_societe;;
contamination (new_societe, 100, 5000, 1.);;

(* Tue les malades, ou les guérit avec une certaine proba / Vérifiée*);;
let compte_morts (societe) =
	let n = vect_length societe and res = ref 0 in begin
		for i = 0 to n-1 do
			let (x,y,z) = societe.(i) in if x= -1 then res:= !res+1
		done; 
		!res;
	end;;

let compresse (societe,n) =
	let m = vect_length societe in
		let res = make_vect (m-n) (-1,false,false) and compteur = ref 0 in begin
			for i = 0 to m-1 do
				let (x,y,z) = societe.(i) in
					if x <> -1 then begin
						res.(!compteur) <- (x,y,z);
						compteur := !compteur+1
					end;
			done;
			res;
		end;;

let mort (societe, population, proba_mort, proba_guerison) =
	for i = 0 to population-1 do
		let (indiv, y, z) = societe.(i) in begin
			if y then begin
				let deces = random__int(1000) in
					if float_of_int(deces) < 1000.*.proba_mort
								then societe.(i) <- (-1,false,false);
				let gueri = random__int(1000) in
					if float_of_int(gueri) < 1000.*.proba_guerison
								then societe.(i) <- (indiv,false,true)
			end;
		end;
	done;
	let n = compte_morts (societe) in
	(compresse(societe,n),population-n);;

(* Autour de la vaccination *)

let vaccination (societe, population, proba_vaccin, proba_retour) =
	for i = 0 to population-1 do
		let (indiv, y, z) = societe.(i) in
			if not y then
				if z then
					let retour = random__int(1000) in
						if float_of_int(retour)<1000.*.proba_retour
							then societe.(i) <- (indiv,false,false);
				else
					let vaccin = random__int(1000) in
						if float_of_int(vaccin)<1000.*.proba_vaccin
							then societe.(i) <- (indiv,false,true);
	done;
	societe;;


(* Premiers tests / Non fonctionnel *)

let test (taille_environnement, population, nombre_contamines, nombre_vaccines, proba_contamination, proba_mort, proba_guerison,proba_vaccin,proba_retour,n) =
	let societe = ref (creer_societe population nombre_contamines nombre_vaccines taille_environnement) and popu = ref population
	and P=make_vect n 0 and S=make_vect n 0 and I = make_vect n 0 and R = make_vect n 0
	in begin
		for k = 0 to n-1 do
			(*if k mod 20 = 0 then  afficher_societe(!societe,taille_environnement);*)
			let (s,i,r) = compte_sains_malades_vaccines(!societe) in begin
				P.(k) <- s+i+r;
				S.(k) <- s;
				I.(k) <- i;
				R.(k) <- r;
			end;
			societe := contamination (!societe, taille_environnement, !popu, proba_contamination);
			(*societe := deplacement (!societe, taille_environnement, !popu);*)
			let (x,y) = mort (!societe, !popu, proba_mort, proba_guerison) in begin
				societe := x;
				popu := y
			end;
			societe:=vaccination(!societe, population, proba_vaccin, proba_retour);
		done;
		(P,S,I,R);
	end;;
let draw(P,S,I,R,n)=
	open_graph (string_of_int(2*n)^"x501+0-0");
	set_window_title "Courbes";
	set_color black;
	for k=0 to n-1 do
		plot (2*k) (P.(k)/20)
	done;
	set_color green;
	for k=0 to n-1 do
		fill_circle (2*k) (S.(k)/20) 2
	done;
	set_color red;
	for k=0 to n-1 do
		fill_circle (2*k) (I.(k)/20) 2
	done;
	set_color blue;
	for k=0 to n-1 do
		fill_circle (2*k) (R.(k)/20) 2
	done;
	wait_next_event [Button_down];
	clear_graph();;

let petite_societe = [| (0,true);(2,false);(3,true);(6,true);(8,true) |];;
contamination(petite_societe,3,5,1.0);;
mort(petite_societe,5,1.0,0.0);;
test(100, 10000, 1,0, 0.2, 0., 0.05, 0., 0.,200);;
let P,S,I,R=test (100, 10000, 1,0, 1., 0., 0.02, 0., 0.,200);;
draw(P,S,I,R,200);;
let P,S,I,R=test (100, 10000, 1,0, 1., 0., 0.05, 0., 0.,200);;
draw(P,S,I,R,200);;
moyenne(100, 5000, 2500, 1., 0.5, 0.9,10);;

let tableau_dimension_5 (a,b,c,d,e,valeur) =
 let result = make_vect a (make_vect b (make_vect c (make_vect d (make_vect e valeur)))) in begin
 	for i = 1 to a - 1 do
  		for j = 0 to b - 1 do
  			for k = 0 to c - 1 do
  				for l = 0 to d - 1 do
  					result.(i).(j).(k).(l) <- make_vect e valeur
  				done;
  			done;
  		done;
  	done;
  	result;
  end;;

let moyenne(taille_environnement, population, nombre_contamines, proba_contamination, proba_mort, proba_guerison,nbre_essais) =
	let res = ref 0 and compteur = ref 0 in begin
		for i = 1 to nbre_essais do
			let nouv = test(taille_environnement, population, nombre_contamines, proba_contamination, proba_mort, proba_guerison) in
				if nouv = population
					then compteur := !compteur + 1
				else
					res := !res + nouv
		done;
		res:= !res/(nbre_essais-(!compteur));
		!res
	end;;

let epidemie (taille_environnement) =
	let t = taille_environnement-(taille_environnement/4) in begin
		let resultat = tableau_dimension_5 (t,t,21,21,21,-1) in
			for i=0 to (t)-1 do
				for j=0 to (t)-1 do
					for k=0 to 20 do
						for l=0 to 20 do
							for m = 0 to 20 do
								let	population = i and
										nombre_contamines = j and
										proba_contamination = float_of_int(k)/.20. and
										proba_mort = float_of_int(l)/.20. and
										proba_guerison = float_of_int(m)/.20. in
								resultat.(i).(j).(k).(l).(m) <- moyenne(taille_environnement, population, nombre_contamines, proba_contamination, proba_mort, proba_guerison,100)
							done;
						done;
					done;
				done;
			done;
			resultat;
	end;;

epidemie(5);;
