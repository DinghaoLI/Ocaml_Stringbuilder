  (****************** Q1*********************)
(**
 Q1 type concat
 *)
type string_builder = Feuille of int*string | Noeud of int*string_builder*string_builder ;;

(****test****)
let example = Noeud(7,Feuille(1,"G"),Noeud(6,Feuille(3,"ATT"),Noeud(3,Feuille(1,"A"),Feuille(2,"CA"))));;
let cas = Noeud(6,Noeud(3,Feuille(1,"A"),Noeud(2,Feuille(1,"A"),Feuille(1,"A"))),Feuille(3,"ATT"));;

(**
 * @param un string_builder
   @return la longueur de caractère dans string_builder 
   *)
let lg sb = match sb with
  |Noeud(a,_,_)->a
  |Feuille(a,_)->a;;


(**
 * @param 2 string_builder
   @return e le nouveau string_builder par leur concaténation.
   @test:cas;;
- : string_builder =
Noeud (6,
 Noeud (3, Feuille (1, "A"), Noeud (2, Feuille (1, "A"), Feuille (1, "A"))),
 Feuille (3, "ATT"))

# concat cas cas;;
- : string_builder =
Noeud (12,
 Noeud (6,
  Noeud (3, Feuille (1, "A"), Noeud (2, Feuille (1, "A"), Feuille (1, "A"))),
  Feuille (3, "ATT")),
 Noeud (6,
  Noeud (3, Feuille (1, "A"), Noeud (2, Feuille (1, "A"), Feuille (1, "A"))),
  Feuille (3, "ATT")))
   *)
let concat sb1 sb2 = let lg (sb1,sb2)=match (sb1,sb2) with 
  |(Noeud(a,_,_),Noeud(b,_,_))->a+b
  |(Feuille(a,_),Noeud(b,_,_))->a+b
  |(Noeud(a,_,_),Feuille(b,_))->a+b
  |(Feuille(a,_),Feuille(b,_))->a+b
                     in Noeud(lg (sb1,sb2),sb1,sb2);;
  						
(**
 Q1 word 
 *)

(**
 * @param un string
   @return la feuille de string.
   @test : # word "Bonjour";;
- : string_builder = Feuille (7, "Bonjour")
   *)
let word str = Feuille (String.length(str),str);;


  (****************** Q2*********************)

(**
 Q2 
 @param : 'string_builder' qui est une racine pour un mot.
 @return:  la liste des partitions de mot (a l'ordre)    
 *)
let rec getStr = function 
  |Feuille (a,sb)-> [(sb)]
  |Noeud(a,sb1,sb2)-> (getStr sb1)@(getStr sb2);; 

 
(**  
 Q2 
 @param n:l'index pour un caractère d'une chaîne et une racine pour un mot.
 @return un caractère dans la chaîne  selon l'index;   
 @test: 
# cas;;
- : string_builder =
Noeud (6,
 Noeud (3, Feuille (1, "A"), Noeud (2, Feuille (1, "A"), Feuille (1, "A"))),
 Feuille (3, "ATT"))
# char_at 4 cas;;
- : char = 'T' 
 *)
let char_at index sb = if ((index+1) <= (lg sb)) then String.get (String.concat "" (getStr sb)) index
else failwith "L'index est plus grand que la longeur !";;  



  (****************** Q3 *********************)



(**Q3 
   @param  on prend 2 entiers i et m , 1 string_builder .
   @return les caractères dans le string que le string_builder représente (dès i à i+m)  
   @test:
    cas;;
- : string_builder =
Noeud (6,
 Noeud (3, Feuille (1, "A"), Noeud (2, Feuille (1, "A"), Feuille (1, "A"))),
 Feuille (3, "ATT"))
# sub_string2 2 3 cas;;
- : string_builder = Noeud (3, Feuille (1, "A"), Feuille (2, "AT"))
   *)
let rec sub_string2 i m sb = 
  if (0<=i)&&(i<i+m)&&(i+m<=(lg sb))	
  then match sb with
    |Feuille(nb,str)-> word (String.sub str i m)
    |Noeud(nb,sb1,sb2)->if(i>=(lg sb1)) then sub_string2 (i-(lg sb1)) m sb2
    else if(i+m)<=(lg sb1) then sub_string2 i m sb1 
    else concat (sub_string2 i ((lg sb1)-i) sb1) (sub_string2 0 ((i+m)-(lg sb1)) sb2) 
 else
   failwith "input wrong";;






  (****************** Q4*********************)
(**
   Q4 
   @param  on prend un string_builder  
   @return le coût de string_builder 
   @test :
   # cas;;
- : string_builder =
Noeud (6,
 Noeud (3, Feuille (1, "A"), Noeud (2, Feuille (1, "A"), Feuille (1, "A"))),
 Feuille (3, "ATT"))
# cost cas;;
- : int = 11
 *)  
let cost sb =
  let rec costn (n,sb) =  match (n,sb) with 
  |(n,Feuille(nb,_)) -> n*nb
  |(n,Noeud(nb,left,right)) -> (costn (n+1,left)) + (costn (n+1,right)) 
   in     costn (0,sb);;

  (****************** Q5*********************)
(**
 * @param  le code ASCII de chiffre
 * @return  char qui correspond le code ASCII
 * *)
let int_to_char num = 
  if((num >= 0)&&(num<=57)) then 
    Char.chr (num+65)
  else failwith"Input your correct number(0-57) please";;

(**
 * @param  rien
 * @return  produire un chiffre par hasard entre 1~5
 * *)
let randTaille()=(Random.int 5)+1;;  

(**
 * @param  rien
 * @return  produire un chiffre par hasard entre 1~5
 * *)
let randChar()= Char.escaped (int_to_char(Random.int 57));;
(**
 * @param  rien
 * @return  produire un string avec la taille 1～５ par hasard 
 * *)
let randStr() =let rec randCharList n = match n with 
  |0 ->[]  
  |_ -> randChar()::(randCharList (n-1))
 in 		String.concat "" (randCharList (randTaille()));;

(**
 * @param  rien
 * @return   un chiffre par hasard entre 1~２
 * *)
let leftOrRight() = (Random.int 2)+1;; 

(**
 * @param   profondeur i
 * @return  
 * @test 
 *  # random_string (5);;
- : string_builder =
Noeud (19,
 Noeud (18, Feuille (5, "f`eVo"),
  Noeud (13, Feuille (5, "lwkod"),
   Noeud (8, Noeud (3, Feuille (2, "vE"), Feuille (1, "t")),
    Feuille (5, "CVmCj")))),
 Feuille (1, "W"))
 * *)
let rec random_string (i) = let x=leftOrRight() in match i with
  | 0-> word (randStr())
  | n-> if x=1 then concat (random_string (n-1)) (word (randStr()))
  else concat (word (randStr())) (random_string (n-1));;
 
 
   (****************** Q6*********************)
 (**
 Q6 
 @param : 'string_builder' qui est une racine pour un mot.
 @return:  la liste des partitions de mot (a l'ordre)   
 @test:
   cas;;
- : string_builder =
Noeud (6,
 Noeud (3, Feuille (1, "A"), Noeud (2, Feuille (1, "A"), Feuille (1, "A"))),
 Feuille (3, "ATT"))
# list_of_string cas;;
- : string list = ["A"; "A"; "A"; "ATT"] 
 *)
 let rec list_of_string = function 
  |Feuille (a,sb)-> [(sb)]
  |Noeud(a,sb1,sb2)-> (getStr sb1)@(getStr sb2);; 

  (****************** Q7*********************)
  (**
    *@param un string_builder 
    *@return obtenir la list de Feuille
   *)
let rec getFlist = function 
  |Feuille (a,sb)-> [Feuille (a,sb)]
  |Noeud(a,sb1,sb2)-> (getFlist sb1) @ (getFlist sb2);; 
(**
*@param deux string_builder 
*@return calculer le coût après la concaténation 
*)
let cosAfterCon sb1 sb2 =cost (concat sb1 sb2);;

(**
*@param deux string_builder 
*@return calculer le coût  plus faible dans la list (compare par n)
*)
let rec getCostMin1 n list = 
  match list with
  |[]-> 0
  |t::[]-> n
  |t::b::q-> if (n)>(cosAfterCon t b) then getCostMin1 (cosAfterCon t b) (b::q) 
  else getCostMin1 n (b::q) ;;
(**
*@param une liste de feuilles 
*@return obtenir le coût  des deux premier Feuilles
*)
let getCostFirst list = match list with
  |[]->failwith "Wrong input"
  |t::[]->failwith "Wrong input"
  |t::b::q->cosAfterCon t b;;

 (**
*@param une liste de feuilles 
*@return la liste qui a concaténé les deux feuilles successifs dont la concaténation a le coût le plus faible.
*)
let rec concatMin list = let costMin list = getCostMin1 (getCostFirst list) list in
  let min=costMin list in
   match list with
     |[]->failwith "Wrong"
     |t::[]->failwith "Wrong"
     |t::b::q->if (cosAfterCon t b)=min then (concat t b)::q
     else t::(concatMin (b::q));;   
      
  
  (**
*@param un arbre
*@return l'arbre qui a été optimisé par l'algorithme "balance".
 @test 
 # example;;
- : string_builder =
Noeud (7, Feuille (1, "G"),
 Noeud (6, Feuille (3, "ATT"),
  Noeud (3, Feuille (1, "A"), Feuille (2, "CA"))))
# balance example;;
- : string_builder =
Noeud (7, Noeud (4, Feuille (1, "G"), Feuille (3, "ATT")),
 Noeud (3, Feuille (1, "A"), Feuille (2, "CA")))
*)    
let balance ab =let rec balanceL list  = match list with
  |[]->failwith "Wrong input"
  |t::[]->failwith "Wrong input"
  |t::q::[]-> [(concat t q)]
  |t::b::q ->balanceL (concatMin (t::b::q))
 in
   match balanceL (getFlist ab) with
  |[]->failwith "Wrong"
  |q::t->q;;

(******le cas n'est pas optimal pour "balance" *******)
let cas = Noeud(6,Noeud(3,Feuille(1,"A"),Noeud(2,Feuille(1,"A"),Feuille(1,"A"))),Feuille(3,"ATT"));;
cost cas;;
cost (balance cas);;

  (****************** Q8*********************)
(**
*@param rien
*@return produire la profondeur d'arbre entre 1~5;
*)  
let ranDeep()=(Random.int 5)+1;;

(**
*@param rien
*@return produire une arbre aléatoire (profondeur entre 1~5);
*)  
let ranDeepAB() = random_string (ranDeep());;

(**
*@param n est le nombre d'échantillon 
*@return la liste d'échantillon ;
*)   
let rec echantillon (n) = match n with
  |0->failwith "Aucun arbre"
  |1-> let ab = ranDeepAB() in [(cost ab)-(cost (balance ab))]
  |n-> let ab = ranDeepAB() in [(cost ab)-(cost (balance ab))]@(echantillon (n-1));;

  (**
*@param une liste 
*@return l'element maximal dans la list
*)   
let rec max_number l =
    match l with 
    |[] -> failwith"list vide"
    |x::[] -> x
    |x::xs -> max x (max_number xs);;
 (**
*@param une liste 
*@return l'element minimum  dans la list
*)  
let rec min_number l =
    match l with 
    |[] -> failwith"list vide"
    |x::[] -> x
    |x::xs -> min x (max_number xs);;

(**
*@param une liste 
*@return la moyenne d'éléments. 
*)  
let moyenne l = let rec somme (count,sum,l) = match l with 
  |[]->failwith"list vide"
  |t::[]-> (count+1,sum+t,[])
  |t::q -> somme (count+1,sum+t,q) in
  let (count,sum,l)=somme (0,0,l) in float_of_int(sum)/. float_of_int(count);;
  (**
*@param une liste 
*@return la liste triée . 
*)  
let tri l = Sort.list (<) l;;

 (**
*@param (longueur d'une list,list) longueur est pair  
*@return la médiane 
*)  
let rec medianeTriPaire (n,l) = if (List.length l)==((n/2)+1) 
then match l with
  |[]->failwith"list vide"
  |t::[]->failwith"list trop court"
  |t::b::q-> float_of_int(t+b) /. 2.0
else match l with 
  |[]->failwith"list vide"
  |t::[]->failwith"list trop court"
  |t::b::q->medianeTriPaire (n,b::q);;
      (**
*@param (longueur d'une list,list) longueur est impaire   
*@return la médiane 
*)     
let rec medianeTriImpaire (n,l) = if (List.length l)==((n/2)+1) 
then match l with
  |[]->failwith"list vide"
  |t::[]->failwith"list trop court"
  |t::b::q-> float_of_int(t)
else match l with 
  |[]->failwith"list vide"
  |t::[]->failwith"list trop court"
  |t::b::q->medianeTriImpaire (n,b::q);;
      (**
*@param une liste  
*@return la médiane de la liste  
*)  
let mediane l = match l with 
  |[]->failwith"list vide"
  |t::[]-> float_of_int(t)
  |t::b::[] -> float_of_int(t+b) /. 2.0
  |t::b::q-> let lt = tri l in if ((List.length lt) mod 2) == 1
  then medianeTriImpaire ((List.length lt),lt)
  else medianeTriPaire ((List.length lt),lt);;
(**
*@param le nombre d'échantillon   
*@return le min, le max, la moyenne et la valeur médiane en analysant les gains par la  fonction 'balance' 
 @test
 # analyse(10000);;
- : int * int * float * float = (-1, 35, 5.0854, 3.)
# analyse(10000);;
- : int * int * float * float = (6, 33, 5.0458, 3.)
 * *)  
let analyse (n) = let e = echantillon (n) in (min_number(e),max_number(e),moyenne(e),mediane(e));;




(********** Mon algorithme optimal pour le coût  selon le théorie de "Codage de Huffman"***********)
let rec getFlist = function 
  |Feuille (a,sb)-> [Feuille (a,sb)]
  |Noeud(a,sb1,sb2)-> (getFlist sb1) @ (getFlist sb2);; 
(**
*@param deux string_builder 
*@return calculer la longueur après la concaténation 
*)
let numAfterCon sb1 sb2 = lg (concat sb1 sb2);;

(**
*@param deux string_builder 
*@return calculer la longueur  plus faible dans la list (compare par n)
*)
let rec getLgMin1 n list = 
  match list with
  |[]-> 0
  |t::[]-> n
  |t::b::q-> if (n)>(numAfterCon t b) then getCostMin1 (numAfterCon t b) (b::q) 
  else getLgMin1 n (b::q) ;;
(**
*@param une liste de feuilles 
*@return obtenir la longueur des deux premier Feuilles
*)
let getLGFirst list = match list with
  |[]->failwith "Wrong input"
  |t::[]->failwith "Wrong input"
  |t::b::q->numAfterCon t b;;

 (**
*@param une liste de feuilles 
*@return la liste qui a concaténé les deux feuilles successifs dont la concaténation a la longueur le plus petit.
*)
let rec concatMin list = let lgMin list = getLgMin1 (getLGFirst list) list in
  let min=lgMin list in
   match list with
     |[]->failwith "Wrong"
     |t::[]->failwith "Wrong"
     |t::b::q->if (numAfterCon t b)=min then (concat t b)::q
     else t::(concatMin (b::q));;   
      
  (**
*@param un arbre
*@return l'arbre qui a été optimisé par l'algorithme "huffman".
 @test
 # let cas = Noeud(6,Noeud(3,Feuille(1,"A"),Noeud(2,Feuille(1,"A"),Feuille(1,"A"))),Feuille(3,"ATT"));;
val cas : string_builder =
  Noeud (6,
   Noeud (3, Feuille (1, "A"), Noeud (2, Feuille (1, "A"), Feuille (1, "A"))),
   Feuille (3, "ATT"))
# cost cas;;
- : int = 11
#  cost (balance cas);;
- : int = 12
# cost (huffman cas);;
- : int = 11
# balance cas;;
- : string_builder =
Noeud (6, Noeud (2, Feuille (1, "A"), Feuille (1, "A")),
 Noeud (4, Feuille (1, "A"), Feuille (3, "ATT")))
# huffman cas;;
- : string_builder =
Noeud (6,
 Noeud (3, Noeud (2, Feuille (1, "A"), Feuille (1, "A")), Feuille (1, "A")),
 Feuille (3, "ATT"))
*)    
let huffman ab =let rec huffmanL list  = match list with
  |[]->failwith "Wrong input"
  |t::[]->failwith "Wrong input"
  |t::q::[]-> [(concat t q)]
  |t::b::q ->huffmanL (concatMin (t::b::q))
 in
   match huffmanL (getFlist ab) with
  |[]->failwith "Wrong"
  |q::t->q;;