type lexeme = 
|LBRACE
|RBRACE
|ADD
|SUB
|MUL
|ADDF
|SUBF
|MULF
|QUOT
|MOD
|Int of int
|Float of float
|INT
|FLOAT
|PLUS
|MINUS

(*teste si on était bien en train de lire un nombre, et auquel cas agit selon que c'est un entier ou un flottant*)
let test_gen str_mot str_nb typ signe lexl = 
  if !str_mot <> "" then failwith"pas bonne syntaxe"; (*si on est dans une séquence avec des lettres*)
  if !str_nb <> "" then begin
    if !typ then begin (*si le nombre est flottant*)
      if !signe then begin (*si le nombre est négatif*)
        lexl := Float(float_of_string !str_nb) :: MINUS :: !lexl;
        signe := false
      end
      else (*si le nombre est flottant positif*)
        lexl := Float(float_of_string(!str_nb)) :: !lexl;
      typ := false
    end
    else
      if !signe then begin (*flottant négatif*)
        lexl := Int(int_of_string(!str_nb)) :: MINUS :: !lexl;
        signe := false
      end
      else  (*flottant positif*)
        lexl := Int(int_of_string(!str_nb)) :: !lexl;
    str_nb := ""
  end
  


let analyse_lex s =
  let n = String.length s in
  let lexl = ref [] in (*liste de lexemes (enregistrée à l'envers, on la retourne à la fin)*)
  let en_cours_mot = ref "" in
  let en_cours_nombre = ref "" in
  let signe = ref false in (*true si le nombre actuel est négatif*)
  let type_nb = ref false in (*true si le nombre actuel est flottant*)
  let compteur_op = ref 0 in (*incrémenté si on croise une opération*)
  for i = 0 to (n-2) do
    match s.[i] with
      |'(' ->
        if !en_cours_mot = "int" then lexl := LBRACE :: INT :: !lexl
        else if !en_cours_mot = "float" then lexl := LBRACE :: FLOAT :: !lexl
	      else if (!en_cours_mot <> "" && !en_cours_nombre <> "") then failwith"pas dans le  langage" (*on a un mélange de lettres et chiffres, ou bien un nombre, qui ne peut pas précéder une ( *)
        else lexl := LBRACE :: !lexl;
        en_cours_mot := ""; (*dans tous les cas une ( met fin au mot, donc on remet la variable à ""*)
	      compteur_op := 0;
        signe := false ;
        type_nb := false
      |')' ->
	      if !compteur_op <> 0 then failwith"op avant )"
        else test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
	      lexl := RBRACE :: !lexl;
        (*met fin à tous les décomptes*)
        en_cours_nombre := "";
	      en_cours_mot := "";
        signe := false ;
        type_nb := false
      |'+' ->
	      (*if !compteur_op > 1 then failwith"plus de deux opérations à la suite"
	      else if !compteur_op = 1 then begin
	        if s.[i+1] <> '(' then failwith"deux opérations à la suite"
	        else lexl := PLUS :: !lexl
        end
	      else begin
          if (i=0 && s.[i+1] = '(') then
            lexl := PLUS :: !lexl
          else begin
	          test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
            compteur_op := !compteur_op + 1;
	          if s.[i+1] = '.' then lexl := ADDF :: !lexl (*teste si c'est une opération flottante*)
            else lexl := ADD :: !lexl
          end
	      end;
        signe := false ;
        type_nb := false*)

        if !compteur_op > 1 then failwith"plus de deux opérations à la suite"
	      else if !compteur_op = 1 then begin (*on vient de voir une opération*)
          (*teste si c'est un - de négation d'un nombre*)
          if '0' <= s.[i+1] && s.[i+1] <= '9' then 
            lexl := PLUS :: !lexl
          (*teste si c'est un + d'une expression*)
          else 
	          if s.[i+1] <> '(' then failwith"deux opérations à la suite" 
	          else lexl := PLUS :: !lexl
        end
	      else begin 
          (*teste si c'est un + de nombre*)
          if (i = 0 && '0' <= s.[i+1] && s.[i+1] <= '9') then ()
          else begin
            (*si c'est un + d'une expression*)
            if s.[i+1] = '(' then lexl := PLUS :: !lexl
            (*sinon c'est une soustraction classique*)
            else begin
	            test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
	            if s.[i+1] = '.' then begin
                compteur_op := !compteur_op + 1;
                lexl := ADDF :: !lexl (*si c'est une opération flottante*)
              end 
              else begin
                compteur_op := !compteur_op + 1;
                lexl := ADD :: !lexl
              end 
              end
            end
          end;
          signe := false ;
          type_nb := false

      |'*' ->
	      if !compteur_op <> 0 then failwith"deux opérations à la suite"
	      else begin
	        test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
          compteur_op := !compteur_op + 1;
	        if s.[i+1] = '.' then lexl := MULF :: !lexl (*teste si c'est une opération flottante*)
          else lexl := MUL :: !lexl
	      end;
        signe := false ;
        type_nb := false
      |'-' ->
	      if !compteur_op > 1 then failwith"plus de deux opérations à la suite"
	      else if !compteur_op = 1 then begin (*on vient de voir une opération*)
          (*teste si c'est un - de négation d'un nombre*)
          if '0' <= s.[i+1] && s.[i+1] <= '9' then
            if !signe then failwith"deux fois moins"
            else signe := true
          (*teste si c'est un moins de négation d'une expression*)
          else 
	          if s.[i+1] <> '(' then failwith"deux opérations à la suite" 
	          else lexl := MINUS :: !lexl
        end
	      else begin 
          (*teste si c'est une négation de nombre*)
          if (i = 0 && '0' <= s.[i+1] && s.[i+1] <= '9') then
            if !signe then failwith"deux fois moins"
            else signe := true
          else begin
            (*si c'est la négation d'une expression*)
            if s.[i+1] = '(' then lexl := MINUS :: !lexl
            (*sinon c'est une soustraction classique*)
            else begin
	            test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
	            if s.[i+1] = '.' then begin
              compteur_op := !compteur_op + 1;
                lexl := SUBF :: !lexl (*si c'est une opération flottante*)
              end
              else begin
                compteur_op := !compteur_op + 1;
                lexl := SUB :: !lexl
              end 
              end
            end
          end;
        type_nb := false 
      |'/' ->
	      if !compteur_op <> 0 then failwith"deux opérations à la suite"
	      else begin
          compteur_op := !compteur_op + 1;
	        test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
	        lexl := QUOT :: !lexl
	      end;
        signe := false;
        type_nb := false
      |'%' ->
	      if !compteur_op <> 0 then failwith"deux opérations à la suite"
	      else begin
          compteur_op := !compteur_op + 1;
	        test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
	        lexl := MOD :: !lexl
	      end;
        signe := false 
      |' ' ->
        if !en_cours_mot = "int" then
          lexl := INT :: !lexl
        else if !en_cours_mot = "float" then
          lexl := FLOAT :: !lexl
	      else test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
        en_cours_mot := "";
        en_cours_nombre := "";
        signe := false;
        type_nb := false 
      |'.' ->
        if i = 0 then begin (*le premier nombre est flottant < 1*)
          let x = s.[i+1] in
	        if ('0' <= x && x <= '9') then begin
	          type_nb := true; 
	          en_cours_nombre := "."
	        end
          else failwith"erreur synt"
        end
      else if (s.[i-1] = '+' || s.[i-1] = '-' || s.[i-1] = '*') then () (*on a déjà compté que c'est une opération flottante*)
      else begin (*le point indique un nombre flottant ou c'est une erreur*)
	      if !type_nb then failwith"deux points !!!" (*on est actuellement dans un nombre et on a croisé le point flottant -> point en trop*)
	      else if !compteur_op <> 0 then begin (*on vient de voir une opération*)
          let x = s.[i+1] in
	        if ('0' <= x && x <= '9') then begin
	          type_nb := true; 
	          en_cours_nombre := "."
	        end
        else
          failwith"deux op ou deux points à la suite !"
        end
        else if !en_cours_nombre <> "" then begin (*on est dans un nombre et le point indique qu'il est flottant*)
          en_cours_nombre := !en_cours_nombre ^ ".";
          type_nb := true
        end
	      else (*on regarde si le point introduit un flottant, sinon c'est une erreur*)
	        let x = s.[i+1] in
	        if ('0' <= x && x <= '9') then begin
	          type_nb := true;
	          en_cours_nombre := "."
	      end
      end 
      |x ->
        compteur_op := 0;
        if '0' <= x && x <= '9' then
          en_cours_nombre := !en_cours_nombre ^ (String.make 1 s.[i])
        else
          en_cours_mot := !en_cours_mot ^ (String.make 1 s.[i])
  done;
  begin
  match s.[n-1] with (*On regarde le cas particulier du dernier élément pour le traiter différemment*)
    |'(' -> failwith"finit par une parenthèse ouvrante"
    |')' ->
      test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
      lexl := RBRACE :: !lexl
    |' ' ->
      test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
    |'.' ->
      if !type_nb then failwith"deux points !!!"
      else if !en_cours_nombre <> "" then  begin
        en_cours_nombre := !en_cours_nombre ^ ".";
        type_nb := true;
        test_gen en_cours_mot en_cours_nombre type_nb signe lexl
      end
      else failwith"mauvaise syntaxe"
    |x ->
      if '0' <= x && x <= '9' then begin
        en_cours_nombre := !en_cours_nombre ^ (String.make 1 s.[n-1]);
	      test_gen en_cours_mot en_cours_nombre type_nb signe lexl
      end
      else failwith"mauvaise syntaxe"
  end;
  List.rev !lexl

