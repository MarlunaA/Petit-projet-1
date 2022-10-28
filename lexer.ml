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


let test_gen str_mot str_nb typ signe lexl = (*teste si on �tait bien en train de lire un nombre, et auquel cas agit selon que c'est un entier ou un flottant*)
  if !str_mot <> "" then failwith"pas bonne syntaxe";
  if !str_nb <> "" then begin
    if !typ then begin
      if !signe then begin
        lexl := Float(float_of_string !str_nb) :: MINUS :: !lexl;
        signe := false
      end
      else
        lexl := Float(float_of_string(!str_nb)) :: !lexl;
      typ := false
    end
    else
      if !signe then begin
        lexl := Int(int_of_string(!str_nb)) :: MINUS :: !lexl;
        signe := false
      end
      else 
        lexl := Int(int_of_string(!str_nb)) :: !lexl;
    str_nb := ""
  end
  


let analyse_lex s =
  let n = String.length s in
  let lexl = ref [] in
  let en_cours_mot = ref "" in
  let en_cours_nombre = ref "" in
  let signe = ref false in
  let type_nb = ref false in (*mis � jour en true si on croise un flottant*)
  let compteur_op = ref 0 in (*incr�ment� si on croise une op�ration*)
  for i = 0 to (n-2) do
    match s.[i] with
      |'(' ->
        if !en_cours_mot = "int" then lexl := LBRACE :: INT :: !lexl
        else if !en_cours_mot = "float" then lexl := LBRACE :: FLOAT :: !lexl
	      else if (!en_cours_mot <> "" && !en_cours_nombre <> "") then failwith"pas dans le  langage" (*on a un m�lange de lettres et chiffres, ou bien un nombre, qui ne peut pas pr�c�der une ( *)
        else lexl := LBRACE :: !lexl;
        en_cours_mot := ""; (*dans tous les cas une ( met fin au mot, donc on remet la variable � 0*)
	      compteur_op := 0
      |')' ->
	      if !compteur_op <> 0 then failwith"op avant )"
        else test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
	      lexl := RBRACE :: !lexl;
        en_cours_nombre := "";
	      en_cours_mot := ""
      |'+' ->
	      if !compteur_op > 1 then failwith"plus de deux op�rations � la suite"
	      else if !compteur_op = 1 then begin
          compteur_op := !compteur_op + 1;
	        if s.[i+1] <> '(' then failwith"deux op�rations � la suite"
	        else lexl := PLUS :: !lexl
        end
	      else begin
	        test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
          compteur_op := !compteur_op + 1;
	        if s.[i+1] = '.' then lexl := ADDF :: !lexl (*teste si c'est une op�ration flottante*)
          else lexl := ADD :: !lexl
	      end
      |'*' ->
	      if !compteur_op <> 0 then failwith"deux op�rations � la suite"
	      else begin
	        test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
          compteur_op := !compteur_op + 1;
	        if s.[i+1] = '.' then lexl := MULF :: !lexl
          else lexl := MUL :: !lexl
	      end
      |'-' ->
	      if !compteur_op > 1 then failwith"plus de deux op�rations � la suite"
	      else if !compteur_op = 1 then begin
          if '0' <= s.[i+1] && s.[i+1] <= '9' then
            if !signe then failwith"deux fois moins"
            else signe := true
          else 
            compteur_op := !compteur_op + 1;
	          if s.[i+1] <> '(' then failwith"deux op�rations � la suite"
	          else lexl := MINUS :: !lexl
        end
	      else
          if '0' <= s.[i+1] && s.[i+1] <= '9' then
            if !signe then failwith"deux fois moins"
            else signe := true
          else begin
            compteur_op := !compteur_op + 1;
	          test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
	          if s.[i+1] = '.' then lexl := SUBF :: !lexl
            else 
              lexl := SUB :: !lexl
            end
      |'/' ->
	      if !compteur_op <> 0 then failwith"deux op�rations � la suite"
	      else begin
          compteur_op := !compteur_op + 1;
	        test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
	        lexl := QUOT :: !lexl
	      end
      |'%' ->
	      if !compteur_op <> 0 then failwith"deux op�rations � la suite"
	      else begin
          compteur_op := !compteur_op + 1;
	        test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
	        lexl := MOD :: !lexl
	      end
      |' ' ->
        if !en_cours_mot = "int" then
          lexl := INT :: !lexl
        else if !en_cours_mot = "float" then
          lexl := FLOAT :: !lexl
	      else test_gen en_cours_mot en_cours_nombre type_nb signe lexl;
        en_cours_mot := "";
        en_cours_nombre := "";
      |'.' ->
	      if !type_nb then failwith"deux points !!!"
	      else if !compteur_op <> 0 then failwith"deux op ou deux points � la suite !"
        else if !en_cours_nombre <> "" then begin
          en_cours_nombre := !en_cours_nombre ^ ".";
          type_nb := true
        end
	      else
	      let x = s.[i+1] in
	      if ('0' <= x && x <= '9') then begin
	        type_nb := true;
	        en_cours_nombre := "."
	      end
      |x ->
        compteur_op := 0;
        if '0' <= x && x <= '9' then
          en_cours_nombre := !en_cours_nombre ^ (String.make 1 s.[i])
        else
          en_cours_mot := !en_cours_mot ^ (String.make 1 s.[i])
  done;
  begin
  match s.[n-1] with (*On regarde le cas particulier du dernier �l�ment pour le traiter diff�remment*)
    |'(' -> failwith"finit par une parenth�se ouvrante"
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

