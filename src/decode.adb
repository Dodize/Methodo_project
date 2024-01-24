
with Utils; use Utils;
with Ada.Text_IO ;                use Ada.Text_IO ;
with Ada.Integer_Text_IO ;        use Ada.Integer_Text_IO ;


package body Decode is

-- Importer le module de memoire dans .adb

   -- Initialise le compteur
   -- @param CP : compteur a initialiser
   procedure init_CP (CP: out Integer) is
   begin
      CP := 1;
   end init_CP;

   -- Retourne le nombre d'instructions du tableau d'instructions (le nombre de ligne)
   -- @param Tab : le tableau d'instructions
   -- @return : le nombre d'instructions
    function get_nombre_instruc(Tab : in T_tab_instruc) return Integer is
    begin
        return Tab'Length;
    end get_nombre_instruc;


   -- Incremente le compteur
   -- @param CP : compteur a incrementer
   procedure increm_CP (CP : in out Integer) is
   begin
      CP := CP+1;
    end increm_CP;


    procedure ecrire(mem : in T_Memoire; Cle : in Unbounded_String) is
      type_var : Unbounded_String;
    begin
       type_var := RecupererType(mem, cle);
       if type_var = "Entier" then
          Put(RecupererValeur_entier(mem, cle));
       elsif type_var = "Chaine" then
          Put(To_String(RecupererValeur_chaine(mem, cle)));
       end if;
    end ecrire;

    procedure lire(mem : in out T_Memoire; Cle : in Unbounded_String) is
       type_var : Unbounded_String;
       val_entier : Integer;
       val_string : String (1..100); --on définit un max arbitrairement
    begin
       type_var := RecupererType(mem, cle);
       if type_var = "Entier" then
          Get(val_entier);
          Modifier_Entier(mem, cle, val_entier);
       elsif type_var = "Chaine" then
          Get(val_string);
          Modifier_Chaine(mem, cle, To_Unbounded_String(val_string));
       end if;
    end lire;


    -- Effectue l'instruction goto en allant au label souhaite (sous forme de numero de ligne)
    -- @param CP : le compteur qui doit etre modifie
    -- @param label : le numero de la ligne a laquelle on souhaite aller
    -- @exception GOTO_OUT_OF_RANGE_EXCEPTION : si le label ne correspond pas a une ligne valide
    -- (la ligne est < 1 ou > au nombre d'instructions)
   procedure instru_goto (CP : out Integer; label : in Integer; Tab : in T_tab_instruc) is
    begin
        if (label < 1) or else (label > Tab'Length) then
            raise GOTO_OUT_OF_RANGE_EXCEPTION;
        else
            CP := label;
        end if;
    end instru_goto;


    -- Calcule le resultat d'une operation composee de deux entiers
    -- @param CleVal1 : le nom de la premiere variable de l'operation
    -- @param Operation : l'operation a effectuer
    -- @param CleVal2 : le nom de la deuxieme variable de l'operation
    -- @param Memoire : la memoire
    -- @return : le resultat de l'operation
    function result_instru_entier(CleVal1 : in Unbounded_String; Operation : in Unbounded_String; CleVal2 : in Unbounded_String; Memoire : in out T_memoire) return Integer is
        Result : Integer;
        Valeur1 : Integer;
        Valeur2 : Integer;
    begin
        Valeur1 := RecupererValeur_Entier(Memoire, CleVal1);
        Valeur2 := RecupererValeur_Entier(Memoire, CleVal2);
        if Operation = "+" then
            Result := Valeur1 + Valeur2;
        elsif Operation = "-" then
            Result := Valeur1 - Valeur2;
        elsif Operation = "/" then
            Result := Valeur1 / Valeur2;
        elsif Operation = "*" then
            Result := Valeur1 * Valeur2;
        elsif Operation = "=" then
            if Valeur1 = Valeur2 then
                Result := 1;
            else
                Result := 0;
            end if;
        elsif Operation = "<" then
            if Valeur1 < Valeur2 then
                Result := 1;
            else
                Result := 0;
            end if;
        elsif Operation = ">" then
            if Valeur1 > Valeur2 then
                Result := 1;
            else
                Result := 0;
            end if;
        elsif Operation = "<=" then
            if Valeur1 <= Valeur2 then
                Result := 1;
            else
                Result := 0;
            end if;
        elsif Operation = ">=" then
            if Valeur1 >= Valeur2 then
                Result := 1;
            else
                Result := 0;
            end if;
        elsif Operation = "OR" then
            if Valeur1/=0 or else Valeur2/=0 then
                Result := 1;
            else
                Result := 0;
            end if;
        elsif Operation = "AND" then
            if Valeur1/=0 and then Valeur2/=0 then
                Result := 1;
            else
                Result := 0;
            end if;
        else
            raise OP_NON_RECONNUE_EXCEPTION;
        end if;

        return Result;

    end result_instru_entier;

    -- Calcule le resultat d'une operation composee de deux chaines de caracteres
    -- @param CleVal1 : le nom de la premiere variable de l'operation
    -- @param Operation : l'operation a effectuer
    -- @param CleVal2 : le nom de la deuxieme variable de l'operation
    -- @param Memoire : la memoire
    -- @return : le resultat de l'operation
    procedure result_instru_chaine(CleVal1 : in Unbounded_String; Operation : in Unbounded_String; CleVal2 : in Unbounded_String; Memoire : in out T_memoire; New_Chaine : out Unbounded_String ; New_Bool : out Integer) is
        Valeur1 : Unbounded_String;
        Valeur2 : Unbounded_String;
    begin
        Valeur1 := RecupererValeur_Chaine(Memoire, CleVal1);
        Valeur2 := RecupererValeur_Chaine(Memoire, CleVal2);
        if Operation = "+" then
            New_Chaine := Valeur1 & Valeur2;
        elsif Operation = "=" then
            if Valeur1 = Valeur2 then
                New_Bool := 1;
            else
                New_Bool := 0;
            end if;
        elsif Operation = "<" then
            if Valeur1 < Valeur2 then
                New_Bool := 1;
            else
                New_Bool := 0;
            end if;
        elsif Operation = ">" then
            if Valeur1 > Valeur2 then
                New_Bool := 1;
            else
                New_Bool := 0;
            end if;
        else
            -- exception
            Null;
        end if;
    end result_instru_chaine;

    -- Effectue l'instruction operation demande
    -- @param CleVariableAffectation : le nom de la variable affectee
    -- @param Valeur1 : le nom de la premiere variable de l'operation
    -- @param Operation : le type d'operation a realiser (en chaine)
    -- @param Valeur2 : le nom de la deuxieme variable de l'operation
    -- @param Memoire : la memoire
    procedure instru_op (CleVariableAffectation : in Unbounded_String; Valeur1 : in Unbounded_String; Operation : in Unbounded_String; Valeur2 : in Unbounded_String; Memoire : in out T_memoire; CP: in out Integer) is
        Type_Var : Unbounded_String;
        New_Bool : Integer;
        New_Chaine : Unbounded_String;
        FirstOfValeur1 : Character; -- le premier caractere de valeur1
    begin

        New_Bool := -1;
        Type_Var := RecupererType(Memoire, Valeur1);
        FirstOfValeur1 := To_String(Valeur1)(To_String(Valeur1)'First);
        if Type_Var = "Entier" or else FirstOfValeur1 /= '"' then
            Modifier_Entier(Memoire, CleVariableAffectation, result_instru_entier(Valeur1, Operation, Valeur2, Memoire));
        elsif Type_Var = "Chaine" or else FirstOfValeur1 = '"' then
            result_instru_chaine(Valeur1, Operation, Valeur2, Memoire, New_Chaine, New_Bool);
            if New_Bool /= -1 then
                Modifier_Entier(Memoire, CleVariableAffectation, New_Bool);
            else
                Modifier_Chaine(Memoire, CleVariableAffectation, New_Chaine);
            end if;
        else
            Null; --TODO quand on aura les autres types
        end if;
        increm_CP(CP);
    end instru_op;



    -- Effectue l'instruction affectation
    -- @param CleVariable : le nom de la variable a modifier
    -- @param Valeur : la nouvelle valeur de la variable (recuperee en string dans le tableau d'instruction)
    -- @param Mem : la memoire
    procedure instru_affectation (CleVariable : in Unbounded_String; Valeur : in Unbounded_String; Mem : in out T_memoire; CP : in out Integer) is
        Type_Var : Unbounded_String;
    begin
        Type_Var := RecupererType(Mem, CleVariable);
        if Type_Var = "Entier" then
            Modifier_Entier(Mem, CleVariable, Integer'Value(To_String(Valeur)));
        elsif Type_Var = "Chaine" then
            Modifier_Chaine(Mem, CleVariable, Valeur);
        else
            null;
        end if;
        increm_CP(CP);
    end instru_affectation;


     --  Effectue l'instruction if en fonction de la variable VariableBool
     --  @param VariableBool : le nom de la variable booleenne
     --  @param Label : la valeur que doit prendre CP si le booleen vaut True
     --  @param CP : le compteur de la ligne courante
     --  @param mem : la memoire
    procedure instru_if (VariableBool : in Unbounded_String; Label : in Integer; CP : in out Integer; mem : in T_memoire) is
        valeur : Integer; -- valeur de "VariableBool"
    begin
        -- Recuperer la valeur du booleen
        valeur := RecupererValeur_Entier(mem, VariableBool);
        if valeur = 0 then -- cas ou la valeur vaut false : on ne fait rien
            increm_CP(CP);
        else -- cas ou la valeur vaut true : on change la valeur de CP
            CP := Label;
        end if;
   end instru_if;


   -- Effectue l'instruction null, soit ne fait rien
   procedure instru_null(CP : in out Integer) is
   begin
      increm_CP(CP);
   end instru_null;

   -- Rempli une ligne du tableau en mettant en forme l'instruction null ou un commentaire
   -- @param Tab : tableau a remplir
   -- @param Index : ligne du tableau a remplir
   procedure remplir_ligne_null(Tab: out T_tab_instruc; Pos : in Integer) is
   begin
      Tab(Pos).pos1 := To_Unbounded_String("NULL");
   end remplir_ligne_null;

   -- Rempli une ligne du tableau en mettant en forme l'instruction
   -- @param Tab : tableau a remplir
   -- @param Index : ligne du tableau a remplir
   -- @param Ligne : ligne dont on recupere les informations
   -- @param Mot : premier mot de la ligne
   procedure remplir_ligne(Tab: out T_tab_instruc; Pos : in Integer; Ligne: in out Unbounded_String; Mot : in Unbounded_String) is
   begin
      Tab(Pos).pos1 := Mot;
      slice_mot(Ligne, Tab(Pos).pos2, " ");
      slice_mot(Ligne, Tab(Pos).pos3, " ");
      slice_mot(Ligne, Tab(Pos).pos4, " ");
   end remplir_ligne;

   -- Rempli une ligne du tableau en mettant en forme l'instru x y op z
   -- @param Tab : tableau a remplir
   -- @param Pos : ligne du tableau a remplir
   -- @param Ligne : ligne dont on recupere les informations
   -- @param Mot : premier mot de la ligne
   procedure remplir_ligne_op(Tab: out T_tab_instruc; Pos : in Integer; Ligne: in out Unbounded_String; Mot : in Unbounded_String) is
      inutile : Unbounded_String;
   begin
      Tab(Pos).pos1 := Mot;
      slice_mot(Ligne, inutile, " ");
      slice_mot(Ligne, Tab(Pos).pos2, " ");
      if index(Ligne, " ") = 0 then
         Tab(Pos).pos3 := To_Unbounded_String("affect");
      else
        slice_mot(Ligne, Tab(Pos).pos3, " ");
         slice_mot(Ligne, Tab(Pos).pos4, " ");
      end if;
    end remplir_ligne_op;

   -- Rempli une ligne du tableau en lire/ecrire dans la premiere case et la variable dans la deuxième
   -- @param Tab : tableau a remplir
   -- @param Pos : ligne du tableau a remplir
   -- @param Ligne : ligne dont on recupere les informations
   -- @param Mot : premier mot de la ligne
    procedure remplir_ligne_lire_ecrire(Tab: out T_tab_instruc; Pos : in Integer; Mot : in out Unbounded_String) is
    begin
        slice_mot(Mot, Tab(Pos).pos1, "(");
        slice_mot(Mot, Tab(Pos).pos2, ")");
    end remplir_ligne_lire_ecrire;

    -- function intermédiaire pour simplifier la lecture
    -- renvoie si le mot correspond à une opération
    -- cad que ce n'est pas GOTO, pas IF, pas Lire et pas Ecrire (null et commentaire ayant été traité plus tot)
    function est_op(Mot : in Unbounded_String) return boolean is
    begin
        return (Mot /= "GOTO" and Mot /= "IF" and not (Length(Mot)>=4 and then To_String(Mot)(1..4) = "Lire") and not (Length(Mot)>=6 and then To_String(Mot)(1..6) = "Ecrire"));
    end;

    -- procédure permettant de retirer des espaces avant le premier mot de la ligne
    -- pour supprimer l'indentation du code intermédiaire
    procedure enlever_indentation (Ligne : in out Unbounded_String) is
        inutile : Unbounded_String;
    begin
        while Index(Ligne, " ") = 1 loop
            slice_mot(Ligne, inutile, " ");
        end loop;
    end;

   -- Rempli le tableau avec les instructions du fichier
   -- en mettant sous la forme voulue
   -- Si la ligne est un coommentaire : devient un null dans le tableau
   -- @param Tab : tableau a remplir
   -- @param NomFichier : le nom du fichier contenant code source a utiliser pour remplir le tableau
    procedure remplir_tab_instruc (Tab : in out T_tab_instruc; NomFichier : in String) is
        Ligne : Unbounded_String;
        Mot : Unbounded_String;
        Pos : Integer;
        Fichier : File_Type;
    begin
        Ouvrir_Fichier_Lecture(NomFichier, Fichier);
        Pos := 1;
        parcourir_debut(Fichier);
        -- Parcours le code en remplicant le tableau
        Ligne := To_Unbounded_String(Get_Line(Fichier));
        while not (Ligne = "Fin") loop
            enlever_indentation(Ligne);
            slice_mot(Ligne, Mot, " ");
            if Mot = "--" or Mot = "NULL" then
                remplir_ligne_null(Tab, Pos);
            elsif est_op(Mot) then -- pour op car traitement special
                remplir_ligne_op(Tab, Pos, Ligne, Mot);
            elsif Mot = "GOTO" or Mot = "IF" then
                remplir_ligne(Tab, Pos, Ligne, Mot);
            else -- dans le cas de lire ou ecrire car mise en forme speciale
                remplir_ligne_lire_ecrire(Tab, Pos, Mot);
            end if;
            Pos := Pos + 1;
            Ligne := To_Unbounded_String(Get_Line(Fichier));
        end loop;
        Close(Fichier);
    end remplir_tab_instruc;


   -- Retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   -- @return : la partie 1 de l'instruction
   function recuperer_instru_pos1 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String is
   begin
      return Tab(CP).pos1;
   end recuperer_instru_pos1;

   -- Retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   -- @return : la partie 2 de l'instruction
   function recuperer_instru_pos2 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String is
   begin
      return Tab(CP).pos2;
   end recuperer_instru_pos2;

   -- Retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   -- @return : la partie 3 de l'instruction
   function recuperer_instru_pos3 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String is
   begin
      return Tab(CP).pos3;
   end recuperer_instru_pos3;

   -- Retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   -- @return : la partie 4 de l'instruction
   function recuperer_instru_pos4 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String is
   begin
      return Tab(CP).pos4;
    end recuperer_instru_pos4;


    --affiche le numéro de ligne où l'on ira à la prochaine instruction
    -- @param Tab : tableau comptenant les instructions
    -- @param CP : compteur
    -- @param mem : liste chainee comptenant les variables et leurs valeurs
    procedure afficher_label(Tab : in T_tab_instruc; CP : in Integer; Mem: in T_Memoire) is
        instruct : Unbounded_String;
    begin
        instruct := recuperer_instru_pos1(Tab, CP);
        Put_Line("");
        Put("Prochaine ligne : ");
        if instruct = "GOTO" then
            Put(To_String(recuperer_instru_pos2(Tab, CP)));
        elsif instruct = "IF" then
            if RecupererValeur_Entier(Mem, recuperer_instru_pos2(Tab, CP)) = 1 then
                Put(To_String(recuperer_instru_pos4(Tab, CP)));
            else
                put(CP+1);
            end if;
        else --instruct = "NULL" or instruct = "Lire" or instruct = "Ecrire" or on est dans affectation ou op
            Put(CP+1);
        end if;
        Put_Line("");
    end afficher_label;


    -- Pour debugger : Affihe memoire CP et la memoire regroupant les valeurs des differentes variables
    -- @param Tab : tableau comptenant les instructions
    -- @param CP : compteur
    -- @param mem : liste chainee comptenant les variables et leurs valeurs
    procedure afficher (Tab : in T_tab_instruc; CP : in Integer; mem : in out T_memoire) is
    begin
        afficher_label(Tab, CP, Mem);
        Put_Line("Variable en mémoire : ");
        --afficher_variable(mem); --dans mémoire
    end afficher;


   -- Effectue une instruction passee en parametre en fonction de son type (GOTO, null, if, op)
   -- Et modifie le CP en consequence
   -- @param Tab : tableau comptenant les instructions
   -- @param CP : compteur
   -- @param mem : liste chainee contenant les variables et leurs valeurs
    procedure effectuer_instru (Tab : in T_tab_instruc; CP : in out Integer; mem : in out T_memoire) is
        InstruPart1, InstruPart2, InstruPart3, InstruPart4 : Unbounded_String; -- differentes parties de l'instruction
    begin
        -- Recuperer l'instruction
        InstruPart1 := recuperer_instru_pos1(Tab, CP);
        InstruPart2 := recuperer_instru_pos2(Tab, CP);
        InstruPart3 := recuperer_instru_pos3(Tab, CP);
        InstruPart4 := recuperer_instru_pos4(Tab, CP);

        -- Realiser l'instruction
        -- en fonction du premier mot de l'instruction, effectuer la bonne operation
        if InstruPart1 = "NULL" then
            instru_null(CP);
        elsif InstruPart1 = "GOTO" then
            instru_goto(CP, Integer'Value(To_String(InstruPart2)), Tab);
        elsif InstruPart1 = "IF" then
            instru_if(InstruPart2, Integer'Value(To_String(InstruPart4)), CP, mem);
        elsif InstruPart1 = "Lire" then
            Lire(mem, InstruPart2);
        elsif InstruPart1 = "Ecrire" then
            Ecrire(mem, InstruPart2);
        else
            -- affectation sinon operation
            if InstruPart3 = "affect" then
                instru_affectation(InstruPart1, InstruPart2, mem, CP);
            else
                instru_op(InstruPart1, InstruPart2, InstruPart3, InstruPart4, mem, CP);
            end if;
        end if;

    end effectuer_instru;

end Decode;
