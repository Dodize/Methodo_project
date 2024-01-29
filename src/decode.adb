
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

    -- Permet de traduire l'indice d'un tableau en remplacant la variable par sa valeur
    -- Par exemple si CleVariable = "Tab(i)" et i=5 alors CleVariable vaudra "Tab(5)"
    -- @param CleVariable : le nom de la variable de type tableau
    -- @param Mem : la memoire permettant d'acceder a la valeur de la variable
    procedure traduire_indice_tableau(CleVariable : in out Unbounded_String; Memoire : in T_memoire) is
        RecopieCle, ClePartie1, SplittedCle : Unbounded_String; -- parties de la cle permettant de modifier la valeur de l'indice
        IndiceRecupere : Integer;
    begin
        RecopieCle := CleVariable;
        -- on recupere la partie avant la parenthese et a l'interieur de celle ci
        slice_mot (RecopieCle, ClePartie1, "(");
        slice_mot (RecopieCle, SplittedCle, ")");
        -- test si l'indice ne contient pas que des chiffres (dans ce cas il s'agit d'une variable dont la valeur doit etre cherchee)
        if not (for all Char of To_String(SplittedCle) => Char in '0' .. '9') then
            IndiceRecupere := RecupererValeur_Entier(Memoire, SplittedCle);
            -- reassemble les parties de la cle
            CleVariable := To_Unbounded_String(Strip_Space(To_String(ClePartie1) & "(" & Integer'Image(IndiceRecupere) & ")"));
        end if;
    end traduire_indice_tableau;

    -- effectue l'instruction ecrire dans le terminal
    -- @param Mem : la memoire
    -- @param Cle : la variable a ecrire (constante ou nom de variable)
    -- @param CP : le compteur d'instruction
    procedure ecrire(Mem : in T_Memoire; Cle : in Unbounded_String; CP: in out Integer) is
        type_var : Unbounded_String;
        CleTraduite : Unbounded_String;
        DebutCleTraduite : Character;
    begin
        CleTraduite := Cle;
        -- si le nom de la variable contient des parentheses alors il s'agit d'un tableau
        -- on traduit eventuellement l'indice s'il s'agit d'une variable
        if Index(CleTraduite, "(") > 0 then
            traduire_indice_tableau(CleTraduite, Mem);
        end if;

        type_var := RecupererType(mem, CleTraduite);
        if type_var = "Entier" or else type_var = "TabEntier" then
            Put(RecupererValeur_entier(mem, CleTraduite));
        elsif type_var = "Chaine" or else type_var = "TabChaine" then
            Put(To_String(RecupererValeur_chaine(mem, CleTraduite)));
        else
            -- dans le cas d'une constante, on selectionne le premier caractere
            DebutCleTraduite := To_String(CleTraduite)(To_String(CleTraduite)'First);
            -- s'il s'agit de guillemets, alors on les supprime
            if DebutCleTraduite = '"' or DebutCleTraduite = ''' then
                Delete(CleTraduite, 1, 1);
                CleTraduite := Unbounded_Slice(CleTraduite, 1, Length(CleTraduite) - 1);
            end if;
            Put(To_String(CleTraduite));
        end if;
        increm_CP(CP);
    end ecrire;

    -- effectue l'instruction lire (ce que l'utilisateur tappe dans le terminal)
    -- @param mem : la memoire
    -- @param Cle : le nom de la variable qui est modifiee
    -- @param CP : le compteur d'instruction
    procedure lire(mem : in out T_Memoire; Cle : in Unbounded_String; CP: in out Integer) is
        type_var : Unbounded_String;
        val_entier : Integer;
        val_string : String (1..100); --on définit un max arbitrairement
        CleTraduite : Unbounded_String;
    begin
        CleTraduite := Cle;

        -- si le nom de la variable contient des parentheses alors il s'agit d'un tableau
        -- on traduit eventuellement l'indice s'il s'agit d'une variable
        if Index(CleTraduite, "(") > 0 then
            traduire_indice_tableau(CleTraduite, Mem);
        end if;

        type_var := RecupererType(mem, CleTraduite);
        if type_var = "Entier" or else type_var = "TabEntier" then
            Get(val_entier);
            Modifier_Entier(mem, CleTraduite, val_entier);
        elsif type_var = "Chaine" or else type_var = "TabChaine" then
            Get(val_string);
            Modifier_Chaine(mem, CleTraduite, To_Unbounded_String(val_string));
        end if;
        increm_CP(CP);
    end lire;


    -- Effectue l'instruction goto en allant au label souhaite (sous forme de numero de ligne)
    -- @param CP : le compteur qui doit etre modifie
    -- @param Label : le numero de la ligne a laquelle on souhaite aller
    -- @param TabInstruc : le tableau d'instructions permettant de verifier si le label est correct
    -- @exception GOTO_OUT_OF_RANGE_EXCEPTION : si le label ne correspond pas a une ligne valide
    -- (la ligne est < 1 ou > au nombre d'instructions)
    procedure instru_goto (CP : out Integer; Label : in Integer; TabInstruc : in T_tab_instruc) is
    begin
        if (Label < 1) or else (Label > TabInstruc'Length) then
            raise GOTO_OUT_OF_RANGE_EXCEPTION;
        else
            CP := Label;
        end if;
    end instru_goto;


    -- Calcule le resultat d'une operation composee de deux entiers
    -- @param CleVal1 : le nom de la premiere variable de l'operation ou la valeur de la constante
    -- @param Operation : l'operation a effectuer
    -- @param CleVal2 : le nom de la deuxieme variable de l'operation ou la valeur de la constante
    -- @param Memoire : la memoire
    -- @return : le resultat de l'operation
    -- @exception OP_NON_RECONNUE_EXCEPTION : si l'operation n'est pas reconnue
    function result_instru_entier(CleVal1 : in Unbounded_String; Operation : in Unbounded_String; CleVal2 : in Unbounded_String; Memoire : in out T_memoire) return Integer is
        Result : Integer;
        Valeur1, Valeur2 : Integer; --transformation des variables en leur veritable valeur
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
        elsif Operation = "/=" then
            if Valeur1 /= Valeur2 then
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
    -- @param CleVal1 : le nom de la premiere variable de l'operation ou la valeur de la constante
    -- @param Operation : l'operation a effectuer
    -- @param CleVal2 : le nom de la deuxieme variable de l'operation ou la valeur de la constante
    -- @param Memoire : la memoire
    -- @New_Chaine : le resultat si l'operation retourne une chaine
    -- @New_Bool : le resultat si l'operation retourne un booleen
    -- @exception OP_NON_RECONNUE_EXCEPTION : si l'operation n'est pas reconnue
    procedure result_instru_chaine(CleVal1 : in Unbounded_String; Operation : in Unbounded_String; CleVal2 : in Unbounded_String; Memoire : in out T_memoire; New_Chaine : out Unbounded_String ; New_Bool : out Integer) is
        Valeur1, Valeur2 : Unbounded_String; -- transformation des variables en leur veritable valeur
    begin
        New_Bool := -1;
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
        elsif Operation = "/=" then
            if Valeur1 /= Valeur2 then
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
        elsif Operation = "<=" then
            if Valeur1 <= Valeur2 then
                New_Bool := 1;
            else
                New_Bool := 0;
            end if;
        elsif Operation = ">=" then
            if Valeur1 >= Valeur2 then
                New_Bool := 1;
            else
                New_Bool := 0;
            end if;
        else
            raise OP_NON_RECONNUE_EXCEPTION;
        end if;
    end result_instru_chaine;

    -- indique si la modification se fait sur un entier ou non
    -- @param Type_Var : le type de la variable1 de l'operation
    -- @param FirstOfValeur1 : le premier caractere de la variable1
    -- @param FirstOfValeur2 : le premier caractere de la variable2
    -- return : true si la modification se fait sur un entier, false sinon
    function estModificationEntier(Type_Var : in Unbounded_String; FirstOfValeur1 : in Character; FirstOfValeur2 : in Character) return Boolean is
    begin
        -- retourne true si : la variable est de type Entier ou Tableau d'entier
        -- si la variable est une constante (directement sa valeur) elle ne doit pas contenir de guillemet en premier caractere
        return Type_Var = "Entier" or else Type_Var = "TabEntier" or else (Type_Var = "null" and (FirstOfValeur1 /= '"' and FirstOfValeur1 /= ''' and FirstOfValeur2 /= '"' and FirstOfValeur2 /= '"'));
    end estModificationEntier;

    -- indique si la modification se fait sur une chaine ou non
    -- @param Type_Var : le type de la variable1 de l'operation
    -- @param FirstOfValeur1 : le premier caractere de la variable1
    -- @param FirstOfValeur2 : le premier caractere de la variable2
    -- return : true si la modification se fait sur une chaine, false sinon
    function estModificationChaine(Type_Var : in Unbounded_String; FirstOfValeur1 : in Character; FirstOfValeur2 : in Character) return Boolean is
    begin
        -- retourne true si : la variable est de type Chaine ou Tableau de chaines
        -- si la variable est une constante (directement sa valeur) elle doit contenir des guillemets en premier caractere
        return Type_Var = "Chaine" or else Type_Var = "TabChaine" or else (Type_Var = "null" and (FirstOfValeur1 = '"' or FirstOfValeur1 /= ''' or FirstOfValeur2 /= '"' or FirstOfValeur2 /= '"'));
    end estModificationChaine;


    -- Fonction intermédiaire
    -- si le nom de la variable contient des parentheses alors il s'agit d'un tableau
    -- on traduit eventuellement l'indice s'il s'agit d'une variable
    procedure trad_clef(cle : in out Unbounded_String; Mem : in out T_Memoire) is
    begin
        if Index(cle, "(") > 0 then
            traduire_indice_tableau(cle, Mem);
        end if;
    end trad_clef;

    -- Effectue l'instruction operation demandee
    -- @param CleVariableAffectation : le nom de la variable affectee
    -- @param Valeur1 : le nom de la premiere variable de l'operation
    -- @param Operation : le type d'operation a realiser (en chaine)
    -- @param Valeur2 : le nom de la deuxieme variable de l'operation
    -- @param Memoire : la memoire
    -- @param CP : le compteur d'instructions
    -- @exception OP_NON_RECONNUE_EXCEPTION : si l'operation n'est pas reconnue
    procedure instru_op (CleVariableAffectation : in Unbounded_String; Valeur1 : in Unbounded_String; Operation : in Unbounded_String; Valeur2 : in Unbounded_String; Memoire : in out T_memoire; CP: in out Integer) is
        Type_Var : Unbounded_String;                  -- permet de stocker le type d'une variable
        New_Bool : Integer;                           -- resultat d'une operation retournant un booleen
        New_Chaine : Unbounded_String;                -- resultat d'une operation retournant une chaine
        FirstOfValeur1, FirstOfValeur2 : Character;   -- le premier caractere de valeur1/valeur2
        CleTraduite : Unbounded_String;               -- traduction de la variable affectee s'il s'agit d'un tableau
        -- afin de remplacer l'indice par sa valeur
        Valeur1Traduite, Valeur2Traduite : Unbounded_String;

    begin
        CleTraduite := CleVariableAffectation;
        Valeur1Traduite := Valeur1;
        Valeur2Traduite:= Valeur2;

        trad_clef(CleTraduite, Memoire);
        trad_clef(Valeur1Traduite, Memoire);
        trad_clef(Valeur2Traduite, Memoire);

        New_Bool := -1;
        Type_Var := RecupererType(Memoire, Valeur1Traduite);

        -- on recupere le premier caractere de Valeur1 et Valeur2 (si Valeur2 existe)
        FirstOfValeur1 := To_String(Valeur1Traduite)(To_String(Valeur1Traduite)'First);
        if Length(Valeur2Traduite) > 0 then
            FirstOfValeur2 := To_String(Valeur2Traduite)(To_String(Valeur2Traduite)'First);
        end if;

        -- si Type_var vaut null => alors il s'agit d'une constante et on regarde le premier caractère pour connaitre son type
        if estModificationEntier(Type_Var, FirstOfValeur1, FirstOfValeur2) then
            Modifier_Entier(Memoire, CleTraduite, result_instru_entier(Valeur1Traduite, Operation, Valeur2Traduite, Memoire));
        elsif estModificationChaine(Type_Var, FirstOfValeur1, FirstOfValeur2)  then
            -- s'il s'agit de constantes, on enleve les guillemets
            if FirstOfValeur1 = '"' or FirstOfValeur1 = ''' then
                Delete(Valeur1Traduite, 1, 1);
                Valeur1Traduite := Unbounded_Slice(Valeur1Traduite, 1, Length(Valeur1Traduite) - 1);
            end if;

            if FirstOfValeur2 = '"' or FirstOfValeur2 = ''' then
                Delete(Valeur2Traduite, 1, 1);
                Valeur2Traduite := Unbounded_Slice(Valeur2Traduite, 1, Length(Valeur2Traduite) - 1);
            end if;

            result_instru_chaine(Valeur1Traduite, Operation, Valeur2Traduite, Memoire, New_Chaine, New_Bool);

            -- on test s'il le resultat est un booleen ou une chaine
            if New_Bool /= -1 then
                Modifier_Entier(Memoire, CleVariableAffectation, New_Bool);
            else
                Modifier_Chaine(Memoire, CleVariableAffectation, New_Chaine);
            end if;
        end if;
        increm_CP(CP);
    end instru_op;


    -- Effectue l'instruction affectation
    -- @param CleVariable : le nom de la variable a modifier
    -- @param Valeur : la nouvelle valeur de la variable (nom de variable ou valeur reelle)
    -- @param Mem : la memoire
    -- @param CP : le compteur d'instructions
    procedure instru_affectation (CleVariable : in Unbounded_String; Valeur : in Unbounded_String; Mem : in out T_memoire; CP : in out Integer) is
        Type_Var : Unbounded_String;       -- permet de stocker le type de la variable a effecter
        ValeurInt : Integer;               -- valeur reelle de la variable si c'est un integer
        ValeurString : Unbounded_String;   -- valeur reelle de la variable si c'est une chaine
        CleTraduite, ValeurTraduite : Unbounded_String; -- permet de traduire les noms des variables s'il s'agit de tableaux indices par une variable
    begin
        -- recopie de la cle pour la modifier
        CleTraduite := CleVariable;
        -- recopie de la valeur a effecter pour la modifier
        ValeurTraduite := Valeur;

        trad_clef(CleTraduite, Mem);
        trad_clef(ValeurTraduite, Mem);

        Type_Var := RecupererType(Mem, CleTraduite);
        if Type_Var = "Entier" or else Type_Var = "TabEntier" then
            -- on test s'il s'agit d'une constante ou d'un nom de variable et on recupere la valeur
            if RecupererType(Mem, ValeurTraduite) = "null" then
                ValeurInt := Integer'Value(To_String(ValeurTraduite));
            else
                ValeurInt := RecupererValeur_Entier(Mem, ValeurTraduite);
            end if;
            Modifier_Entier(Mem, CleTraduite, ValeurInt);

        elsif Type_Var = "Chaine" or else Type_Var = "TabChaine" then
            -- on test s'il s'agit d'une constante ou d'un nom de variable et on recupere la valeur
            if RecupererType(Mem, ValeurTraduite) /= "null" then
                ValeurString := RecupererValeur_Chaine(Mem, ValeurTraduite);
            else
                ValeurString := ValeurTraduite;
                Delete(ValeurString, 1, 1);
                ValeurString := Unbounded_Slice(ValeurString, 1, Length(ValeurString) - 1);
            end if;
            Modifier_Chaine(Mem, CleTraduite, ValeurString);
        end if;

        increm_CP(CP);
    end instru_affectation;


    --  Effectue l'instruction if en fonction de la variable VariableBool
    --  @param VariableBool : le nom de la variable booleenne
    --  @param Label : la valeur que doit prendre CP si le booleen vaut True
    --  @param CP : le compteur de la ligne courante
    --  @param Memoire : la memoire
    --  @param TabInstruc : le tableau d'instructions permettant de verifier que le label est correct
    procedure instru_if (VariableBool : in Unbounded_String; Label : in Integer; CP : in out Integer; Memoire : in T_memoire; TabInstruc : in T_tab_instruc) is
        valeur : Integer; -- valeur de "VariableBool"
    begin
        -- Recuperer la valeur du booleen
        valeur := RecupererValeur_Entier(Memoire, VariableBool);
        if valeur = 0 then -- cas ou la valeur vaut false : on ne fait rien
            increm_CP(CP);
        else -- cas ou la valeur vaut true : on change la valeur de CP
            instru_goto(CP, Label, TabInstruc);
        end if;
    end instru_if;


    -- Effectue l'instruction null, soit ne fait rien et augmente le compteur d'instructions
    -- @param CP : le compteur d'instructions
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
    -- @param Pos : ligne du tableau a remplir
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
        inutile : Unbounded_String;     -- partie inutile de l'instruction a chaque decoupage
        ligne_temp : Unbounded_String;
        First_part : Unbounded_String;
        Second_part : Unbounded_String;
        Third_part : Unbounded_String;
        is_chaine : Boolean;
    begin
        is_chaine := False;

        Tab(Pos).pos1 := Mot;
        slice_mot(Ligne, inutile, " "); -- on enlève "<-" présent dans le code intermédiaire
        slice_string(To_String(Ligne), First_part, Second_part, Third_part, is_chaine);

        if is_chaine then --si une chaine de caractère est trouvée
            if Second_part = "" then
                slice_mot(Ligne, Tab(Pos).pos2, " ");
                if index(Ligne, " ") = 0 then -- on est dans le cas d'une affectation
                    Tab(Pos).pos3 := To_Unbounded_String("affect");
                else -- on est dans le cas d'une opération
                    slice_mot(Ligne, Tab(Pos).pos3, " ");
                    slice_mot(Ligne, Tab(Pos).pos4, " ");
                end if;
            elsif First_part = "" then
                Tab(Pos).pos2 := Second_part;
                slice_mot(Third_part, inutile, " ");
                ligne_temp := Third_part;
                slice_string(To_String(ligne_temp), First_part, Second_part, Third_part, is_chaine);
                if index(Ligne, " ") = 0 then -- on est dans le cas d'une affectation
                    Tab(Pos).pos3 := To_Unbounded_String("affect");
                else -- on est dans le cas d'une opération
                    if Second_part = "" then
                        slice_mot(First_part, Tab(Pos).pos3, " ");
                        slice_mot(First_part, Tab(Pos).pos4, " ");
                    else
                        slice_mot(First_part, Tab(Pos).pos3, " ");
                        Tab(Pos).pos4 := Second_part;
                    end if;
                end if;
            elsif Third_part = "" then
                slice_mot(First_part, Tab(pos).pos2, " ");
                if index(Ligne, " ") = 0 then -- on est dans le cas d'une affectation
                    Tab(Pos).pos3 := To_Unbounded_String("affect");
                else -- on est dans le cas d'une opération
                    slice_mot(First_part, Tab(Pos).pos3, " ");
                    Tab(Pos).pos4 := Second_part;
                end if;
            end if;
        else
            slice_mot(Ligne, Tab(Pos).pos2, " ");
            if index(Ligne, " ") = 0 then -- on est dans le cas d'une affectation
                Tab(Pos).pos3 := To_Unbounded_String("affect");
            else -- on est dans le cas d'une opération
                slice_mot(Ligne, Tab(Pos).pos3, " ");
                slice_mot(Ligne, Tab(Pos).pos4, " ");
            end if;
        end if;
    end remplir_ligne_op;

    -- Rempli une ligne du tableau en lire/ecrire dans la premiere case et la variable dans la deuxième
    -- @param Tab : tableau a remplir
    -- @param Pos : ligne du tableau a remplir
    -- @param Ligne : ligne dont on recupere les informations
    -- @param Mot : premier mot de la ligne
    procedure remplir_ligne_lire_ecrire(Tab: out T_tab_instruc; Pos : in Integer; Mot : in out Unbounded_String; Ligne : in out Unbounded_String) is
        --LastCharac : Integer;
        Mot_tempo : Unbounded_String;
    begin
        slice_mot(Mot, Tab(Pos).pos1, "(");
        Mot_tempo := Mot & " " & Ligne;
        slice_mot(Mot_tempo, Tab(Pos).pos2, ")");
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
    -- @param Ligne : la ligne d'instruction dont on souhaite retirer les espaces
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
    -- @exception ADA.IO_EXCEPTIONS.NAME_ERROR : si le fichier ayant pour nom "NomFichier" n'existe pas ou n'est pas accessible par le programme
    procedure remplir_tab_instruc (Tab : in out T_tab_instruc; NomFichier : in String) is
        Ligne : Unbounded_String;   -- ligne lue dans le fichier d'instructions
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
                remplir_ligne_lire_ecrire(Tab, Pos, Mot, Ligne);
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


    -- Affiche le numéro de ligne où l'on ira à la prochaine instruction
    -- @param Tab : tableau comptenant les instructions
    -- @param CP : compteur
    -- @param mem : liste chainee comptenant les variables et leurs valeurs
    procedure afficher_label(Tab : in T_tab_instruc; CP : in Integer; Mem: in T_Memoire) is
        instruct : Unbounded_String;
    begin
        instruct := recuperer_instru_pos1(Tab, CP);
        Put_Line("-------------------------------------------------------------------------------"); Put("Prochaine ligne : ");
        if instruct = "GOTO" then
            Put(Integer'Value(To_String(recuperer_instru_pos2(Tab, CP))), 1);
        elsif instruct = "IF" then
            if RecupererValeur_Entier(Mem, recuperer_instru_pos2(Tab, CP)) = 1 then
                Put(Integer'Value(To_String(recuperer_instru_pos4(Tab, CP))), 1);
            else
                put(CP+1, 1);
            end if;
        else --instruct = "NULL" or instruct = "Lire" or instruct = "Ecrire" or on est dans affectation ou op
            Put(CP+1, 1);
        end if;
        Put_Line("");
    end afficher_label;


    -- Pour debugger : Affiche le CP et la memoire regroupant les valeurs des differentes variables
    -- @param Tab : tableau comptenant les instructions
    -- @param CP : compteur
    -- @param mem : liste chainee comptenant les variables et leurs valeurs
    procedure afficher (Tab : in T_tab_instruc; CP : in Integer; mem : in out T_memoire) is
    begin
        afficher_label(Tab, CP, Mem);
        Put_Line("Variable en mémoire : ");
        afficher_variables(mem); --dans mémoire
    end afficher;


    -- Effectue une instruction passee en parametre en fonction de son type (GOTO, null, if, op)
    -- Et modifie le CP en consequence
    -- @param Tab : tableau comptenant les instructions
    -- @param CP : compteur des instructions
    -- @param mem : liste chainee contenant les variables et leurs valeurs (represente la memoire)
    -- @exception OP_NON_RECONNUE_EXCEPTION : si l'instruction a effectuer n'est pas reconnue
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
            instru_if(InstruPart2, Integer'Value(To_String(InstruPart4)), CP, mem, Tab);
        elsif InstruPart1 = "Lire" then
            lire(mem, InstruPart2, CP);
        elsif InstruPart1 = "Ecrire" then
            ecrire(mem, InstruPart2, CP);
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
