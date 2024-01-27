with Ada.Strings.Maps.Constants; use Ada.Strings.Maps;
with Ada.Text_IO;                use Ada.Text_IO;
with Utils;                      use Utils;
with Ada.Integer_Text_IO;
use  Ada.Integer_Text_IO;

package body Memoire is

    type T_TableauDeclare;
    type T_Liste_TypesTableau is access T_TableauDeclare;
    type T_TableauDeclare is
        record
            Nom: Unbounded_String;
            BorneInf: Integer;
            BorneSup : Integer;
            TypeStocke : T_Type;
            Suivant : T_Liste_TypesTableau;
        end record;

    -- TODO : spec + ajouter pre condition
    procedure creer_nouveau_typetableau(ChaineDeclaration : in Unbounded_String; ListeTableaux : in out T_Liste_TypesTableau)
    is
        Current_Line : Unbounded_String;
        Splitted_Line : Unbounded_String;

    begin
        -- on parcoure la liste pour ajouter un nouveau type en fin de liste
        if ListeTableaux /= null then
            creer_nouveau_typetableau(ChaineDeclaration, ListeTableaux.Suivant);
        else
            ListeTableaux := new T_TableauDeclare;
            Current_Line := ChaineDeclaration;
            -- recupere le mot "type"
            slice_mot (Current_Line, Splitted_Line, "T");
            slice_mot (Current_Line, Splitted_Line, " ");
            -- recupere le nom du tableau
            slice_mot (Current_Line, Splitted_Line, " ");
            ListeTableaux.Nom := Splitted_Line;

            -- parcoure jusqu'a la borne inf
            slice_mot (Current_Line, Splitted_Line, "(");
            slice_mot (Current_Line, Splitted_Line, ".");
            ListeTableaux.BorneInf := Integer'Value(To_String(Splitted_Line));

            -- parcoure jusqu'a la borne sup
            slice_mot (Current_Line, Splitted_Line, ".");
            slice_mot (Current_Line, Splitted_Line, ")");
            ListeTableaux.BorneSup := Integer'Value(To_String(Splitted_Line));

            if Index(Current_Line, "d'") /= 0 then
                -- parcoure apres le d' pour obtenir le type
                slice_mot (Current_Line, Splitted_Line, "'");
            else
                -- parcoure apres le "de" pour obtenir le type
                slice_mot (Current_Line, Splitted_Line, " ");
                slice_mot (Current_Line, Splitted_Line, " ");
            end if;

            --traduction du type
            if Current_Line = "entiers" or else Current_Line = "booléens" then
                ListeTableaux.TypeStocke := ENTIER;
            elsif Current_Line = "chaines" or else Current_Line = "caractères" then
                ListeTableaux.TypeStocke := CHAINE;
            end if;

            ListeTableaux.Suivant := null;
        end if;

    end;

    function getDeclarationType(nomType : Unbounded_String; ListeTableaux : T_Liste_TypesTableau) return T_Liste_TypesTableau is
        Current_Tableau : T_Liste_TypesTableau;
    begin
        Current_Tableau := ListeTableaux;
        while Current_Tableau /= null and then Current_Tableau.Nom /= nomType loop
            Current_Tableau := Current_Tableau.Suivant;
        end loop;
        return Current_Tableau;
    end getDeclarationType;


    procedure ajouterCaseMemoire(Current_Mem_Integer : in out P_Memoire_Entier.T_Case_Memoire; NomVariable : in Unbounded_String; TypeData : in P_Memoire_Entier.T_Type) is
    begin
        Current_Mem_Integer.Cle := NomVariable;
        Current_Mem_Integer.TypeOfData := TypeData;
        Current_Mem_Integer.Suivant := new P_Memoire_Entier.T_Var;
        -- Recuperation de l'element suivant
        Current_Mem_Integer := Current_Mem_Integer.Suivant;
    end ajouterCaseMemoire;

    procedure ajouterCaseMemoire(Current_Mem_Chaine : in out P_Memoire_String.T_Case_Memoire; NomVariable : in Unbounded_String; TypeData : in P_Memoire_String.T_Type) is
    begin
        Current_Mem_Chaine.Cle := NomVariable;
        Current_Mem_Chaine.TypeOfData := TypeData;
        Current_Mem_Chaine.Suivant := new P_Memoire_String.T_Var;
        -- Recuperation de l'element suivant
        Current_Mem_Chaine := Current_Mem_Chaine.Suivant;
    end ajouterCaseMemoire;




   -- Declare toutes les variables en memoire
   -- @param Mem : la memoire
   -- @param NomFichierCode : le nom du fichier du code ou sont declarees les variables
   procedure DeclarerVariables (Mem : out T_Memoire; NomFichierCode : in String) is
        Current_Mem_Integer : P_Memoire_Entier.T_Case_Memoire;
        Current_Mem_Chaine  : P_Memoire_String.T_Case_Memoire;
        Current_Line        : Unbounded_String;
        Splitted_Line       : Unbounded_String;
        Fini                : Boolean;
        Current_Type        : T_Type;
        Code                : File_Type;
        ListeTableaux : T_Liste_TypesTableau;
        Current_Tableau : T_Liste_TypesTableau;
        NomTableau : Unbounded_String;
        NomVariableTableau : Unbounded_String;
   begin
      Ouvrir_Fichier_Lecture(NomFichierCode, Code);
      Fini                := False;
      Mem.Entiers         := new P_Memoire_Entier.T_Var;
      Mem.Chaines         := new P_Memoire_String.T_Var;
      Current_Mem_Integer := Mem.Entiers;
      Current_Mem_Chaine  := Mem.Chaines;
      -- Tant qu'on n'a pas atteint la fin du fichier ni la ligne "Debut"
        while not Fini loop

            Current_Line := To_Unbounded_String(Get_Line(Code));

            -- Verifier qu'on n'a pas atteint la ligne "Debut"
            if Index(Current_Line, "Début") > 0 and Index(Current_Line, ":") = 0 then
                Fini := True;
            else

                -- Si pas commentaire ou declaration debut programme (a ameliorer ?)
                if Index (Current_Line, "--") = 0 then
                    -- cas de la declaration d'un type (tableau)
                    if (Index(Current_Line, "Type") > 0 and Index(Current_Line, "tableau") > 0) then
                        creer_nouveau_typetableau(Current_Line, ListeTableaux);

                    -- on verifie la presence des deux point
                    elsif Index (Current_Line, ":") > 0 then
                        -- Split au niveau de ':'
                        slice_mot(Current_Line, Splitted_Line, ":");
                        -- Recuperation du type
                        -- les booleens sont traites comme des entiers
                        if (Current_Line = " Entier" or Current_Line = " Booléen") then
                            Current_Type := ENTIER;
                        elsif (Current_Line = " Chaine" or Current_Line = " Caractère") then
                            Current_Type := CHAINE;
                        else
                            -- on cherche si le type a ete declare avant
                            if ListeTableaux /= null then
                                NomTableau := To_Unbounded_String(Strip_Space(To_String(Current_Line)));
                                Current_Tableau := getDeclarationType(NomTableau, ListeTableaux);
                                if Current_Tableau.TypeStocke = ENTIER then
                                    Current_Type := TAB_ENTIER;
                                elsif Current_Tableau.TypeStocke = CHAINE then
                                    Current_Type := TAB_CHAINE;
                                end if;

                            end if;

                        end if;
                    end if;

                    Current_Line := Splitted_Line;

                    -- Split au niveau de ','
                    slice_mot (Current_Line, Splitted_Line, ",");
                    while Length (Splitted_Line) > 0 loop
                        Splitted_Line := Translate(Splitted_Line, Ada.Strings.Maps.Constants.Lower_Case_Map);
                        Splitted_Line := To_Unbounded_String(Strip_Space(To_String(Splitted_Line)));
                        -- Initialisation de la memoire en fonction de son type
                        if Current_Type = ENTIER then
                            ajouterCaseMemoire(Current_Mem_Integer, Splitted_Line, P_Memoire_Entier.ENTIER);
                        elsif Current_Type = CHAINE then
                            ajouterCaseMemoire(Current_Mem_Chaine, Splitted_Line, P_Memoire_String.CHAINE);
                        elsif Current_Type = TAB_ENTIER then
                            for i in Current_Tableau.BorneInf..Current_Tableau.BorneSup loop
                                NomVariableTableau := Splitted_Line & To_Unbounded_String(Strip_Space("(" & Integer'Image(i) & ")"));
                                ajouterCaseMemoire(Current_Mem_Integer, NomVariableTableau, P_Memoire_Entier.TAB_ENTIER);
                            end loop;
                        elsif Current_Type = TAB_CHAINE then
                            for i in Current_Tableau.BorneInf..Current_Tableau.BorneSup loop
                                NomVariableTableau := Splitted_Line & To_Unbounded_String(Strip_Space("(" & Integer'Image(i) & ")"));
                                ajouterCaseMemoire(Current_Mem_Chaine, NomVariableTableau, P_Memoire_String.TAB_CHAINE);
                            end loop;
                        end if;
                        -- Split au niveau de ','
                        slice_mot (Current_Line, Splitted_Line, ",");
                    end loop;
                end if;
            end if;
        end loop;
        Close(Code); -- fermeture du fichier contenant le code
   end DeclarerVariables;

   -- Permet de trouver la case memoire de la variable ayant Cle comme nom
   -- @param CaseMem : la premiere case memoire
   -- @param Cle : le nom de la variable
   -- @return : la case memoire contenant la variable
   function trouver_case_entier (CaseMem : in P_Memoire_Entier.T_Case_Memoire; Cle : in Unbounded_String) return P_Memoire_Entier.T_Case_Memoire is
      Pos_entier : P_Memoire_Entier.T_Case_Memoire;
   begin
      -- Recuperer la liste chainee
      Pos_entier := CaseMem;
      -- a chaque iteration on transforme la cle en minuscule pour que ce ne soit pas case sensitive
      while Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map) /= Pos_entier.Cle loop -- on n'est pas oblige de commencer par Pos_entier /= null or else car on sait que la variable existe en memoire
         Pos_entier := Pos_entier.Suivant;
      end loop;
      return Pos_entier;
   end trouver_case_entier;

   -- Permet de trouver la case memoire de la variable ayant Cle comme nom
   -- @param CaseMem : la premiere case memoire
   -- @param Cle : le nom de la variable
   -- @return : la case memoire contenant la variable
   function trouver_case_chaine (CaseMem : in P_Memoire_String.T_Case_Memoire; Cle : in Unbounded_String) return P_Memoire_String.T_Case_Memoire is
      Pos_chaine : P_Memoire_String.T_Case_Memoire;
   begin
      -- Recuperer la liste chainee
      Pos_chaine := CaseMem;
      -- a chaque iteration on transforme la cle en minuscule pour que ce ne soit pas case sensitive
      while Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map) /= Pos_chaine.Cle loop -- on n'est pas oblige de commencer par Pos_entier /= null or else car on sait que la variable existe en memoire
         Pos_chaine := Pos_chaine.Suivant;
      end loop;
      return Pos_chaine;
   end trouver_case_chaine;

   -- Modifie la donnee d'une variable existante en memoire
   -- @param Mem : la memoire modifiee
   -- @param Cle : le nom de la variable a modifier
   -- @param Data : la nouvelle valeur de la variable
   procedure Modifier_Entier (Mem : in out T_Memoire; Cle : in Unbounded_String; Data : in Integer) is
      Pos_entier : P_Memoire_Entier.T_Case_Memoire;
    begin
      Pos_entier := trouver_case_entier(Mem.Entiers, Cle);
      Pos_entier.Data := Data;
   end Modifier_Entier;

   -- Modifie la donnee d'une variable existante en memoire
   -- @param Mem : la memoire modifiee
   -- @param Cle : le nom de la variable a modifier
   -- @param Data : la nouvelle valeur de la variable
   procedure Modifier_Chaine (Mem : in out T_Memoire; Cle : in Unbounded_String; Data : in Unbounded_String) is
      Pos_chaine : P_Memoire_String.T_Case_Memoire;
   begin
      Pos_chaine := trouver_case_chaine(Mem.Chaines, Cle);
      Pos_chaine.Data := Data;
   end Modifier_Chaine;

    -- Recupere la valeur d'une variable par son nom a partir d'une case memoire (est appelee par RecupererValeur_Entier)
    -- @param CaseMem : la case memoire dans laquelle on verifie si la cle correspond
    -- @param Cle : la cle de la variable dont on cherche la valeur
    -- @return : la valeur associee au nom de la variable
    function RecupererValeur_Entier_FromCase(CaseMem : in P_Memoire_Entier.T_Case_Memoire; Cle : in Unbounded_String) return Integer
    is
    begin
        -- Verifier si la cle est la bonne pour chaque case memoire
        if Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map) = CaseMem.Cle then
            return CaseMem.All.Data;
        else
            return RecupererValeur_Entier_FromCase(CaseMem.Suivant, Cle); -- on sait que la cle existe donc si elle n'a pas encore ete trouvee
            -- alors elle est dans la case suivante (pas la peine de tester si Suivant /= Null)
        end if;
   end RecupererValeur_Entier_FromCase;

    -- Recupere la valeur d'une variable par son nom a partir d'une case memoire (est appelee par RecupererValeur_Chaine)
    -- @param CaseMem : la case memoire dans laquelle on verifie si la cle correspond
    -- @param Cle : la cle de la variable dont on cherche la valeur
    -- @return : la valeur associee au nom de la variable
   function RecupererValeur_Chaine_FromCase(CaseMem : in P_Memoire_String.T_Case_Memoire; Cle : in Unbounded_String) return Unbounded_String
   is
   begin
       -- Verifier si la cle est la bonne pour chaque case memoire
       if Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map) = CaseMem.Cle then
           return CaseMem.All.Data;
       else
           return RecupererValeur_Chaine_FromCase(CaseMem.Suivant, Cle); -- on sait que la cle existe donc si elle n'a pas encore ete trouvee
           -- alors elle est dans la case suivante (pas la peine de tester si Suivant /= Null)
       end if;
   end RecupererValeur_Chaine_FromCase;

   -- Recupere la valeur d'une variable par son nom (a partir de l'objet memoire)
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
   -- @return la valeur de la variable
   -- Pre-condition : Mem.Entiers /= Null et la cle existe dans la memoire (car le programme intermediaire est bien forme)
   function RecupererValeur_Entier (Memoire : in T_Memoire; Cle : in Unbounded_String) return Integer
   is
      Type_Var : Unbounded_String;
   begin
      Type_Var := RecupererType(Memoire, Cle);
      -- on verifie s'il s'agit d'une variable (type=entier) ou d'une constante (type=null)
      if Type_Var = "Entier" or else Type_Var = "TabEntier" then
         -- Recuperer la valeur en memoire
         return RecupererValeur_Entier_FromCase(Memoire.Entiers, Cle);
      elsif Type_Var = "null" then
         return Integer'Value(To_String(Cle));
      else
         return -1;
      end if;
   end RecupererValeur_Entier;

    -- Recupere la valeur d'une variable par son nom (a partir de l'objet memoire)
    -- @param Mem : la memoire dans laquelle est stockee la variable
    -- @param Cle : le nom de la variable recherchee
    -- @return la valeur de la variable
    -- Pre-condition : Mem.Chaines /= Null et la cle existe dans la memoire (car le programme intermediaire est bien forme)
    function RecupererValeur_Chaine (Memoire : in T_Memoire; Cle : in Unbounded_String) return Unbounded_String
    is
        Type_Var : Unbounded_String;
    begin
        Type_Var := RecupererType(Memoire, Cle);
        -- on verifie s'il s'agit d'une variable (type=chaine) ou d'une constante (type=null)
        if Type_Var = "Chaine" or else Type_Var = "TabChaine" then
            -- Recuperer la valeur en memoire
            return RecupererValeur_Chaine_FromCase(Memoire.Chaines, Cle);
        elsif Type_Var = "null" then
            return Cle;
        else
            return To_Unbounded_String("");
        end if;
    end RecupererValeur_Chaine;

    -- fonction disant si le type est une chaine de caractère
    -- surchargé
    function donner_Type(Mem : in T_Memoire; Cle : in Unbounded_String; search_type : in out P_Memoire_String.T_Case_Memoire) return Unbounded_String is
    begin
        search_type := Mem.Chaines;
        while (search_type /= null) and then (search_type.Cle /= Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map)) loop
            search_type := search_type.Suivant;
        end loop;
        if search_type /= null then
            if search_type.TypeOfData = CHAINE then
                return To_Unbounded_String("Chaine");
            elsif search_type.TypeOfData = TAB_CHAINE then
                return To_Unbounded_String("TabChaine");
            end if;
        end if;
        return To_Unbounded_String("null");
    end donner_Type;

    -- fonction disant si le type est un entier
    -- surchargé
    function donner_Type(Mem : in T_Memoire; Cle : in Unbounded_String; search_type : in out P_Memoire_Entier.T_Case_Memoire) return Unbounded_String is
    begin
        search_type := Mem.Entiers;
        while (search_type /= null) and then (search_type.Cle /= Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map)) loop
            search_type := search_type.Suivant;
        end loop;
        if search_type /= null then
            if search_type.TypeOfData = ENTIER then
                return To_Unbounded_String("Entier");
            elsif search_type.TypeOfData = TAB_ENTIER then
                return To_Unbounded_String("TabEntier");
            end if;

        end if;
        return To_Unbounded_String("null");
    end donner_Type;

   -- Recupere le type d'une variable par son nom
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
   -- @return le type de la variable
    function RecupererType (Mem : in T_Memoire; Cle : in Unbounded_String) return Unbounded_String is
        Pos_entier : P_Memoire_Entier.T_Case_Memoire;
        Pos_string : P_Memoire_String.T_Case_Memoire;
        TypeRenvoi : Unbounded_String;
    begin
        -- Recherche si la cle est un entier
        TypeRenvoi := donner_Type(Mem, Cle, Pos_entier);
        if TypeRenvoi /= To_Unbounded_String("null") then
            return TypeRenvoi;
        end if;
        Pos_string := Mem.Chaines;
        --recherche si la cle est une chaine de caractère
        TypeRenvoi := donner_Type(Mem, Cle, Pos_string);
        if TypeRenvoi /= To_Unbounded_String("null") then
            return TypeRenvoi;
        end if;
        return To_Unbounded_String("null");
    end RecupererType;

    -- affiche le contenue de la liste de chaine de caractère
    procedure afficher_liste_chaine(Mem : in P_Memoire_String.T_Case_Memoire; adresse : in out Integer) is
    begin
        if Mem.Suivant = null then
            null;
        else
            Put("        Adresse en mémoire : "); Put(adresse, 1); Put_Line("");
            Put("        Nom de la variable : "); Put(To_String(Mem.Cle)); Put_Line("");
            Put("        Valeur de la variable : "); Put_Line(To_String(Mem.Data)); Put_Line("");
            adresse := adresse +1;
            afficher_liste_chaine(Mem.Suivant, adresse);
        end if;
    end afficher_liste_chaine;

    -- affiche le contenue de la liste d'entier
    procedure afficher_liste_entier(Mem : in P_Memoire_Entier.T_Case_Memoire; adresse : in out Integer) is
    begin
        if Mem.Suivant = null then
            null;
        else
            Put("        Adresse en mémoire : "); Put(adresse, 1); Put_Line("");
            Put("        Nom de la variable : "); Put(To_String(Mem.Cle)); Put_Line("");
            Put("        Valeur de la variable : "); Put(Mem.Data, 1); Put_Line(""); Put_Line("");
            adresse := adresse +1;
            afficher_liste_entier(Mem.Suivant, adresse);
        end if;
    end afficher_liste_entier;

    -- affiche les variables avec leur valeur et leur position
    -- @param Mem : la memoire dans laquelle est stockee les variables
    procedure afficher_variables(Mem : in T_Memoire) is
        adresse : Integer;
    begin
        adresse := 1;
        Put_Line("    Affichage de la liste d'entier ou boolean : ");
        afficher_liste_entier(Mem.Entiers, adresse);
        Put_Line("    Affichage de la liste de caractère ou chaine : ");
        afficher_liste_chaine(Mem.Chaines, adresse);
    end afficher_variables;

end Memoire;
