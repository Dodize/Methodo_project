with Ada.Strings.Maps.Constants; use Ada.Strings.Maps;
with Ada.Text_IO;                use Ada.Text_IO;
with Utils;                      use Utils;
with Ada.Integer_Text_IO;
use  Ada.Integer_Text_IO;

package body Memoire is

    -- types internes permettant de declarer des tableaux depuis le code intermediaire
    -- on stocke pour chaque type tableau son nom, sa borne inf et sup, le type de donnees stockees
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

    -- Permet d'instancier un nouveau type de tableau (et l'ajouter en fin de liste)
    -- @param ChaineDeclaration : la ligne du code intermediaire correspondant a la declaration du type
    -- @param ListeTableaux : la liste des tableaux (peut valoir null si aucun tableau n'a encore ete declare)
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

    -- Recupere le type tableau dans la liste en fonction du nom du type
    -- @param nomType : le nom du type
    -- @param ListeTableaux : la liste des tableaux declares
    -- @return l'element type tableau correspondant au nomType
    -- Precondition : le type est present dans la liste (le code intermediaire etant bien forme, le type a ete declare avant
    -- son utilisation
    -- Postcondition : Result.Nom = NomType
    function getDeclarationTypeTableau(NomType : Unbounded_String; ListeTableaux : T_Liste_TypesTableau) return T_Liste_TypesTableau is
        Current_Tableau : T_Liste_TypesTableau;
    begin
        Current_Tableau := ListeTableaux;
        while Current_Tableau /= null and then Current_Tableau.Nom /= NomType loop
            Current_Tableau := Current_Tableau.Suivant;
        end loop;
        return Current_Tableau;
    end getDeclarationTypeTableau;

    -- Ajoute une nouvelle case memoire en fonction de la declaration de la variable de type entier ou booleen
    -- @param Current_Mem_Integer : la case memoire a instancier
    -- @param NomVariable : le nom de la variable
    -- @param TypeData : le type de la variable (pour differencier les tableaux des variables "simples")
    -- Precondition : Current_Mem_Integer /= null
    procedure ajouterCaseMemoire(Current_Mem_Integer : in out P_Memoire_Entier.T_Case_Memoire; NomVariable : in Unbounded_String; TypeData : in P_Memoire_Entier.T_Type) is
    begin
        Current_Mem_Integer.Cle := NomVariable;
        Current_Mem_Integer.TypeOfData := TypeData;
        Current_Mem_Integer.Suivant := new P_Memoire_Entier.T_Var;
        -- Recuperation de l'element suivant
        Current_Mem_Integer := Current_Mem_Integer.Suivant;
    end ajouterCaseMemoire;

    -- Ajoute une nouvelle case memoire en fonction de la declaration de la variable de type chaine ou caractere
    -- @param Current_Mem_Chaine : la case memoire a instancier
    -- @param NomVariable : le nom de la variable
    -- @param TypeData : le type de la variable (pour differencier les tableaux des variables "simples")
    -- Precondition : Current_Mem_Chaine /= null
    procedure ajouterCaseMemoire(Current_Mem_Chaine : in out P_Memoire_String.T_Case_Memoire; NomVariable : in Unbounded_String; TypeData : in P_Memoire_String.T_Type) is
    begin
        Current_Mem_Chaine.Cle := NomVariable;
        Current_Mem_Chaine.TypeOfData := TypeData;
        Current_Mem_Chaine.Suivant := new P_Memoire_String.T_Var;
        -- Recuperation de l'element suivant
        Current_Mem_Chaine := Current_Mem_Chaine.Suivant;
    end ajouterCaseMemoire;

    -- Recupere le type des variables de la ligne courante de declaration
    -- @param Current_Line : la ligne parcourue dans le fichier du code intermediaire
    -- @param Splitted_Line : la liste des noms de variables apres le split de la ligne courante
    -- @param ListeTableau : la liste des tableaux declares permettant de recuperer le type de tableau
    -- @param Current_Tableau : le tableau concerne par la ligne courante si un tableau est concerne
    function getCurrentType(Current_Line : in out Unbounded_String; Splitted_Line : out Unbounded_String; ListeTableaux : in T_Liste_TypesTableau; Current_Tableau : out T_Liste_TypesTableau) return T_Type is
        NomTableau : Unbounded_String;
    begin
        -- Split au niveau de ':'
        slice_mot(Current_Line, Splitted_Line, ":");
        -- Recuperation du type
        -- les booleens sont traites comme des entiers
        if (Current_Line = " Entier" or Current_Line = " Booléen") then
            return ENTIER;
        elsif (Current_Line = " Chaine" or Current_Line = " Caractère") then
            return CHAINE;
        else
            -- on cherche si le type a ete declare avant
            NomTableau := To_Unbounded_String(Strip_Space(To_String(Current_Line)));
            Current_Tableau := getDeclarationTypeTableau(NomTableau, ListeTableaux);
            if Current_Tableau.TypeStocke = ENTIER then
                return TAB_ENTIER;
            else
                return TAB_CHAINE;
            end if;
        end if;
    end getCurrentType;

    -- Declare une nouvelle variable en memoire
    -- @param Current_Line : la ligne courante lue dans le fichier du code intermediaire
    -- @param Splitted_Line : le nom de la variable courante
    -- @param Current_Type : le type de la variable a ajouter
    -- @param Current_Mem_integer : la case memoire libre concernant les variables de type entier/booleens
    -- @param Current_Mem_Chaine : la case memoire libre concernant les variables de type chaine/caractere
    -- @param Current_Tableau : si la variable est de type tableau, contient les caracteristiques de ce tableau
    procedure DeclarerUneVariable(Current_Line : in out Unbounded_String; Splitted_Line : in out Unbounded_String; Current_Type : in T_Type;
                                  Current_Mem_integer : in out P_Memoire_Entier.T_Case_Memoire; Current_Mem_Chaine : in out P_Memoire_String.T_Case_Memoire; Current_Tableau : in T_Liste_TypesTableau) is
        NomVariableTableau : Unbounded_String;
    begin
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
            -- ajout du nombre de variables concerne par le tableau (bornesup - borneinf = nombre de variables a ajouter)
            for i in Current_Tableau.BorneInf..Current_Tableau.BorneSup loop
                NomVariableTableau := Splitted_Line & To_Unbounded_String(Strip_Space("(" & Integer'Image(i) & ")"));
                ajouterCaseMemoire(Current_Mem_Chaine, NomVariableTableau, P_Memoire_String.TAB_CHAINE);
            end loop;
        end if;
        -- Split au niveau de ','
        slice_mot (Current_Line, Splitted_Line, ",");
    end DeclarerUneVariable;




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
                        Current_Type := getCurrentType(Current_Line, Splitted_Line, ListeTableaux, Current_Tableau);
                        Current_Line := Splitted_Line;
                        -- Split au niveau de ','
                        slice_mot (Current_Line, Splitted_Line, ",");
                        while Length (Splitted_Line) > 0 loop
                            DeclarerUneVariable(Current_Line, Splitted_Line, Current_Type, Current_Mem_Integer, Current_Mem_Chaine, Current_Tableau);
                        end loop;
                    end if;
                end if;
            end if;
        end loop;
        Close(Code); -- fermeture du fichier contenant le code
    end DeclarerVariables;


    -- Permet de trouver la case memoire de la variable ayant Cle comme nom
    -- @param CaseMem : la premiere case memoire
    -- @param Cle : le nom de la variable
    -- @return : la case memoire contenant la variable
    -- Precondition : la cle existe dans la liste des cases mémoire
    -- Postcondition : Result.Cle = Cle
    function trouver_case_entier (CaseMem : in P_Memoire_Entier.T_Case_Memoire; Cle : in Unbounded_String) return P_Memoire_Entier.T_Case_Memoire is
        Pos_entier : P_Memoire_Entier.T_Case_Memoire;
    begin
        -- Recuperer la liste chainee
        Pos_entier := CaseMem;
        -- a chaque iteration on transforme la cle en minuscule pour que ce ne soit pas case sensitive
        while Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map) /= Pos_entier.Cle loop
            Pos_entier := Pos_entier.Suivant;
        end loop;
        return Pos_entier;
    end trouver_case_entier;

    -- Permet de trouver la case memoire de la variable ayant Cle comme nom
    -- @param CaseMem : la premiere case memoire
    -- @param Cle : le nom de la variable
    -- @return : la case memoire contenant la variable
    -- Precondition : la cle existe dans la liste des cases mémoire
    -- Postcondition : Result.Cle = Cle
    function trouver_case_chaine (CaseMem : in P_Memoire_String.T_Case_Memoire; Cle : in Unbounded_String) return P_Memoire_String.T_Case_Memoire is
        Pos_chaine : P_Memoire_String.T_Case_Memoire;
    begin
        -- Recuperer la liste chainee
        Pos_chaine := CaseMem;
        -- a chaque iteration on transforme la cle en minuscule pour que ce ne soit pas case sensitive
        while Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map) /= Pos_chaine.Cle loop
            Pos_chaine := Pos_chaine.Suivant;
        end loop;
        return Pos_chaine;
    end trouver_case_chaine;


    -- Modifie la donnee d'une variable existante (mais pas forcement initialisee) de type entier/booleen
    -- @param Mem : la memoire contenant la variable a modifier
    -- @param Cle : le nom de la variable a modifier
    -- @param Data : la nouvelle valeur de la variable
    -- Precondition : la cle existe dans la liste des cases mémoire
    procedure Modifier_Entier(Mem : in out T_Memoire ; Cle : in Unbounded_String ; Data : in Integer) is
        Pos_entier : P_Memoire_Entier.T_Case_Memoire;
    begin
        Pos_entier := trouver_case_entier(Mem.Entiers, Cle);
        Pos_entier.Data := Data;
    end Modifier_Entier;

    -- Modifie la donnee d'une variable existante en memoire (mais pas forcement initialisee) de type chaine/caractere
    -- @param Mem : la memoire contenant la variable a modifier
    -- @param Cle : le nom de la variable a modifier
    -- @param Data : la nouvelle valeur de la variable
    -- Precondition : la cle existe dans la liste des cases memoire (
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
    -- Pre-condition : La cle existe dans la memoire
    function RecupererValeur_Entier_FromCase(CaseMem : in P_Memoire_Entier.T_Case_Memoire; Cle : in Unbounded_String) return Integer
    is
    begin
        -- Verifier si la cle est la bonne pour chaque case memoire
        if Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map) = CaseMem.Cle then
            return CaseMem.All.Data;
        else
            return RecupererValeur_Entier_FromCase(CaseMem.Suivant, Cle);
        end if;
    end RecupererValeur_Entier_FromCase;


    -- Recupere la valeur d'une variable par son nom a partir d'une case memoire (est appelee par RecupererValeur_Chaine)
    -- @param CaseMem : la case memoire dans laquelle on verifie si la cle correspond
    -- @param Cle : la cle de la variable dont on cherche la valeur
    -- @return : la valeur associee au nom de la variable
    -- Pre-condition : La cle existe dans la memoire
    function RecupererValeur_Chaine_FromCase(CaseMem : in P_Memoire_String.T_Case_Memoire; Cle : in Unbounded_String) return Unbounded_String
    is
    begin
        -- Verifier si la cle est la bonne pour chaque case memoire
        if Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map) = CaseMem.Cle then
            return CaseMem.All.Data;
        else
            return RecupererValeur_Chaine_FromCase(CaseMem.Suivant, Cle);
        end if;
    end RecupererValeur_Chaine_FromCase;


    -- Recupere la valeur d'une variable (de type entier ou booleen)
    -- S'il s'agit d'une variable declaree, cherche sa valeur dans la memoire
    -- S'il s'agit d'une constante declaree sans variable, cast en Integer
    -- @param Memoire : la memoire dans laquelle est stockee la variable
    -- @param Cle : le nom de la variable recherchee
    -- @return : la valeur de la variable
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

    -- Recupere la valeur d'une variable (de type chaine ou caractere)
    -- S'il s'agit d'une variable declaree, cherche sa valeur dans la memoire
    -- S'il s'agit d'une constante declaree sans variable, cast en Integer
    -- @param Memoire : la memoire dans laquelle est stockee la variable
    -- @param Cle : le nom de la variable recherchee
    -- @return : la valeur de la variable
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

    -- Retourne le type de la variable si elle est trouvee dans la partie memoire des chaines/caracteres
    -- @param Mem : la memoire
    -- @param Cle : le nom de la variable
    -- @param search_type : prend la valeur de chaque case parcourue pour chercher la variable
    -- @return : le type de la variable, ou null si elle n'est pas trouvee
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


    -- Retourne le type de la variable si elle est trouvee dans la partie memoire des entiers/booleens
    -- @param Mem : la memoire
    -- @param Cle : le nom de la variable
    -- @param search_type : prend la valeur de chaque case parcourue pour chercher la variable
    -- @return : le type de la variable, ou null si elle n'est pas trouvee
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


    -- Recupere le type d'une variable par son nom ou renvoie null si la variable n'est pas stockee en memoire (constante)
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


    -- affiche le contenu de la liste de chaine de caractère
    -- @param Mem : la case memoire a afficher
    -- @param Adresse : l'adresse de la case memoire
    procedure afficher_liste_chaine(Mem : in P_Memoire_String.T_Case_Memoire; Adresse : in out Integer) is
    begin
        if Mem.Suivant = null then
            null;
        else
            Put("        Adresse en mémoire : "); Put(Adresse, 1); Put_Line("");
            Put("        Nom de la variable : "); Put(To_String(Mem.Cle)); Put_Line("");
            Put("        Valeur de la variable : "); Put_Line(To_String(Mem.Data)); Put_Line("");
            Adresse := Adresse +1;
            afficher_liste_chaine(Mem.Suivant, Adresse);
        end if;
    end afficher_liste_chaine;

    -- affiche le contenu de la liste d'entier
    -- @param Mem : la case memoire a afficher
    -- @param Adresse : l'adresse de la case memoire
    procedure afficher_liste_entier(Mem : in P_Memoire_Entier.T_Case_Memoire; Adresse : in out Integer) is
    begin
        if Mem.Suivant = null then
            null;
        else
            Put("        Adresse en mémoire : "); Put(Adresse, 1); Put_Line("");
            Put("        Nom de la variable : "); Put(To_String(Mem.Cle)); Put_Line("");
            Put("        Valeur de la variable : "); Put(Mem.Data, 1); Put_Line(""); Put_Line("");
            Adresse := Adresse +1;
            afficher_liste_entier(Mem.Suivant, Adresse);
        end if;
    end afficher_liste_entier;

    -- affiche les variables avec leur valeur et leur position
    -- @param Mem : la memoire dans laquelle est stockee les variables
    procedure afficher_variables(Mem : in T_Memoire) is
        adresse : Integer;
    begin
        adresse := 1;
        if Mem.Entiers.Suivant /= null then
            Put_Line("    Affichage de la liste d'entier ou boolean : ");
            afficher_liste_entier(Mem.Entiers, adresse);
        end if;

        if Mem.Chaines.Suivant /= null then
            Put_Line("    Affichage de la liste de caractère ou chaine : ");
            afficher_liste_chaine(Mem.Chaines, adresse);
        end if;
    end afficher_variables;

end Memoire;
