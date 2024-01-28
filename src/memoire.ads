-- Ce module definit les operations necessaires a la gestion de la memoire
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Case_Memoire;

package Memoire is

    package P_Memoire_Entier is new Case_Memoire(T_Elt => Integer);
    package P_Memoire_String is new Case_Memoire(T_Elt => Unbounded_String);
    use P_Memoire_Entier;
    use P_Memoire_String;

    -- Enumerable du type de la variable
    type T_Type is (CHAINE, ENTIER, TAB_ENTIER, TAB_CHAINE);

    type T_Memoire is
        record
            -- represente la liste des cases memoires contenant des types entiers,
            -- booleens ou des tableaux d'entiers/de booleens
            Entiers : P_Memoire_Entier.T_Case_Memoire;
            -- represente la liste des cases memoires contenant des types chaines,
            -- caracteres ou des tableaux de chaines/caracteres
            Chaines : P_Memoire_String.T_Case_Memoire;
        end record;

    -- Declare toutes les variables en memoire a partir du fichier d'instructions
    -- @param Mem : la memoire
    -- @param NomFichierCode : le nom du fichier contenant le code ou sont declarees les variables
    -- @exception ADA.IO_EXCEPTIONS.NAME_ERROR : si le fichier ayant pour nom "NomFichier" n'existe pas ou n'est pas accessible par le programme
    procedure DeclarerVariables(Mem : out T_Memoire ; NomFichierCode : in String);

    -- Modifie la donnee d'une variable existante (mais pas forcement initialisee) de type entier/booleen
    -- @param Mem : la memoire contenant la variable a modifier
    -- @param Cle : le nom de la variable a modifier
    -- @param Data : la nouvelle valeur de la variable
    -- Precondition : la liste des cases memoires contient une case memoire ayant pour Cle la Cle donnee en parametre
    procedure Modifier_Entier(Mem : in out T_Memoire ; Cle : in Unbounded_String ; Data : in Integer) with
            Post => RecupererValeur_Entier(Mem, Cle) = Data;

    -- Modifie la donnee d'une variable existante en memoire (mais pas forcement initialisee) de type chaine/caractere
    -- @param Mem : la memoire contenant la variable a modifier
    -- @param Cle : le nom de la variable a modifier
    -- @param Data : la nouvelle valeur de la variable
    procedure Modifier_Chaine(Mem : in out T_Memoire ; Cle : in Unbounded_String ; Data : in Unbounded_String) with
            Post => RecupererValeur_Chaine(Mem, Cle) = Data;

    -- Recupere la valeur d'une variable (de type entier ou booleen) par son nom
    -- @param Memoire : la memoire dans laquelle est stockee la variable
    -- @param Cle : le nom de la variable recherchee
    -- @return : la valeur de la variable
    function RecupererValeur_Entier(Memoire : in T_Memoire ; Cle : in Unbounded_String) return Integer with
            Pre => Memoire.Entiers /= Null;

    -- Recupere la valeur d'une variable (de type chaine ou caractere) par son nom
    -- @param Mem : la memoire dans laquelle est stockee la variable
    -- @param Cle : le nom de la variable recherchee
    -- @return : la valeur de la variable
    function RecupererValeur_Chaine(Memoire : in T_Memoire ; Cle : in Unbounded_String) return Unbounded_String with
            Pre => Memoire.Entiers /= Null;

    -- Recupere le type d'une variable par son nom ou renvoie null si la variable n'est pas stockee en memoire (constante)
    -- @param Mem : la memoire dans laquelle est stockee la variable
    -- @param Cle : le nom de la variable recherchee
    -- @return : le type de la variable
    function RecupererType(Mem : in T_Memoire ; Cle : in Unbounded_String) return Unbounded_String;

    -- affiche les variables avec leur valeur et leur position
    -- @param Mem : la memoire dans laquelle est stockee les variables
    procedure afficher_variables(Mem : in T_Memoire);

end Memoire;
