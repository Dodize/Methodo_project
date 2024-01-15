package body Memoire is

   -- Initialise une case memoire. La case memoire est vide
   -- @param Mem : la memoire a initialiser
    procedure Initialiser(Mem : out T_Memoire) is
    begin
        Null;
    end Initialiser;

   -- Declare toutes les variables en memoire
   -- @param Mem : la memoire modifiee
   -- @param Code : le code où sont déclarées les variables
    procedure DeclarerVariables(Mem : in out T_Memoire ; Code : in String) is
    begin
        Null;
    end DeclarerVariables;


   -- Modifie la donnee d'une variable existante en memoire
   -- @param Mem : la memoire modifiee
   -- @param Cle : le nom de la variable a modifier
   -- @param Data : la nouvelle valeur de la variable
    procedure Modifier(Mem : in out T_Memoire ; Cle : in String ; Data : in T_Elt) is
    begin
        Null;
    end Modifier;


   -- Recupere la valeur d'une variable par son nom
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
    function RecupererValeur(Mem : in T_Memoire ; Cle : in String) return T_Elt is
    begin
        return Mem.all.Data; -- A CHANGER !!
    end RecupererValeur;


   -- Recupere le type d'une variable par son nom
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
    function RecupererType(Mem : in T_Memoire ; Cle : in String) return T_Type is
    begin
        return CHAINE;
    end RecupererType;

end Memoire;
