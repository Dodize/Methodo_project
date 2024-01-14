package body Decode is

-- Importer le module de memoire dans .adb
-- Mettre le tableau memoire en argument


    -- Verifie si le tableau d'instruction est vide
   -- @param Tab : tableau d'instruction à comparer
    function est_null(Tab : in T_tab_instruc) return Boolean is
    begin
        return True;
    end est_null;


   -- Initialiser le tableau d'instruction
   -- @param Tab: tableau d'instruction a initialiser
   procedure init_tab_instruc (Tab : out T_tab_instruc) is
    begin
        Null;
    end init_tab_instruc;


   -- Initialise le compteur
   -- @param CP : compteur a initialiser
   procedure init_CP (CP: out Integer) is
    begin
        Null;
    end init_CP;


   -- Incrémente le compteur
   -- @param CP : compteur a incrementer
   procedure increm_CP (CP : in out Integer) is
    begin
        Null;
    end increm_CP;


   -- Rempli le tableau avec les instructions du fichier
   -- en mettant sous la forme voulue
   -- Si la ligne est un coommentaire : devient un null dans le tableau
   -- @param Tab : tableau a remplir
   -- @param Fichier : code source a utiliser pour remplir le tableau
    procedure remplir_tab_instruc (Tab : in out T_tab_instruc; Fichier : in File_Type) is
    begin
        Null;
    end remplir_tab_instruc;


   -- Effectue une instruction passee en parametre en fonction de son type (GOTO, null, if, op)
   -- @param Tab : tableau comptenant les instructions
   -- @param CP : compteur
   -- @param mem : liste chainee comptenant les variables et leurs valeurs
    procedure effectuer_instru (Tab : in T_tab_instruc; CP : in Integer; mem : in out T_memoire) is
    begin
        Null;
    end effectuer_instru;


   -- Pour debugger : Affihe memoire CP et la memoire regroupant les valeurs des differentes variables
   -- @param Tab : tableau comptenant les instructions
   -- @param CP : compteur
   -- @param mem : liste chainee comptenant les variables et leurs valeurs
    procedure afficher (Tab : in T_tab_instruc; CP : in Integer; mem : in out T_memoire) is
    begin
        Null;
    end afficher;



   -- Methode interne
	-- Recupere l'instruction dans le tableau.
	-- function recup_instru (Tab : in T_tab_instruc; CP : in Integer) return String with
 --	Post => Tab(CP).pos1 = res or Tab(CP).pos3 = res;

   --effectuer_instru : appel instru_goto, instru_op, instru_affectation, instru_if, instru_null


   -- Effectue l'instruction goto en allant au label souhaite
    procedure instru_goto (CP : out Integer; label : in Integer) is
    begin
        Null;
    end instru_goto;


   -- Effectue l'instruction operation demande
    procedure instru_op (x : in Integer; y : in Integer; op : in String; z : out Integer; mem : in out T_memoire) is
    begin
        Null;
    end instru_op;


   -- Effectue l'instruction operation demande
   -- si "chaine" + "5" dans le doc y aura ""5""
    procedure instru_op (x : in String; y : in String; op : in String; z : out Integer; mem : in out T_memoire) is
    begin
        Null;
    end instru_op;


   -- Effectue l'instruction affectation TODO A FAIRE EN GENERIQUE SUR VAL ET RAJOUTER LE TABLEAU DE MEMOIRE DEDANS
    procedure instru_affectation (val : in Integer; cle : in out String; mem : in out T_memoire) is
    begin
        Null;
    end instru_affectation;


   -- Effectue l'instruction if
   -- Integer pour X car c'est notre clef
    procedure instru_if (x : in Integer; label : in Integer; CP : out Integer; mem : in T_memoire) is
    begin
        Null;
    end instru_if;


   -- Effectue l'instruction null, soit ne fait rien
   procedure instru_null (CP : out Integer) with
            Post => CP'Old +1 = CP is
    begin
        Null;
   end instru_null;

   -- Pour debugger : retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   function recuperer_instru_pos1 (Tab : in T_tab_instruc; CP : in Integer) return String is
   begin
      return "";
   end recuperer_instru_pos1;

   -- Pour debugger : retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   function recuperer_instru_pos2 (Tab : in T_tab_instruc; CP : in Integer) return String is
   begin
      return "";
   end recuperer_instru_pos2;

   -- Pour debugger : retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   function recuperer_instru_pos3 (Tab : in T_tab_instruc; CP : in Integer) return String is
   begin
      return "";
   end recuperer_instru_pos3;

   -- Pour debugger : retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   function recuperer_instru_pos4 (Tab : in T_tab_instruc; CP : in Integer) return String is
   begin
      return "";
   end recuperer_instru_pos4;

end Decode;
