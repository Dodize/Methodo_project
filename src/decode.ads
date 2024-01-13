-- Ce module definit les operations necessaires au decodage du code intermediaire
-- Il stock les instructions et les executes.

-- Importer le module de memoire dans .adb
-- Mettre le tableau memoire en argument
with Memoire;

generic
    with package P_Memoire is new Memoire(<>);
      Capacite: Integer;   -- Nombre de ligne du code interprete

package Decode is
    
    use P_Memoire;
   
    Type T_tab_instruc is limited private ;
    
    GOTO_OUT_OF_RANGE_EXCEPTION : Exception; -- la ligne renvoyée par GOTO est < à 1 ou > au nombre de lignes du code
   
   -- Verifie si le tableau d'instruction est vide
   function est_null(Tab : in T_tab_instruc) return Boolean;

   -- Initialiser le tableau d'instruction
   procedure init_tab_instruc (Tab : out T_tab_instruc) with
     Post => est_null(Tab);

   -- Initialise le compteur
   procedure init_CP (CP: out Integer) with
     Post => CP = 1;

   procedure increm_CP (CP : in out Integer) with
     Post => CP'Old +1 = CP;

   -- Rempli le tableau avec les instructions
   -- Coommentaire : DEVIENT NULL
   -- en mettant sous la forme voulue
   procedure remplir_tab_instruc (Tab : in out T_tab_instruc);

   --METTRE dans .adb en methode interne
	-- Recupere l'instruction dans le tableau.
	-- function recup_instru (Tab : in T_tab_instruc; CP : in Integer) return String with
	--	Post => Tab(CP).pos1 = res or Tab(CP).pos3 = res;

   -- Effectue une instruction passée en paramètre en fonction de son type
   -- utilise recup_instru ?
   -- appel instru_goto, instru_op, instru_affectation, instru_if, instru_null
	procedure effectuer_instru (Tab : in T_tab_instruc; CP : in Integer; mem : in out T_memoire);

   -- Effectue l'instruction goto en allant au label souhaite
   procedure instru_goto (CP : out Integer; label : in Integer);

   -- Effectue l'instruction operation demande
   procedure instru_op (x : in Integer; y : in Integer; op : in String; z : out Integer; mem : in out T_memoire);

   -- Effectue l'instruction operation demande
   -- si "chaine" + "5" dans le doc y aura ""5""
   procedure instru_op (x : in String; y : in String; op : in String; z : out Integer; mem : in out T_memoire);

   -- Effectue l'instruction affectation TODO A FAIRE EN GENERIQUE SUR VAL ET RAJOUTER LE TABLEAU DE MEMOIRE DEDANS
   procedure instru_affectation (val : in Integer; cle : in out String; mem : in out T_memoire);

   -- Effectue l'instruction if
   -- Integer pour X car c'est notre clef
   procedure instru_if (x : in Integer; label : in Integer; CP : out Integer; mem : in T_memoire);

   -- Effectue l'instruction null, soit ne fait rien
   procedure instru_null (CP : out Integer) with
     Post => CP'Old +1 = CP;

   -- Pour debugger : Affihe memoire CP et la memoire regroupant les valeurs des differentes variables
   procedure afficher (Tab : in T_tab_instruc; CP : in Integer; mem : in out T_memoire);

   
   -- A DEPLACER PLUS AU 
   -- Permet de choisir le mode d'utilisation
   -- 0 pour normal et 1 pour debugge
   -- function menu return Integer with
   --   Post => res = 0 or res = 1;

   -- Lance l'interpreteur en lisant et excecutant le code
   -- procedure execution;

private

   -- VIRE AU DESSUS DANS EXECUTION VU QU'ON DECLARE TAB AUSSI D'ABORD
   -- CP integer;
   
      type T_op is
      record
         pos1: String (1..25);
         pos2 : String (1..25); --si fait ca faut un 5eme champs pour cste ou variable
         pos3 : String (1..25); --on met que des int au lieu de string
         pos4 : String (1..25); --x on met l'adresse
      end record;
   
   type T_tab_instruc is array (1..Capacite) of T_op;

end Decode;
