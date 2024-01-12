-- Ce module definit les operations necessaires au decodage des instructions
-- ainsi qu'a leurs executions.

-- Importer le module de mémoire

package Decode is

   Type T_tab_instruc is limited private ;
   Limited private CP integer;
   Capacite: constant Integer := 70;   -- Cette taille est arbitraire POUR LE MOMENT

   -- Initialiser le tableau d'instruction
   procedure init_tab_instruc (Tab : out T_tab_instruc) with
     Post Tab = null;

   -- Initialise le compteur
   procedure init_CP (CP: out Integer) with
     Post => CP = 1;

   procedure increm_CP (CP : in out Integer) with
     Post => CP'Old +1 = CP;

   -- Rempli le tableau avec les instructions
   -- en ignorant les commentaires ou pas de commentaires ?
   -- en mettant sous la forme voulue
   procedure remplir_tab_instruc (Tab : in out T_tab_instruc);

   --Mettre dans .adb en methode interne ???
	-- Recupere l'instruction dans le tableau.
	function recup_instru (Tab : in T_tab_instruc, CP : in Integer) return String with
		Post => Tab(CP).pos1 = res or Tab(CP).pos3 = res;

   -- Effectue une instruction passée en paramètre en fonction de son type
   -- utilise recup_instru ?
   -- appel instru_goto, instru_op, instru_affectation, instru_if, instru_null
	procedure effectuer_instru (Tab : in T_tab_instruc, CP : in Integer);

   -- Effectue l'instruction goto en allant au label souhaite
   procedure instru_goto (CP : out Integer, label : in Integer);

   -- Effectue l'instruction operation demande
   procedure instru_op (x : in Integer, y : in Integer, op : in String, z : out Integer);

   -- Effectue l'instruction operation demande
   -- si "chaine" + "5" dans le doc y aura ""5""
   procedure instru_op (x : in String, y : in String, op : in String, z : out Integer);

   -- Effectue l'instruction affectation TODO A FAIRE EN GENERIQUE SUR VAL ET RAJOUTER LE TABLEAU DE MEMOIRE DEDANS
   procedure instru_affectation (val : in Integer, cle : in out String);

   -- Effectue l'instruction if
   -- Integer pour X car c'est notre clef
   procedure instru_if (x : in Integer, label : in int, CP : out Integer);

   -- Effectue l'instruction null, soit ne fait rien
   procedure instru_null (CP : out Integer) with
     Post CP'Old +1 = CP;

   -- Pour debugger : Affihe mémoire CP et la mémoire regroupant les valeurs des différentes variables
   procedure afficher (Tab : in T_tab_instruc, CP : in Integer);

   -- Permet de choisir le mode d'utilisation
   -- 0 pour normal et 1 pour debugge
   function menu () return Integer with
     Post res = 0 or res = 1;

   -- Lance l'interpreteur en lisant et excecutant le code
   procedure execution ();

private

   type T_tab_instruc is array (1..Capacite) of T_op;

   type T_op is
      record
         pos1: String;
         pos2 : String; --si fait ça faut un 5eme champs pour cste ou variable
         pos3 : String; --on met que des int au lieu de string
         pos4 : String; --x on met l'adresse
      end record;

end Decode;
