procedure Decode is

-- Importer le module de memoire dans .adb
-- Mettre le tableau memoire en argument

begin
   -- Methode interne
	-- Recupere l'instruction dans le tableau.
	-- function recup_instru (Tab : in T_tab_instruc; CP : in Integer) return String with
 --	Post => Tab(CP).pos1 = res or Tab(CP).pos3 = res;

   --effectuer_instru : appel instru_goto, instru_op, instru_affectation, instru_if, instru_null


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
   null;
end Decode;
