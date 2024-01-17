

package body Decode is

-- Importer le module de memoire dans .adb

   -- Initialise le compteur
   -- @param CP : compteur a initialiser
   procedure init_CP (CP: out Integer) is
   begin
      CP := 1;
   end init_CP;


   -- Incremente le compteur
   -- @param CP : compteur a incrementer
   procedure increm_CP (CP : in out Integer) is
   begin
      CP := CP+1;
   end increm_CP;

   --effectuer_instru : appel instru_goto, instru_op, instru_affectation, instru_if, instru_null

    -- Effectue l'instruction goto en allant au label souhaite (sous forme de numero de ligne)
    -- @param CP : le compteur qui doit etre modifie
    -- @param label : le numero de la ligne a laquelle on souhaite aller
   procedure instru_goto (CP : out Integer; label : in Integer) is
   begin
      CP := label;
   end instru_goto;


   -- Effectue l'instruction operation demande
   procedure instru_op (x : in Integer; y : in Integer; op : in String; z : in Integer; mem : in out T_memoire) is
   begin
      Null;
   end instru_op;


   -- Effectue l'instruction operation demande (attention X pas forcement meme type que y et z)
   -- si "chaine" + "5" dans le doc y aura ""5""
   --  procedure instru_op (x : in String; y : in String; op : in String; z : out Integer; mem : in out T_memoire) is
   --  begin
   --      Null;
   --  end instru_op;


   -- Effectue l'instruction affectation TODO A FAIRE EN GENERIQUE SUR VAL ET RAJOUTER LE TABLEAU DE MEMOIRE DEDANS
   procedure instru_affectation (cle : in String; val : in Integer; mem : in out T_memoire) is
   begin
      Null;
   end instru_affectation;


    -- Effectue l'instruction if en fonction de la variable VariableBool
    -- @param VariableBool : le nom de la variable booleenne
    -- @param Label : la valeur que doit prendre CP si le booleen vaut True
    -- @param CP : le compteur de la ligne courante
    -- @param mem : la memoire
    procedure instru_if (VariableBool : in String; Label : in Integer; CP : out Integer; mem : in T_memoire) is
        valeur : Integer; -- valeur de "VariableBool"
    begin
        -- Recuperer la valeur du booleen
        valeur := RecupererValeur(mem, VariableBool);
        if valeur = 0 then -- cas ou la valeur vaut false : on ne fait rien
            Null;
        else -- cas ou la valeur vaut true : on change la valeur de CP
            CP := Label;
        end if;
    end instru_if;



   -- Effectue l'instruction null, soit ne fait rien
   procedure instru_null is
   begin
      Null;
   end instru_null;


   -- Vérifier si delete inclu la supp du delimiteur comme souhaité
   -- permet de retirer le premier mot avant délimiteur de la ligne en le plaçant dans mot
   -- @param Ligne : ligne dont on extrait le mot
   -- @param Mot : où placer le mot extrait
   -- @param Delimiteur: caractère signant la fin du mot
   procedure slice_mot(Ligne : in out Unbounded_String; Mot : out Unbounded_String; Delimiteur : in Character) is
      Index : Natural;
   begin
      -- Trouver l'indice du délimiteur
      Index := Ada.Strings.Unbounded.Index(Ligne, Character'Image(Delimiteur));
      -- Extraire la sous-chaîne jusqu'au délimiteur
      if Index /= 0 then
         Mot := Unbounded_Slice(Ligne, 1, Index);
         -- Supprimer la partie extraite de la ligne
         Delete(Ligne, 1, Index);
      else
         -- Si le délimiteur n'est pas trouvé, copier la ligne entière dans le mot
         Mot := Ligne;
         -- Réinitialiser la ligne
         Delete(Ligne, 1, Length(Ligne));
      end if;
   end slice_mot;

   -- Rempli une ligne du tableau en mettant en forme l'instruction null ou un commentaire
   -- @param Tab : tableau a remplir
   -- @param Index : ligne du tableau a remplir
   procedure remplir_ligne_null(Tab: out T_tab_instruc; Index : in Integer) is
   begin
      Tab(Index).pos1 := To_Unbounded_String("null");
   end remplir_ligne_null;

   -- Rempli une ligne du tableau en mettant en forme l'instruction
   -- @param Tab : tableau a remplir
   -- @param Index : ligne du tableau a remplir
   -- @param Ligne : ligne dont on recupere les informations
   -- @param Mot : premier mot de la ligne
   procedure remplir_ligne(Tab: out T_tab_instruc; Index : in Integer; Ligne: in out Unbounded_String; Mot : in Unbounded_String) is
   begin
      Tab(Index).pos1 := Mot;
      slice_mot(Ligne, Tab(Index).pos2, ' ');
      slice_mot(Ligne, Tab(Index).pos3, ' ');
      slice_mot(Ligne, Tab(Index).pos4, ' ');
   end remplir_ligne;

   -- Rempli le tableau avec les instructions du fichier
   -- en mettant sous la forme voulue
   -- Si la ligne est un coommentaire : devient un null dans le tableau
   -- @param Tab : tableau a remplir
   -- @param Fichier : code source a utiliser pour remplir le tableau
   procedure remplir_tab_instruc (Tab : in out T_tab_instruc; Fichier : in File_Type) is
      Ligne : Ada.Strings.Unbounded.Unbounded_String;
      Mot : Ada.Strings.Unbounded.Unbounded_String;
      Index : Integer;
   begin
      Index := 1;
      -- Parcours jusqu'a debut pour remplir uniquement les lignes de codes en ignorant la decla de variable
      while To_String(Ligne) /= "Debut" loop
         Ligne := To_Unbounded_String(Get_Line(Fichier));
      end loop;

      -- Rempli le tableau avec les instructions
      while not End_Of_File (Fichier) loop
         Ligne := To_Unbounded_String(Get_Line(Fichier));
         slice_mot(Ligne, Mot, ' ');
         if Mot = "--" or Ligne = "null" then
            remplir_ligne_null(Tab, Index);
         else
            remplir_ligne(Tab, Index, Ligne, Mot);
         end if;
         Index := Index + 1;
      end loop;

   end remplir_tab_instruc;


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
     --Post => Tab(CP).pos1 = res or Tab(CP).pos3 = res;



   -- Retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   function recuperer_instru_pos1 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String is
   begin
      return Tab(CP).pos1;
   end recuperer_instru_pos1;

   -- Retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   function recuperer_instru_pos2 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String is
   begin
      return Tab(CP).pos2;
   end recuperer_instru_pos2;

   -- Retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   function recuperer_instru_pos3 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String is
   begin
      return Tab(CP).pos3;
   end recuperer_instru_pos3;

   -- Retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   function recuperer_instru_pos4 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String is
   begin
      return Tab(CP).pos4;
    end recuperer_instru_pos4;


     -- Effectue une instruction passee en parametre en fonction de son type (GOTO, null, if, op)
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
        -- en fonction du premier mot de l'instruction, effectuer la bonne opÃ©ration
        if InstruPart1 = "Null" then
            instru_null;
        elsif InstruPart1 = "GOTO" then
            instru_goto(CP, Integer'Value(To_String(InstruPart2)));
        elsif InstruPart1 = "IF" then
            instru_if(To_String(InstruPart2), Integer'Value(To_String(InstruPart4)), CP, mem);
        else
            -- affectation sinon operation
            if InstruPart3 = "affect" then
                instru_affectation(To_String(InstruPart1), Integer'Value(To_String(InstruPart2)), mem);
            else
                instru_op(Integer'Value(To_String(InstruPart1)), Integer'Value(To_String(InstruPart2)), To_String(InstruPart3), Integer'Value(To_String(InstruPart4)), mem);
            end if;
        end if;

    end effectuer_instru;



end Decode;
