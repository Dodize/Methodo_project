

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


-- VÈrifier si delete inclu la supp du delimiteur comme souhaitÈ
   procedure slice_mot(Ligne : in out Unbounded_String; mot : in out Unbounded_String; delimiteur : in Character) is
      Index : Natural;
   begin
      -- Trouver l'indice du dÈlimiteur
      Index := Ada.Strings.Unbounded.Index(Ligne, Character'Image(Delimiteur));

      -- Extraire la sous-chaÓne jusqu'au dÈlimiteur
      if Index /= 0 then
         Mot := Unbounded_Slice(Ligne, 1, Index);
         -- Supprimer la partie extraite de la ligne
         Delete(Ligne, 1, Index);
      else
         -- Si le dÈlimiteur n'est pas trouvÈ, copier la ligne entiËre dans le mot
         Mot := Ligne;
         -- RÈinitialiser la ligne
         Delete(Ligne, 1, Length(Ligne));
      end if;
   end slice_mot;



   -- Rempli le tableau avec les instructions du fichier
   -- en mettant sous la forme voulue
   -- Si la ligne est un coommentaire : devient un null dans le tableau
   -- @param Tab : tableau a remplir
   -- @param Fichier : code source a utiliser pour remplir le tableau
   procedure remplir_tab_instruc (Tab : in out T_tab_instruc; Fichier : in File_Type) is
      Ligne : Ada.Strings.Unbounded.Unbounded_String;
      Mot : Ada.Strings.Unbounded.Unbounded_String;
   begin
      -- Parcours jusqu'a debut pour remplir uniquement les lignes de codes en ignorant la decla de variable
      while To_String(Ligne) /= "Debut" loop
         Ligne := To_Unbounded_String(Get_Line(Fichier));
      end loop;

      -- Rempli le tableau avec les instructions
      while not End_Of_File (Fichier) loop
         Ligne := To_Unbounded_String(Get_Line(Fichier));
         --SPLITTER LA LIGNE (avec ligne qu'on raccourci et



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


   --effectuer_instru : appel instru_goto, instru_op, instru_affectation, instru_if, instru_null

   -- Effectue l'instruction goto en allant au label souhaite
    procedure instru_goto (CP : out Integer; label : in Integer) is
    begin
        Null;
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


   -- Effectue l'instruction if
   -- Integer pour X car c'est notre clef
    procedure instru_if (VariableBool : in String; Label : in Integer; CP : out Integer; mem : in T_memoire) is
    begin
        Null;
    end instru_if;


   -- Effectue l'instruction null, soit ne fait rien
   procedure instru_null is
    begin
        Null;
   end instru_null;

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
        -- en fonction du premier mot de l'instruction, effectuer la bonne op√©ration
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
