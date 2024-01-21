
with Utils; use Utils;


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


    -- Effectue l'instruction goto en allant au label souhaite (sous forme de numero de ligne)
    -- @param CP : le compteur qui doit etre modifie
    -- @param label : le numero de la ligne a laquelle on souhaite aller
    -- @exception GOTO_OUT_OF_RANGE_EXCEPTION : si le label ne correspond pas a une ligne valide
    -- (la ligne est < 1 ou > au nombre d'instructions)
   procedure instru_goto (CP : out Integer; label : in Integer; Tab : in T_tab_instruc) is
    begin
        if (label < 1) or else (label > Tab'Length) then
            raise GOTO_OUT_OF_RANGE_EXCEPTION;
        else
            CP := label;
        end if;

    end instru_goto;


    -- Calcule le resultat d'une operation composee de deux entiers
    -- @param CleVal1 : le nom de la premiere variable de l'operation
    -- @param Operation : l'operation a effectuer
    -- @param CleVal2 : le nom de la deuxieme variable de l'operation
    -- @param Memoire : la memoire
    -- @return : le resultat de l'operation
    function result_instru_entier(CleVal1 : in Unbounded_String; Operation : in Unbounded_String; CleVal2 : in Unbounded_String; Memoire : in out T_memoire) return Integer is
        Result : Integer;
        Valeur1 : Integer;
        Valeur2 : Integer;
    begin
        Valeur1 := RecupererValeur_Entier(Memoire, CleVal1);
        Valeur2 := RecupererValeur_Entier(Memoire, CleVal2);
        if Operation = "+" then
            Result := Valeur1 + Valeur2;
        elsif Operation = "-" then
            Result := Valeur1 - Valeur2;
        elsif Operation = "/" then
            Result := Valeur1 / Valeur2;
        elsif Operation = "*" then
            Result := Valeur1 * Valeur2;
        elsif Operation = "=" then
            if Valeur1 = Valeur2 then
                Result := 1;
            else
                Result := 0;
            end if;
        elsif Operation = "<" then
            if Valeur1 < Valeur2 then
                Result := 1;
            else
                Result := 0;
            end if;
        elsif Operation = ">" then
            if Valeur1 > Valeur2 then
                Result := 1;
            else
                Result := 0;
            end if;
        elsif Operation = "OR" then
            if Valeur1/=0 or else Valeur2/=0 then
                Result := 1;
            else
                Result := 0;
            end if;
        elsif Operation = "AND" then
            if Valeur1/=0 and then Valeur2/=0 then
                Result := 1;
            else
                Result := 0;
            end if;
        else
            -- exception
            Null;
        end if;

        return Result;

    end result_instru_entier;



    -- Effectue l'instruction operation demande
    -- @param CleVariableAffectation : le nom de la variable affectee
    -- @param Valeur1 : le nom de la premiere variable de l'operation
    -- @param Operation : le type d'operation a realiser (en chaine)
    -- @param Valeur2 : le nom de la deuxieme variable de l'operation
    -- @param Memoire : la memoire
   procedure instru_op (CleVariableAffectation : in Unbounded_String; Valeur1 : in Unbounded_String; Operation : in Unbounded_String; Valeur2 : in Unbounded_String; Memoire : in out T_memoire; CP: in out Integer) is
   begin
      if RecupererType(Memoire, CleVariableAffectation) = "Entier" then
         Modifier_Entier(Memoire, CleVariableAffectation, result_instru_entier(Valeur1, Operation, Valeur2, Memoire));
      else
         Null; --TODO quand on aura les autres types
      end if;
      increm_CP(CP);
   end instru_op;



    -- Effectue l'instruction affectation
    -- @param CleVariable : le nom de la variable a modifier
    -- @param Valeur : la nouvelle valeur de la variable (recuperee en string dans le tableau d'instruction)
    -- @param Mem : la memoire
   procedure instru_affectation (CleVariable : in Unbounded_String; Valeur : in Unbounded_String; Mem : in out T_memoire; CP : in out Integer) is
   begin
      if RecupererType(Mem, CleVariable) = "Entier" then
         Modifier_Entier(Mem, CleVariable, Integer'Value(To_String(Valeur)));
      else
         Null; --TODO quand on aura les autres types
      end if;
      increm_CP(CP);
   end instru_affectation;


     --  Effectue l'instruction if en fonction de la variable VariableBool
     --  @param VariableBool : le nom de la variable booleenne
     --  @param Label : la valeur que doit prendre CP si le booleen vaut True
     --  @param CP : le compteur de la ligne courante
     --  @param mem : la memoire
    procedure instru_if (VariableBool : in Unbounded_String; Label : in Integer; CP : in out Integer; mem : in T_memoire) is
        valeur : Integer; -- valeur de "VariableBool"
    begin
        -- Recuperer la valeur du booleen
        valeur := RecupererValeur_Entier(mem, VariableBool);
        if valeur = 0 then -- cas ou la valeur vaut false : on ne fait rien
            increm_CP(CP);
        else -- cas ou la valeur vaut true : on change la valeur de CP
            CP := Label;
        end if;
   end instru_if;


   -- Effectue l'instruction null, soit ne fait rien
   procedure instru_null(CP : in out Integer) is
   begin
      increm_CP(CP);
   end instru_null;

   -- Rempli une ligne du tableau en mettant en forme l'instruction null ou un commentaire
   -- @param Tab : tableau a remplir
   -- @param Index : ligne du tableau a remplir
   procedure remplir_ligne_null(Tab: out T_tab_instruc; Pos : in Integer) is
   begin
      Tab(Pos).pos1 := To_Unbounded_String("NULL");
   end remplir_ligne_null;

   -- Rempli une ligne du tableau en mettant en forme l'instruction
   -- @param Tab : tableau a remplir
   -- @param Index : ligne du tableau a remplir
   -- @param Ligne : ligne dont on recupere les informations
   -- @param Mot : premier mot de la ligne
   procedure remplir_ligne(Tab: out T_tab_instruc; Pos : in Integer; Ligne: in out Unbounded_String; Mot : in Unbounded_String) is
   begin
      Tab(Pos).pos1 := Mot;
      slice_mot(Ligne, Tab(Pos).pos2, " ");
      slice_mot(Ligne, Tab(Pos).pos3, " ");
      slice_mot(Ligne, Tab(Pos).pos4, " ");
   end remplir_ligne;

   -- Rempli une ligne du tableau en mettant en forme l'instru x y op z
   -- @param Tab : tableau a remplir
   -- @param Pos : ligne du tableau a remplir
   -- @param Ligne : ligne dont on recupere les informations
   -- @param Mot : premier mot de la ligne
   procedure remplir_ligne_op(Tab: out T_tab_instruc; Pos : in Integer; Ligne: in out Unbounded_String; Mot : in Unbounded_String) is
      inutile : Unbounded_String;
   begin
      Tab(Pos).pos1 := Mot;
      slice_mot(Ligne, inutile, " ");
      slice_mot(Ligne, Tab(Pos).pos2, " ");
      if index(Ligne, " ") = 0 then
         Tab(Pos).pos3 := To_Unbounded_String("affect");
      else
        slice_mot(Ligne, Tab(Pos).pos3, " ");
         slice_mot(Ligne, Tab(Pos).pos4, " ");
      end if;
   end remplir_ligne_op;

   -- Rempli le tableau avec les instructions du fichier
   -- en mettant sous la forme voulue
   -- Si la ligne est un coommentaire : devient un null dans le tableau
   -- @param Tab : tableau a remplir
   -- @param NomFichier : le nom du fichier contenant code source a utiliser pour remplir le tableau
   procedure remplir_tab_instruc (Tab : in out T_tab_instruc; NomFichier : in String) is
      Ligne : Unbounded_String;
      Mot : Unbounded_String;
      Pos : Integer;
      Fichier : File_Type;
   begin
      Ouvrir_Fichier_Lecture(NomFichier, Fichier);
      Pos := 1;
      parcourir_debut(Fichier);
      -- Parcours le code en remplicant le tableau
      Ligne := To_Unbounded_String(Get_Line(Fichier));
      while not (Ligne = "Fin") loop
         slice_mot(Ligne, Mot, " ");
         if Mot = "--" or Ligne = "NULL" then
            remplir_ligne_null(Tab, Pos);
         elsif Mot /= "GOTO" and Mot /= "--" and Mot /= "NULL" and Mot /= "IF" then -- pour op car traitement special
            remplir_ligne_op(Tab, Pos, Ligne, Mot);
         else
            remplir_ligne(Tab, Pos, Ligne, Mot);
         end if;
         Pos := Pos + 1;
         Ligne := To_Unbounded_String(Get_Line(Fichier));
        end loop;
        Close(Fichier);
   end remplir_tab_instruc;


   -- Pour debugger : Affihe memoire CP et la memoire regroupant les valeurs des differentes variables
   -- @param Tab : tableau comptenant les instructions
   -- @param CP : compteur
   -- @param mem : liste chainee comptenant les variables et leurs valeurs
    procedure afficher (Tab : in T_tab_instruc; CP : in Integer; mem : in out T_memoire) is
    begin
        Null;
    end afficher;



   -- Retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   -- @return : la partie 1 de l'instruction
   function recuperer_instru_pos1 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String is
   begin
      return Tab(CP).pos1;
   end recuperer_instru_pos1;

   -- Retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   -- @return : la partie 2 de l'instruction
   function recuperer_instru_pos2 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String is
   begin
      return Tab(CP).pos2;
   end recuperer_instru_pos2;

   -- Retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   -- @return : la partie 3 de l'instruction
   function recuperer_instru_pos3 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String is
   begin
      return Tab(CP).pos3;
   end recuperer_instru_pos3;

   -- Retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   -- @return : la partie 4 de l'instruction
   function recuperer_instru_pos4 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String is
   begin
      return Tab(CP).pos4;
    end recuperer_instru_pos4;


   -- Effectue une instruction passee en parametre en fonction de son type (GOTO, null, if, op)
   -- Et modifie le CP en consequence
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
      -- en fonction du premier mot de l'instruction, effectuer la bonne operation
      if InstruPart1 = "NULL" then
         instru_null(CP);
      elsif InstruPart1 = "GOTO" then
         instru_goto(CP, Integer'Value(To_String(InstruPart2)), Tab);
      elsif InstruPart1 = "IF" then
         instru_if(InstruPart2, Integer'Value(To_String(InstruPart4)), CP, mem);
      else
         -- affectation sinon operation
         if InstruPart3 = "affect" then
            instru_affectation(InstruPart1, InstruPart2, mem, CP);
         else
            instru_op(InstruPart1, InstruPart2, InstruPart3, InstruPart4, mem, CP);
         end if;
      end if;

    end effectuer_instru;



end Decode;
