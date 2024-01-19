package body Memoire is

   -- Declare toutes les variables en memoire
   -- @param Mem : la memoire
   -- @param Code : le code ou sont declarees les variables
   procedure DeclarerVariables (Mem : out T_Memoire; Code : in File_Type) is
      Current_Mem_Integer : P_Memoire_Entier.T_Case_Memoire;
      Current_Line        : Unbounded_String;
      Temporary_Line      : Unbounded_String;
      Splitted_Line       : Unbounded_String;
      Fini                : Boolean;
      Current_Type        : T_Type;
   begin
      Fini                := False;
      -- Tant qu'on n'a pas atteint la fin du fichier ni la ligne "Debut"
      while not End_Of_File (Code) and not Fini loop
         Current_Line := To_Unbounded_String (Get_Line (Code));
         Current_Line :=
           Translate (Current_Line, Ada.Strings.Maps.Constants.Lower_Case_Map);
         -- Si pas commentaire
         if Index (Current_Line, "--") = 0 then
            -- Verifier qu'on n'a pas atteint la ligne "Debut"
            if
              (Index (Temporary_Line, "d�but") > 0 or
               Index (Temporary_Line, "debut") > 0) and
              Index (Temporary_Line, ":") = 0
            then
               Fini := True;
            else
               -- Split au niveau de ':'
               slice_mot (Current_Line, Splitted_Line, ':');
               -- Recuperation du type
               if Index (Splitted_Line, "entier") > 0 then
                  Current_Type := ENTIER;
               end if;
               slice_mot (Current_Line, Splitted_Line, ',');
               while Length (Current_Line) > 0 loop
                  if Current_Type = ENTIER then
                     P_Memoire_Entier.Initialiser (Current_Mem_Integer);
                     Current_Mem_Integer.Cle        := Splitted_Line;
                     Current_Mem_Integer.TypeOfData := P_Memoire_Entier.ENTIER;

                     if Mem.Entiers = null then
                        Mem.Entiers := Current_Mem_Integer;
                     end if;

                     Current_Mem_Integer := Current_Mem_Integer.Suivant;
                  end if;
                  -- Split au niveau de ','
                  slice_mot (Current_Line, Splitted_Line, ',');
               end loop;
            end if;
         end if;
      end loop;
   end DeclarerVariables;

   -- Modifie la donnee d'une variable existante en memoire
   -- @param Mem : la memoire modifiee
   -- @param Cle : le nom de la variable a modifier
   -- @param Data : la nouvelle valeur de la variable
   procedure Modifier_Entier (Mem : in out T_Memoire; Cle : in Unbounded_String; Data : in Integer)
   is
   begin
      null;
   end Modifier_Entier;


    -- Recupere la valeur d'une variable par son nom à partir d'une case memoire (est appelée par RecupererValeur_Entier)
    -- @param CaseMem : la case memoire dans laquelle on verifie si la cle correspond
    -- @param Cle : la cle de la variable dont on cherche la valeur
    -- @return : la valeur associee au nom de la variable
    function RecupererValeur_Entier_FromCase(CaseMem : in P_Memoire_Entier.T_Case_Memoire; Cle : in Unbounded_String) return Integer
    is
    begin
        -- R1 : Recuperer la premiere case memoire
        if CaseMem.All.Cle = Cle then
            return CaseMem.All.Data;
        else
            return RecupererValeur_Entier_FromCase(CaseMem.Suivant, Cle); -- on sait que la clé existe donc si elle n'a pas encore été trouvée
            -- alors elle est dans la case suivante (pas la peine de tester si Suivant /= Null)
        end if;
    end RecupererValeur_Entier_FromCase;


   -- Recupere la valeur d'une variable par son nom à partir de l'objet memoire
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
   -- @return la valeur de la variable
   -- Pre-condition : Mem.Entiers /= Null et la clé existe dans la mémoire (car le programme intermédiaire est bien formé)
   function RecupererValeur_Entier (Mem : in T_Memoire; Cle : in Unbounded_String) return Integer
    is
    begin
        return RecupererValeur_Entier_FromCase(Mem.Entiers, Cle);
   end RecupererValeur_Entier;

   -- Recupere le type d'une variable par son nom
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
   -- @return le type de la variable
   function RecupererType (Mem : in T_Memoire; Cle : in Unbounded_String) return Unbounded_String is
      Pos_entier : P_Memoire_Entier.T_Case_Memoire;
      Pos_string : P_Memoire_String.T_Case_Memoire;
   begin
      Pos_entier := Mem.Entiers;
      -- Recherche si la cle est un entier
      while (Pos_entier /= null) or else (Cle /= Pos_entier.Cle) loop
         Pos_entier := Pos_entier.Suivant;
      end loop;
      if Pos_entier /= null then
         return To_Unbounded_String("Entier");
      end if;
      Pos_string := Mem.Chaines;
      while (Pos_string /= null) or else (Cle /= Pos_string.Cle) loop
         Pos_string := Pos_string.Suivant;
      end loop;
      if Pos_string /= null then
         return To_Unbounded_String("Chaine");
      end if;
      return To_Unbounded_String("null");
   end RecupererType;

end Memoire;
