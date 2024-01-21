with Ada.Strings.Maps.Constants; use Ada.Strings.Maps;
with Utils;                      use Utils;

package body Memoire is

   -- Declare toutes les variables en memoire
   -- @param Mem : la memoire
   -- @param NomFichierCode : le nom du fichier du code ou sont declarees les variables
   procedure DeclarerVariables (Mem : out T_Memoire; NomFichierCode : in String) is
      Current_Mem_Integer : P_Memoire_Entier.T_Case_Memoire;
      Current_Line        : Unbounded_String;
      Splitted_Line       : Unbounded_String;
      Fini                : Boolean;
      Current_Type        : T_Type;
      Code                : File_Type;
   begin
      Ouvrir_Fichier_Lecture(NomFichierCode, Code);
      Fini                := False;
      Mem.Entiers         := new P_Memoire_Entier.T_Var;
      Current_Mem_Integer := Mem.Entiers;
      -- Tant qu'on n'a pas atteint la fin du fichier ni la ligne "Debut"
      while not End_Of_File (Code) and not Fini loop
         Current_Line := To_Unbounded_String (Get_Line (Code));
         Current_Line := Translate (Current_Line, Ada.Strings.Maps.Constants.Lower_Case_Map);
         -- Si pas commentaire ou declaration debut programme
         if Index (Current_Line, "--") = 0 and Index (Current_Line, ":") > 0 then
            -- Verifier qu'on n'a pas atteint la ligne "Debut"
            if (Index (Current_Line, "début") > 0 or Index (Current_Line, "debut") > 0) then
               Fini := True;
            else
               -- Split au niveau de ':'
               slice_mot(Current_Line, Splitted_Line, ":");

               -- Recuperation du type
               if Index(Current_Line, "entier") > 0 then
                  Current_Type := ENTIER;
               end if;

               Current_Line := Splitted_Line;

               -- Split au niveau de ','
               slice_mot (Current_Line, Splitted_Line, ",");

               while Length (Splitted_Line) > 0 loop
                  -- Initialisation de la memoire en fonction de son type
                  if Current_Type = ENTIER then
                     Current_Mem_Integer.Cle := To_Unbounded_String(Strip_Space(To_String(Splitted_Line)));
                     Current_Mem_Integer.TypeOfData := P_Memoire_Entier.ENTIER;
                     Current_Mem_Integer.Suivant := new P_Memoire_Entier.T_Var;
                     -- Recuperation de l'element suivant
                     Current_Mem_Integer := Current_Mem_Integer.Suivant;
                  end if;
                  -- Split au niveau de ','
                  slice_mot (Current_Line, Splitted_Line, ",");
               end loop;
            end if;
         end if;
        end loop;
        Close(Code); -- fermeture du fichier contenant le code
   end DeclarerVariables;

   -- Permet de trouver la case memoire de la variable ayant Cle comme nom
   -- @param CaseMem : la premiere case memoire
   -- @param Cle : le nom de la variable
   -- @return : la case memoire contenant la variable
   function trouver_case_entier (CaseMem : in P_Memoire_Entier.T_Case_Memoire; Cle : in Unbounded_String) return P_Memoire_Entier.T_Case_Memoire is
      Pos_entier : P_Memoire_Entier.T_Case_Memoire;
   begin
      -- Recuperer la liste chainee
      Pos_entier := CaseMem;
      -- a chaque iteration on transforme la cle en minuscule pour que ce ne soit pas case sensitive
      while Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map) /= Pos_entier.Cle loop -- on n'est pas oblige de commencer par Pos_entier /= null or else car on sait que la variable existe en memoire
         Pos_entier := Pos_entier.Suivant;
      end loop;
      return Pos_entier;
   end trouver_case_entier;

   -- Modifie la donnee d'une variable existante en memoire
   -- @param Mem : la memoire modifiee
   -- @param Cle : le nom de la variable a modifier
   -- @param Data : la nouvelle valeur de la variable
   procedure Modifier_Entier (Mem : in out T_Memoire; Cle : in Unbounded_String; Data : in Integer) is
      Pos_entier : P_Memoire_Entier.T_Case_Memoire;
   begin
      Pos_entier := trouver_case_entier(Mem.Entiers, Cle);
      Pos_entier.Data := Data;
   end Modifier_Entier;


    -- Recupere la valeur d'une variable par son nom a partir d'une case memoire (est appelee par RecupererValeur_Entier)
    -- @param CaseMem : la case memoire dans laquelle on verifie si la cle correspond
    -- @param Cle : la cle de la variable dont on cherche la valeur
    -- @return : la valeur associee au nom de la variable
    function RecupererValeur_Entier_FromCase(CaseMem : in P_Memoire_Entier.T_Case_Memoire; Cle : in Unbounded_String) return Integer
    is
    begin
        -- Verifier si la cle est la bonne pour chaque case memoire
        if Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map) = CaseMem.Cle then
            return CaseMem.All.Data;
        else
            return RecupererValeur_Entier_FromCase(CaseMem.Suivant, Cle); -- on sait que la cle existe donc si elle n'a pas encore ete trouvee
            -- alors elle est dans la case suivante (pas la peine de tester si Suivant /= Null)
        end if;
    end RecupererValeur_Entier_FromCase;


   -- Recupere la valeur d'une variable par son nom (a partir de l'objet memoire)
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
   -- @return la valeur de la variable
   -- Pre-condition : Mem.Entiers /= Null et la clee existe dans la memoire (car le programme intermediaire est bien formee)
   function RecupererValeur_Entier (Memoire : in T_Memoire; Cle : in Unbounded_String) return Integer
    is
    begin
        -- on verifie s'il s'agit d'une variable (type=entier) ou d'une constante (type=null)
        if RecupererType(Memoire, Cle) = "Entier" then
            -- Recuperer la valeur en memoire
            return RecupererValeur_Entier_FromCase(Memoire.Entiers, Cle);
        else
           return Integer'Value(To_String(Cle));
        end if;

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
      while (Pos_entier /= null) and then (Pos_entier.Cle /= Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map)) loop
         Pos_entier := Pos_entier.Suivant;
      end loop;
      if Pos_entier /= null then
         return To_Unbounded_String("Entier");
      end if;
      Pos_string := Mem.Chaines;
      while (Pos_string /= null) and then (Pos_string.Cle /= Translate(Cle, Ada.Strings.Maps.Constants.Lower_Case_Map)) loop
         Pos_string := Pos_string.Suivant;
      end loop;
      if Pos_string /= null then
         return To_Unbounded_String("Chaine");
      end if;
      return To_Unbounded_String("null");
   end RecupererType;

end Memoire;
