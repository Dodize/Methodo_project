package body Memoire is

   -- Vérifier si delete inclu la supp du delimiteur comme souhaité
   procedure slice_mot
     (Ligne      : in out Unbounded_String; mot : in out Unbounded_String;
      delimiteur : in     Character)
   is
      Index : Natural;
   begin
      -- Trouver l'indice du délimiteur
      Index :=
        Ada.Strings.Unbounded.Index (Ligne, Character'Image (delimiteur));

      -- Extraire la sous-chaîne jusqu'au délimiteur
      if Index /= 0 then
         mot := Unbounded_Slice (Ligne, 1, Index);
         -- Supprimer la partie extraite de la ligne
         Delete (Ligne, 1, Index);
      else
      -- Si le délimiteur n'est pas trouvé, copier la ligne entière dans le mot
         mot := Ligne;
         -- Réinitialiser la ligne
         Delete (Ligne, 1, Length (Ligne));
      end if;
   end slice_mot;

   -- Declare toutes les variables en memoire
   -- @param Mem : la memoire
   -- @param Code : le code ou sont declarees les variables
   procedure DeclarerVariables (Mem : out T_Memoire; Code : in File_Type) is

      package Mem_Integer is new Case_Memoire (T_Elt => Integer);
      package Mem_String is new Case_Memoire (T_Elt => Unbounded_String);

      Current_Mem_Integer     : Mem_Integer.T_Memoire;
      New_Mem_Integer : Mem_Integer.T_Memoire;
      Current_Line    : Unbounded_String;
      Temporary_Line  : Unbounded_String;
      Splitted_Line   : Unbounded_String;
      Fini            : Boolean;
      Current_Type    : T_Type;
   begin
      Fini := False;
      Initialiser(Mem);
      Current_Mem_Integer := Mem;
      -- Tant qu'on n'a pas atteint la fin du fichier ni la ligne "Debut"
      while not End_Of_File (Code) and not Fini loop
         Current_Line := To_Unbounded_String (Get_Line (Code));
         Current_Line := Translate (Current_Line, Ada.Strings.Maps.Constants.Lower_Case_Map);
         -- Si pas commentaire
         if Index (Current_Line, "--") = 0 then
            -- Verifier qu'on n'a pas atteint la ligne "Debut"
            if
              (Index (Temporary_Line, "début") > 0 or
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
                     Mem_Integer.Initialiser(New_Mem_Integer);
                     New_Mem_Integer.Cle        := Splitted_Line;
                     New_Mem_Integer.TypeOfData := Mem_Integer.ENTIER;

                     if Est_Vide (Mem) then
                        Mem := New_Mem_Integer;
                     else
                        Current_Mem.Suivant := New_Mem_Integer;
                     end if;
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

   -- Recupere la valeur d'une variable par son nom
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
   -- @return la valeur de la variable
   function RecupererValeur_Entier (Mem : in T_Memoire; Cle : in Unbounded_String) return Integer
   is
   begin
      return 0; -- A CHANGER !!
   end RecupererValeur_Entier;

   -- Recupere le type d'une variable par son nom
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
   -- @return le type de la variable
   function RecupererType (Mem : in T_Memoire; Cle : in Unbounded_String) return Unbounded_String
   is
   begin
      return To_Unbounded_String("CHAINE");
   end RecupererType;

end Memoire;
