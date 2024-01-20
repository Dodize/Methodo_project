package body utils is

   -- permet de retirer le premier mot avant delimiteur de la ligne en le placant dans mot
   -- @param Ligne : ligne dont on extrait le mot
   -- @param Mot : a placer le mot extrait
   -- @param Delimiteur: caractere signant la fin du mot
   procedure slice_mot(Ligne : in out Unbounded_String; Mot : out Unbounded_String; Delimiteur : in String) is
      Pos : Natural;
   begin
      -- Trouver l'indice du delimiteur
      Pos := Index(Ligne, Delimiteur);
      -- Extraire la sous-chaine jusqu'au delimiteur
      if Pos /= 0 then
         Mot := Unbounded_Slice(Ligne, 1, Pos-1);
         -- Supprimer la partie extraite de la ligne
         Delete(Ligne, 1, Pos);
      else
         -- Si le delimiteur n'est pas trouve, copier la ligne entiÃ¨re dans le mot
         Mot := Ligne;
         -- Reinitialiser la ligne
         Delete(Ligne, 1, Length(Ligne));
      end if;
   end slice_mot;

    procedure parcourir_debut(Fichier : in File_Type) is
        Ligne : Unbounded_String;
      -- Parcours jusqu'a debut pour remplir uniquement les lignes de codes en ignorant la decla de variable
      begin
      Ligne := To_Unbounded_String(Get_Line(Fichier));
      while (Index(Ligne, "D�but")) = 0 or else (Index(Ligne, ":")) > 0 loop
         Ligne := To_Unbounded_String(Get_Line(Fichier));
      end loop;
   end parcourir_debut;

   -- Permet de retirer les espaces d'une chaine
   -- @pram Chaine : la string d'origine
   -- @return String
   function Strip_Space (Chaine : in String) return String is
   begin
      if Chaine'Length = 0 then
         return "";
      elsif Chaine (Chaine'First) = ' ' then
         return Strip_Space (Chaine (Chaine'First + 1 .. Chaine'Last));
      else
         return Chaine (Chaine'First) & Strip_Space (Chaine (Chaine'First + 1 .. Chaine'Last));
      end if;
   end Strip_Space;

end utils;
