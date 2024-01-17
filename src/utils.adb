package body utils is

   -- permet de retirer le premier mot avant délimiteur de la ligne en le plaçant dans mot
   -- @param Ligne : ligne dont on extrait le mot
   -- @param Mot : a placer le mot extrait
   -- @param Delimiteur: caractere signant la fin du mot
   procedure slice_mot(Ligne : in out Unbounded_String; Mot : out Unbounded_String; Delimiteur : in Character) is
      Pos : Natural;
   begin
      -- Trouver l'indice du délimiteur
      Pos := Ada.Strings.Unbounded.Index(Ligne, Character'Image(Delimiteur));
      -- Extraire la sous-chaîne jusqu'au délimiteur
      if Pos /= 0 then
         Mot := Unbounded_Slice(Ligne, 1, Pos);
         -- Supprimer la partie extraite de la ligne
         Delete(Ligne, 1, Pos);
      else
         -- Si le délimiteur n'est pas trouvé, copier la ligne entière dans le mot
         Mot := Ligne;
         -- Réinitialiser la ligne
         Delete(Ligne, 1, Length(Ligne));
      end if;
   end slice_mot;

   procedure parcourir_debut(Ligne : out Unbounded_String; Fichier : in File_Type) is
      -- Parcours jusqu'a debut pour remplir uniquement les lignes de codes en ignorant la decla de variable
      begin
      Ligne := To_Unbounded_String(Get_Line(Fichier));
      while (Index(Ligne, "D�but")) = 0 or else (Index(Ligne, ":")) > 0 loop
         Ligne := To_Unbounded_String(Get_Line(Fichier));
      end loop;
   end parcourir_debut;

end utils;
