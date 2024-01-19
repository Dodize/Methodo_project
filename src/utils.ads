with Ada.Text_IO ;                use Ada.Text_IO ;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

-- Ce module définit des fonctions utiles dans plusieurs modules afin d'eviter le code redondant

package utils is

   -- permet de retirer le premier mot avant delimiteur de la ligne en le plaÃ§ant dans mot
   -- @param Ligne : ligne dont on extrait le mot
   -- @param Mot : a placer le mot extrait
   -- @param Delimiteur: caractere signant la fin du mot
   procedure slice_mot(Ligne : in out Unbounded_String; Mot : out Unbounded_String; Delimiteur : in String);
   

   procedure parcourir_debut(Fichier : in File_Type);
   

end utils;