with Ada.Text_IO ;                use Ada.Text_IO ;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

-- Ce module definit des fonctions utiles dans plusieurs modules afin d'eviter le code redondant

package utils is

   -- permet de retirer le premier mot avant delimiteur de la ligne en le placant dans mot
   -- @param Ligne : ligne dont on extrait le mot
   -- @param Mot : a placer le mot extrait
   -- @param Delimiteur: caractere signant la fin du mot
   procedure slice_mot(Ligne : in out Unbounded_String; Mot : out Unbounded_String; Delimiteur : in String);
   

   -- Ouvre le fichier en lecture pour pouvoir ensuite le parcourir
   -- @param NomFichier : le nom du fichier que l'on souhaite ouvrir en lecture
   -- @param FichierOuvert : l'object fichier apres ouverture
   procedure Ouvrir_Fichier_Lecture(NomFichier : in String; FichierOuvert : out File_Type);
    
   procedure parcourir_debut(Fichier : in File_Type);
   
   -- Permet de retirer les espaces d'une chaine
   -- @pram Chaine : la string d'origine
   -- @return String
   function Strip_Space (Chaine : in String) return String;
   
end utils;
