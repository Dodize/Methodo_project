with Ada.Text_IO ;                use Ada.Text_IO ;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

-- Ce module definit des fonctions utiles dans plusieurs modules afin d'eviter le code redondant

package utils is

    -- permet de récupérer dans mot les caractères d'une ligne présent avant délimiteur
    -- @param Ligne : ligne dont on extrait le mot
    -- @param Mot : a placer le mot extrait
    -- @param Delimiteur: caractere signant la fin du mot
    procedure slice_mot(Ligne : in out Unbounded_String; Mot : out Unbounded_String; Delimiteur : in String);
   
    -- Permet de récupérer une chaine de caractères d'une ligne
    -- @param Ligne : ligne dont on extrait la chaine
    -- @param First_part : la partie de la ligne située avant la première chaine
    -- @param Second_part : la partie de la ligne avec la première chaine trouvée
    -- @param Third_part : la partie de la ligne située après la première chaine
    -- @param first_quote_found : indique si on a trouvé une chaine
    procedure slice_string(Ligne : in String; First_part : out Unbounded_String; Second_part : out Unbounded_String; Third_part : out Unbounded_String; first_quote_found : out Boolean);
    
    -- Ouvre le fichier en lecture pour pouvoir ensuite le parcourir
    -- @param NomFichier : le nom du fichier que l'on souhaite ouvrir en lecture
    -- @param FichierOuvert : l'object fichier apres ouverture
    procedure Ouvrir_Fichier_Lecture(NomFichier : in String; FichierOuvert : out File_Type);
    
    -- Parcours le fichier jusqu'à touver le mot "Début" et se place juste après
    -- @param Fichier : fichier à parcourir
    procedure parcourir_debut(Fichier : in File_Type);
   
    -- Permet de retirer les espaces d'une chaine
    -- @pram Chaine : la chaine origine
    -- @return String : renvoie la chaine de départ sans aucun espace
    function Strip_Space (Chaine : in String) return String;
   
    -- Procedure permettant de creer un fichier temporaire d'instructions pour realiser les tests
    -- @param : Fichier le fichier cree
    procedure createFileInstruct(Fichier : out File_Type);
    
    -- Procedure permettant de supprimer le fichier temporaire d'instructions pour realiser les tests
    -- @param : Fichier le fichier a  supprimer
    procedure deleteFileInstruct(NomFichier : in String);
        
end utils;
