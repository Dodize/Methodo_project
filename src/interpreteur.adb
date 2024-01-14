package body Interpreteur is

   -- Permet de choisir le mode d'utilisation : 0 pour normal et 1 pour debug
    function Menu return Integer is
    begin
        return 0;
    end Menu;


   -- Lance l'interpreteur en lisant et excecutant le code
    procedure Executer(Fichier : in File_Type) is
    begin
        Null;
    end Executer;


end Interpreteur;
