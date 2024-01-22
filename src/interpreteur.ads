with Ada.Text_IO ;                use Ada.Text_IO ;

package Interpreteur is

   -- Permet de choisir le mode d'utilisation : 0 pour normal et 1 pour debug
   -- @return : 0 pour mode d'execution normal, 1 pour le mode debug
   function Menu return Integer with
     Post => Menu'Result = 0 or Menu'Result = 1;

   -- Lance l'interpreteur en lisant et excecutant le code
   procedure Executer(NomFichier : in String);

end Interpreteur;
