with Decode;
with Utils;           use Utils;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Memoire; use Memoire;

package body Interpreteur is

   -- Permet de choisir le mode d'utilisation : 0 pour normal et 1 pour debug
   function Menu return Integer is
      type_menu : Integer;
   begin
      Put_Line("Veuillez choisir le mode choisi : 0 pour normal et 1 pour debug");
      Get(type_menu);
      while menu /= 0 or menu /= 1 loop
         Put_Line("Veuillez choisir le mode choisi : 0 pour normal et 1 pour debug");
         Get(type_menu);
      end loop;
      return type_menu;
   end Menu;

   -- Calcule la capacite du tableau d'instructions en fonction du nombre de ligne effective du fichier du code intermediaire
   -- (entre les mots "Début" ou "Debut" et "Fin")
   -- @param : in le fichier d'instructions en code intermediaire
   -- @return : la capacite
   function CalculerCapacite(Fichier : in File_Type) return Integer is
      Capacite : Integer;
   begin

      -- Parcourir le fichier jusqu'à "début"
      parcourir_debut(Fichier);

      -- Calculer le nombre de lignes de la ligne début + 1 / jusqu'à la fin du programme (mot "Fin)
      Capacite := 0;
      while not (Get_Line(Fichier) = "Fin") loop
         Capacite := Capacite + 1;
      end loop;

      return Capacite;

   end CalculerCapacite;


   -- Lance l'interpreteur en lisant et excecutant le code
   procedure Executer(Fichier : in File_Type) is
      package P_Decode is new Decode(Capacite => CalculerCapacite(Fichier));
      use P_Decode;
      Instructions : T_tab_instruc;
      Mem : T_Memoire;
      cp : Integer;
      Capacite_tab : Integer;
   begin
      -- Declaration des variables en memoire
      DeclarerVariables(Mem, Fichier);
      -- Recuperation des instructions
      remplir_tab_instruc(Instructions, Fichier);
      -- Initialisation du CP
      init_CP(cp);
      -- parcour le tableau tant que l'on n'a pas termine le programme
      while cp < Capacite_tab loop
         effectuer_instru(Instructions, cp, Mem);
      end loop;

   end Executer;

end Interpreteur;
