-- Ce module definit les operations necessaires a la gestion de la memoire
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps;
with Case_Memoire;

package Memoire is

   package P_Memoire_Entier is new Case_Memoire(T_Elt => Integer);
   package P_Memoire_String is new Case_Memoire(T_Elt => Unbounded_String);
   use P_Memoire_Entier;
   use P_Memoire_String;

   -- Enumerable du type de la variable
   type T_Type is (CHAINE, ENTIER, TAB);

   type T_Memoire is
      record
         Entiers : P_Memoire_Entier.T_Case_Memoire;
         Chaines : P_Memoire_String.T_Case_Memoire;
      end record;

   -- Declare toutes les variables en memoire
   -- @param Mem : la memoire
   -- @param Code : le code ou sont declarees les variables
   procedure DeclarerVariables(Mem : out T_Memoire ; Code : in File_Type);

   -- Modifie la donnee d'une variable existante en memoire
   -- @param Mem : la memoire modifiee
   -- @param Cle : le nom de la variable a modifier
   -- @param Data : la nouvelle valeur de la variable
   procedure Modifier_Entier(Mem : in out T_Memoire ; Cle : in Unbounded_String ; Data : in Integer);

   -- Recupere la valeur d'une variable par son nom
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
   function RecupererValeur_Entier(Mem : in T_Memoire ; Cle : in Unbounded_String) return Integer;

   -- Recupere le type d'une variable par son nom
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
   function RecupererType(Mem : in T_Memoire ; Cle : in Unbounded_String) return Unbounded_String;

end Memoire;
