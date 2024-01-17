-- Ce module definit les operations necessaires a la gestion de la memoire
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps;
with Case_Memoire;

generic
   with package P_Memoire is new Case_Memoire(<>);

package Memoire is

   use P_Memoire;

   -- Declare toutes les variables en memoire
   -- @param Mem : la memoire
   -- @param Code : le code ou sont declarees les variables
   procedure DeclarerVariables(Mem : out T_Memoire ; Code : in File_Type);

   -- Modifie la donnee d'une variable existante en memoire
   -- @param Mem : la memoire modifiee
   -- @param Cle : le nom de la variable a modifier
   -- @param Data : la nouvelle valeur de la variable
   procedure Modifier(Mem : in out T_Memoire ; Cle : in Unbounded_String ; Data : in T_Elt);

   -- Recupere la valeur d'une variable par son nom
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
   function RecupererValeur(Mem : in T_Memoire ; Cle : in Unbounded_String) return T_Elt;

   -- Recupere le type d'une variable par son nom
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
   function RecupererType(Mem : in T_Memoire ; Cle : in Unbounded_String) return T_Type;

end Memoire;
