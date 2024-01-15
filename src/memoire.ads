-- Ce module definit les operations necessaires a la gestion de la memoire
with Ada.Text_IO; use Ada.Text_IO;

generic
   type T_Elt is private;

package Memoire is

   type T_Memoire is private;

   -- Enumerable du type de la variable
   type T_Type is (CHAINE, ENTIER, TAB);

   -- Declare toutes les variables en memoire
   -- @param Mem : la memoire modifiee
   -- @param Code : le code où sont déclarées les variables
   procedure DeclarerVariables(Mem : in out T_Memoire ; Code : in File_Type);

   -- Modifie la donnee d'une variable existante en memoire
   -- @param Mem : la memoire modifiee
   -- @param Cle : le nom de la variable a modifier
   -- @param Data : la nouvelle valeur de la variable
   procedure Modifier(Mem : in out T_Memoire ; Cle : in String ; Data : in T_Elt);

   -- Recupere la valeur d'une variable par son nom
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
   function RecupererValeur(Mem : in T_Memoire ; Cle : in String) return T_Elt;

   -- Recupere le type d'une variable par son nom
   -- @param Mem : la memoire dans laquelle est stockee la variable
   -- @param Cle : le nom de la variable recherchee
   function RecupererType(Mem : in T_Memoire ; Cle : in String) return T_Type;

   private

	type T_Var;
	type T_Memoire is access T_Var;
	type T_Var is
      record
         Cle: Character;
         Data : T_Elt;
         TypeOfData : T_Type;
         Suivant : T_Memoire;
		end record;

end Memoire;
