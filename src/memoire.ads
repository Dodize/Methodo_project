-- Ce module definit les operations necessaires à la gestion de la mémoire

generic
   type T_Elt is private;

package Memoire is

   type T_Memoire is private;

   -- Enumerable du type de la variable
   type T_Type is (CHAINE, ENTIER, TAB);

   -- Initialise une case memoire. La case memoire est vide
   procedure Initialiser(Mem : out T_Memoire) with
     Post => Est_Vide(Mem);

   -- Indique si la memoire est vide
   function Est_Vide(Mem : in T_Memoire) return Boolean;

   -- Declare toutes les variables en memoire
   procedure DeclarerVariables(Mem : in out T_Memoire ; Code : in String);

   -- Modifie la donnee d'une variable existante en memoire
   procedure Modifier(Mem : in out T_Memoire ; Cle : in String ; Data : in T_Elt);

   -- Recupere une variable par son nom
   function RecupererVariable(Mem : in T_Memoire ; Cle : in String) return T_Memoire;

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
