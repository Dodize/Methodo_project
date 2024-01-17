with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

generic
   type T_Elt is private;

package Case_Memoire is

   -- Enumerable du type de la variable
   type T_Type is (CHAINE, ENTIER, TAB);

	type T_Var;
	type T_Case_Memoire is access T_Var;
	type T_Var is
      record
         Cle: Unbounded_String;
         Data : T_Elt;
         TypeOfData : T_Type;
         Suivant : T_Case_Memoire;
      end record;

   -- Initialise une case memoire. La case memoire est vide
   -- @param Mem : la memoire a initialiser
    procedure Initialiser(Mem : out T_Case_Memoire);

   -- Indique si la memoire est vide
   -- @param Mem : la memoire
   -- @return si la memoire est vide ou non
   function Est_Vide(Mem : in T_Case_Memoire) return Boolean;

end Case_Memoire;
