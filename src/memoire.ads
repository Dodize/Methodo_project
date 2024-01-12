-- Ce module definit les operations necessaires à la gestion de la mémoire

generic
   type T_Elt is private;

package Memoire is

   type T_Memoire is private;

   -- Enum�rable du type de la variable
   type T_Type is (CHAINE, ENTIER, TAB);

   -- Initialise une case m�moire. La case m�moire est vide
   procedure Initialiser(Mem : out T_Memoire) with
     Post => Est_Vide(Mem);

   -- Indique si la m�moire est vide
   function Est_Vide(Mem : in T_Memoire) return Boolean;

   -- D�clare toutes les variables en m�moire
   procedure DeclarerVariables(Mem : in out T_Memoire ; Code : in String);

   -- Modifie la donn�e d'une variable existante en m�moire
   procedure Modifier(Mem : in out T_Memoire ; Cle : in String ; Data : in T_Elt);

   -- R�cup�re une variable par son nom
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
