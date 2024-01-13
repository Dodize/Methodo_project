-- Ce module definit les operations necessaires au decodage du code intermediaire
-- Il stock les instructions et les executes.
WITH Ada.Text_IO ;                USE Ada.Text_IO ;
with Memoire;

generic
    with package P_Memoire is new Memoire(<>);
      Capacite: Integer;   -- Nombre de ligne du code interprete

package Decode is
    
    use P_Memoire;
   
    Type T_tab_instruc is limited private ;
    
    GOTO_OUT_OF_RANGE_EXCEPTION : Exception; -- la ligne renvoyee par GOTO est inf a 1 ou supp au nombre de lignes du code
   
   -- Verifie si le tableau d'instruction est vide
   function est_null(Tab : in T_tab_instruc) return Boolean;

   -- Initialiser le tableau d'instruction
   procedure init_tab_instruc (Tab : out T_tab_instruc) with
     Post => est_null(Tab);

   -- Initialise le compteur
   procedure init_CP (CP: out Integer) with
     Post => CP = 1;

   procedure increm_CP (CP : in out Integer) with
     Post => CP'Old +1 = CP;

   -- Rempli le tableau avec les instructions du fichier
   -- en mettant sous la forme voulue
   -- Si la ligne est un coommentaire : devient un null dans le tableau
   procedure remplir_tab_instruc (Tab : in out T_tab_instruc; Fichier : in File_Type);

   -- Effectue une instruction passee en parametre en fonction de son type (GOTO, null, if, op)
	procedure effectuer_instru (Tab : in T_tab_instruc; CP : in Integer; mem : in out T_memoire);

   -- Pour debugger : Affihe memoire CP et la memoire regroupant les valeurs des differentes variables
   procedure afficher (Tab : in T_tab_instruc; CP : in Integer; mem : in out T_memoire);

   
private
   type T_op is
      record
         pos1: String (1..25);
         pos2 : String (1..25); --si fait ca faut un 5eme champs pour cste ou variable
         pos3 : String (1..25); --on met que des int au lieu de string
         pos4 : String (1..25); --x on met l'adresse
      end record;
   
   type T_tab_instruc is array (1..Capacite) of T_op;

end Decode;
