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

   -- Initialise le compteur
   -- @param CP : compteur a initialiser
   procedure init_CP (CP: out Integer) with
     Post => CP = 1;
   
   -- Incremente le compteur
   -- @param CP : compteur a incrementer
   procedure increm_CP (CP : in out Integer) with
     Post => CP'Old +1 = CP;

   -- Rempli le tableau avec les instructions du fichier
   -- en mettant sous la forme voulue
   -- Si la ligne est un coommentaire : devient un null dans le tableau
   -- @param Tab : tableau a remplir
   -- @param Fichier : code source a utiliser pour remplir le tableau
   procedure remplir_tab_instruc (Tab : in out T_tab_instruc; Fichier : in File_Type);

   -- Effectue une instruction passee en parametre en fonction de son type (GOTO, null, if, op)
   -- @param Tab : tableau comptenant les instructions
   -- @param CP : compteur
   -- @param mem : liste chainee comptenant les variables et leurs valeurs
	procedure effectuer_instru (Tab : in T_tab_instruc; CP : in Integer; mem : in out T_memoire);

   -- Pour debugger : Affihe memoire CP et la memoire regroupant les valeurs des differentes variables
   -- @param Tab : tableau comptenant les instructions
   -- @param CP : compteur
   -- @param mem : liste chainee comptenant les variables et leurs valeurs
   procedure afficher (Tab : in T_tab_instruc; CP : in Integer; mem : in out T_memoire);

   -- Pour debugger : retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   function recuperer_instru_pos1 (Tab : in T_tab_instruc; CP : in Integer) return String;
   function recuperer_instru_pos2 (Tab : in T_tab_instruc; CP : in Integer) return String;
   function recuperer_instru_pos3 (Tab : in T_tab_instruc; CP : in Integer) return String;
   function recuperer_instru_pos4 (Tab : in T_tab_instruc; CP : in Integer) return String;
   
private
   type T_op is
      record
         pos1: String (1..25);
         pos2 : String (1..25);
         pos3 : String (1..25);
         pos4 : String (1..25);
      end record;
   
   type T_tab_instruc is array (1..Capacite) of T_op;

end Decode;
