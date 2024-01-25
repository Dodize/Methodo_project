-- Ce module definit les operations necessaires au decodage du code intermediaire
-- Il stock les instructions et les executes.
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Memoire;                     use Memoire;

generic
      Capacite: Integer;   -- Nombre de ligne du code interprete

package Decode is
       
    Type T_tab_instruc is limited private ;
    
    GOTO_OUT_OF_RANGE_EXCEPTION : Exception; -- la ligne renvoyee par GOTO est inf a 1 ou supp au nombre de lignes du code
    OP_NON_RECONNUE_EXCEPTION : Exception; -- exception levee si l'operation stockee n'est pas reconnue (comportement inattendu)

   -- Initialise le compteur
   -- @param CP : compteur a initialiser
   procedure init_CP (CP: out Integer) with
     Post => CP = 1;
   
   -- Retourne le nombre d'instructions du tableau d'instructions (le nombre de ligne)
   -- @param Tab : le tableau d'instructions
   -- @return : le nombre d'instructions
   function get_nombre_instruc(Tab : in T_tab_instruc) return Integer;
    
   -- Rempli le tableau avec les instructions du fichier
   -- en mettant sous la forme voulue
   -- Si la ligne est un coommentaire : devient un null dans le tableau
   -- @param Tab : tableau a remplir
   -- @param NomFichier : le nom du fichier contenant code source a utiliser pour remplir le tableau
   procedure remplir_tab_instruc (Tab : in out T_tab_instruc; NomFichier : in String);

   -- Effectue une instruction passee en parametre en fonction de son type (GOTO, null, if, op)
   -- @param Tab : tableau comptenant les instructions
   -- @param CP : compteur
   -- @param mem : liste chainee comptenant les variables et leurs valeurs
	procedure effectuer_instru (Tab : in T_tab_instruc; CP : in out Integer; mem : in out T_memoire);

   -- Pour debugger : Affihe memoire CP et la memoire regroupant les valeurs des differentes variables
   -- @param Tab : tableau comptenant les instructions
   -- @param CP : compteur
   -- @param mem : liste chainee comptenant les variables et leurs valeurs
   procedure afficher (Tab : in T_tab_instruc; CP : in Integer; mem : in out T_memoire);

   -- Pour debugger : retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   -- @return : le parametre de tab demande
   function recuperer_instru_pos1 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String;
   function recuperer_instru_pos2 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String;
   function recuperer_instru_pos3 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String;
   function recuperer_instru_pos4 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String;

   
private
   type T_op is
      record
         pos1: Ada.Strings.Unbounded.Unbounded_String;
         pos2 : Ada.Strings.Unbounded.Unbounded_String;
         pos3 : Ada.Strings.Unbounded.Unbounded_String;
         pos4 : Ada.Strings.Unbounded.Unbounded_String;
      end record;
   
   type T_tab_instruc is array (1..Capacite) of T_op;

end Decode;
