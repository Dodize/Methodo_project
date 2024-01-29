-- Ce module definit les operations necessaires au decodage du code intermediaire
-- Il stock les instructions et les executes.
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Memoire;                     use Memoire;

generic
      Capacite: Integer;   -- Nombre de ligne du code interprete

package Decode is
       
    Type T_tab_instruc is limited private ; -- le tableau des instructions
    
    GOTO_OUT_OF_RANGE_EXCEPTION : Exception; -- exception levee si la ligne renvoyee par GOTO est inf a 1 ou supp au nombre de lignes du code
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
   -- Si la ligne est un commentaire : devient un null dans le tableau
   -- @param Tab : tableau a remplir
   -- @param NomFichier : le nom du fichier contenant code source a utiliser pour remplir le tableau
   -- @exception ADA.IO_EXCEPTIONS.NAME_ERROR : si le fichier ayant pour nom "NomFichier" n'existe pas ou n'est pas accessible par le programme
    procedure remplir_tab_instruc (Tab : out T_tab_instruc; NomFichier : in String);

   -- Effectue une instruction passee en parametre en fonction de son type (GOTO, null, if, op)
   -- @param Tab : tableau comptenant les instructions
   -- @param CP : compteur
   -- @param mem : liste chainee comptenant les variables et leurs valeurs
   -- @exception OP_NON_RECONNUE_EXCEPTION : si l'instruction a effectuer n'est pas reconnue
	procedure effectuer_instru (Tab : in T_tab_instruc; CP : in out Integer; mem : in out T_memoire);

   -- Pour debugger : Affiche le CP et la memoire regroupant les valeurs des differentes variables
   -- @param Tab : tableau comptenant les instructions
   -- @param CP : compteur
   -- @param mem : liste chainee comptenant les variables et leurs valeurs
   procedure afficher (Tab : in T_tab_instruc; CP : in Integer; mem : in out T_memoire);

   -- Retourne une partie d'une instruction a la ligne du CP
   -- @param Tab : tableau contenant les instructions
   -- @param CP : la ligne de la partie a recuperer
   -- @return : le parametre de tab demande
   function recuperer_instru_pos1 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String;
   function recuperer_instru_pos2 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String;
   function recuperer_instru_pos3 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String;
   function recuperer_instru_pos4 (Tab : in T_tab_instruc; CP : in Integer) return Unbounded_String;

   
private
   -- Type representant une instruction
   type T_op is
      record
         pos1: Ada.Strings.Unbounded.Unbounded_String;  -- partie1 de l'instruction
         pos2 : Ada.Strings.Unbounded.Unbounded_String; -- partie2 de l'instruction
         pos3 : Ada.Strings.Unbounded.Unbounded_String; -- partie3 de l'instruction
         pos4 : Ada.Strings.Unbounded.Unbounded_String; -- partie4 de l'instruction
      end record;
   
   type T_tab_instruc is array (1..Capacite) of T_op;

end Decode;
