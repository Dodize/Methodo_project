with Decode;
with Utils;           use Utils;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Text_IO ;                use Ada.Text_IO ;
with Memoire; use Memoire;

package body Interpreteur is

    -- Permet de choisir le mode d'utilisation : 0 pour normal et 1 pour debug
    -- @return : 0 pour mode d'execution normal, 1 pour le mode debug
    function Menu return Integer is
        type_menu : Integer;
    begin
        Put_Line("Veuillez choisir le mode choisi : 0 pour normal et 1 pour debug");
        Get(type_menu);
        while type_menu /= 0 and type_menu /= 1 loop
            Put_Line("Veuillez choisir le mode choisi : 0 pour normal et 1 pour debug");
            Get(type_menu);
        end loop;
        return type_menu;

    exception
        when others =>
            -- Gérer l'exception Constraint_Error
            Ada.Text_IO.Put_Line("Erreur : Entrez 0 ou 1.");
            -- Supprimer la ligne incorrecte de l'entrée
            Skip_Line;
            return Menu;
    end Menu;

    -- Calcule la capacite du tableau d'instructions en fonction du nombre de ligne effective du fichier du code intermediaire
    -- (entre les mots "Début" et "Fin")
    -- @param : in le fichier d'instructions en code intermediaire
    -- @return : la capacite
    function CalculerCapacite(NomFichier : in String) return Integer is
        Capacite : Integer;
        Fichier : File_Type;
    begin
        Ouvrir_Fichier_Lecture(NomFichier, Fichier);

        -- Parcourir le fichier jusqu'à "Début"
        parcourir_debut(Fichier);

        -- Calculer le nombre de lignes de la ligne début + 1 / jusqu'à la fin du programme (mot "Fin")
        Capacite := 0;
        while not (Get_Line(Fichier) = "Fin") loop
            Capacite := Capacite + 1;
        end loop;
        Close(Fichier);
        return Capacite;
    end CalculerCapacite;

    -- Lance l'interpreteur en lisant et excecutant le code intermédiaire
    procedure Executer(NomFichier : in String) is
        package P_Decode is new Decode(Capacite => CalculerCapacite(NomFichier));
        use P_Decode;
        Instructions : T_tab_instruc;
        Mem : T_Memoire;
        CP : Integer;
        Capacite_tab : Integer;
        Mode : Integer;
    begin
        -- Declaration des variables en memoire, récuperation des instructions
        DeclarerVariables(Mem, NomFichier);
        remplir_tab_instruc(Instructions, NomFichier);
        Capacite_tab := get_nombre_instruc(Instructions);
        -- Initialisation du CP
        init_CP(CP);
        -- Demande le Mode : normal ou debugg
        mode := menu;
        -- parcour le tableau tant que l'on n'a pas termine le programme
        while (CP <= Capacite_tab) loop
            if Mode = 1 then
                afficher(Instructions, CP, Mem);
            end if;
            effectuer_instru(Instructions, CP, Mem);
        end loop;
    end Executer;

end Interpreteur;
