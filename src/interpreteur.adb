with Decode;
with Memoire;
package body Interpreteur is

   -- Permet de choisir le mode d'utilisation : 0 pour normal et 1 pour debug
    function Menu return Integer is
    begin
        return 0;
    end Menu;

    -- Calcule la capacite du tableau d'instructions en fonction du nombre de ligne effective du fichier du code intermediaire
    -- (entre les mots "Début" ou "Debut" et "Fin")
    -- @param : in le fichier d'instructions en code intermediaire
    -- @return : la capacite
    function CalculerCapacite(Fichier : in File_Type) return Integer is
        Capacite : Integer;
    begin

        -- Parcourir le fichier jusqu'à "début"
        while not (Get_Line(Fichier) = "Début" or else Get_Line(Fichier) = "Debut") loop
            Null;
        end loop;

        -- Calculer le nombre de lignes de la ligne début + 1 / jusqu'à la fin du programme (mot "Fin)
        Capacite := 0;
        while not (Get_Line(Fichier) = "Fin") loop
            Capacite := Capacite + 1;
        end loop;

        return Capacite;

    end CalculerCapacite;



   -- Lance l'interpreteur en lisant et excecutant le code
    procedure Executer(Fichier : in File_Type) is
        package MemoireEntier is new Memoire(T_Elt => Integer);
        package Decode2Entier is new Decode(P_Memoire => MemoireEntier, Capacite => CalculerCapacite(Fichier));
        use MemoireEntier; use Decode2Entier;
        tab : T_tab_instruc;
    begin
        Null;
    end Executer;


end Interpreteur;
