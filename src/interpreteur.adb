with Decode;
with Memoire;
package body Interpreteur is

   -- Permet de choisir le mode d'utilisation : 0 pour normal et 1 pour debug
    function Menu return Integer is
    begin
        return 0;
    end Menu;

    function CalculerCapacite(Fichier : in File_Type) return Integer is
    begin
        return 0; -- TODO
    end CalculerCapacite;



   -- Lance l'interpreteur en lisant et excecutant le code
    procedure Executer(Fichier : in File_Type) is
        package MemoireEntier is new Memoire(T_Elt => Integer);
        package Decode2Entier is new Decode(P_Memoire => MemoireEntier, Capacite => CalculerCapacite(Fichier));
        use MemoireEntier; use Decode2Entier;
        tab : T_tab_instruc;
    begin
        init_tab_instruc(tab);
    end Executer;


end Interpreteur;
