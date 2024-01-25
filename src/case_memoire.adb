package body Case_Memoire is

    -- Initialise une case memoire. La case memoire est vide
    -- @param Mem : la case memoire a initialiser
    procedure Initialiser(Mem : out T_Case_Memoire) is
    begin
        Mem := null;
    end Initialiser;

    -- Indique si une case de la memoire est vide
    -- @param Mem : la case memoire à tester
    -- @return Boolean indiquant si la memoire est vide ou non
    function Est_Vide(Mem : in T_Case_Memoire) return Boolean is
    begin
        return Mem = null;
    end Est_Vide;

end Case_Memoire;
