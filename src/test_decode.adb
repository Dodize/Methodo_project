with Decode; 
with Memoire;
procedure test_decode is
    
    package MemoireEntier is new Memoire(T_Elt => Integer);
    use MemoireEntier;
    
    -- Tests concernant effectuer_instru
    
    procedure test_instruction_NULL is  
        
        package Decode2 is new Decode(P_Memoire => MemoireEntier, Capacite => 2);
        use Decode2;
        
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement, MemoireX : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (deux lignes)
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x : Entier");
        Modifier(MemoirePremierElement, "x", 2);
        
        -- test : opération NULL
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        pragma Assert (CP = 2);
        MemoireX := RecupererVariable(MemoirePremierElement, "x");
        pragma Assert (MemoireX.all.Data = 2);       
        
    end;
    
    
begin
    Null;
    
   
end test_decode;
