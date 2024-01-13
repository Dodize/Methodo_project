with Decode; 
with Memoire;
procedure test_decode is
    
    subtype Str50 is String (1..50);
    
    package MemoireEntier is new Memoire(T_Elt => Integer);
        
    package MemoireChaine is new Memoire(T_Elt => Str50);
    
    package Decode2Entier is new Decode(P_Memoire => MemoireEntier, Capacite => 2);
    
    package Decode2Chaine is new Decode(P_Memoire => MemoireChaine, Capacite => 2);
    
        
    -- Test concernant l'instruction NULL
    procedure test_instruction_NULL is  
        use MemoireEntier; use Decode2Entier;
        
        Tab_Instruc : Decode2Entier.T_tab_instruc;
        MemoirePremierElement, MemoireX : MemoireEntier.T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        Decode2Entier.init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (deux lignes)
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x : Entier");
        Modifier(MemoirePremierElement, "x", 2);
        
        -- test : opération NULL
        Decode2Entier.effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        pragma Assert (CP = 2);
        MemoireX := RecupererVariable(MemoirePremierElement, "x");
        pragma Assert (MemoireX.all.Data = 2);       
        
    end;
    
    -- Test concernant l'instruction OP : addition de deux constantes (entieres)
    procedure test_instruction_addition_entier_const is  
        use MemoireEntier; use Decode2Entier;
        
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement, MemoireX : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (une ligne : "x <- 2 + 3")
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x : Entier");
        Modifier(MemoirePremierElement, "x", 2);
        
        -- test : opération add avec constantes
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        MemoireX := RecupererVariable(MemoirePremierElement, "x");
        pragma Assert (MemoireX.all.Data = 5);       
        
    end;
    
    
    -- Test concernant l'instruction OP : addition de deux variables (entieres)
    procedure test_instruction_addition_entier_var is  
        use MemoireEntier; use Decode2Entier;
        
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement, MemoireX : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (une ligne : "x <- x + y")
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "x", 2);
        Modifier(MemoirePremierElement, "y", 5);
        
        -- test : opération add avec variables
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        MemoireX := RecupererVariable(MemoirePremierElement, "x");
        pragma Assert (MemoireX.all.Data = 7);       
        
    end;
    
    
    -- Test concernant l'instruction OP : addition d'une variable et d'une constante (entieres)
    procedure test_instruction_addition_entier_mix is  
        use MemoireEntier; use Decode2Entier;
        
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement, MemoireX : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (une ligne : "x <- y + 3")
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "y", 5);
        
        -- test : opération add variable + constante
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        MemoireX := RecupererVariable(MemoirePremierElement, "x");
        pragma Assert (MemoireX.all.Data = 8);       
        
    end;
    
    
    -- Test concernant l'instruction OP : addition de deux constantes (string)
    procedure test_instruction_addition_chaine_const is  
        use MemoireChaine; use Decode2Chaine;
        
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement, MemoireMaChaine : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (une ligne : "maChaine <- "1" + "2")
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "maChaine : Chaine");
        Modifier(MemoirePremierElement, "maChaine", "uneChaine");
        
        -- test : opération add chaines constantes
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        MemoireMaChaine := RecupererVariable(MemoirePremierElement, "maChaine");
        pragma Assert (MemoireMaChaine.all.Data = "12");       
        
    end;
    
    -- Test concernant l'instruction OP : addition de deux variables (string)
    procedure test_instruction_addition_chaine_var is  
        use MemoireChaine; use Decode2Chaine;
        
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement, MemoireMaChaine : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (une ligne : "maChaine <- chaineA + chaineB")
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "maChaine, chaineA, chaineB : Chaine");
        Modifier(MemoirePremierElement, "maChaine", "uneChaine");
        Modifier(MemoirePremierElement, "chaineA", "aa");
        Modifier(MemoirePremierElement, "chaineB", "22");
        
        -- test : opération add chaines variables
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        MemoireMaChaine := RecupererVariable(MemoirePremierElement, "x");
        pragma Assert (MemoireMaChaine.all.Data = "aa22");       
        
    end;
    
    
    -- Test concernant l'instruction OP : addition d'une variable et d'une constante (string)
    procedure test_instruction_addition_chaine_mix is  
        use MemoireChaine; use Decode2Chaine;
        
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement, MemoireMaChaine : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (une ligne : "maChaine <- maChaine + " "")
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "maChaine : Chaine");
        Modifier(MemoirePremierElement, "maChaine", "bonjour");
        
        -- test : opération NULL
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        MemoireMaChaine := RecupererVariable(MemoirePremierElement, "maChaine");
        pragma Assert (MemoireMaChaine.all.Data = "bonjour ");       
        
    end;
    
    -- Test concernant l'instruction OP : soustraction de deux constantes (entieres)
    procedure test_instruction_soustraction_const is  
        use MemoireEntier; use Decode2Entier;
        
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement, MemoireX : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (une ligne : "x <- 2 - 5")
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x : Entier");
        Modifier(MemoirePremierElement, "x", 4);
        
        -- test : opération soustraction avec constantes
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        MemoireX := RecupererVariable(MemoirePremierElement, "x");
        pragma Assert (MemoireX.all.Data = -3);       
        
    end;
    
    
    -- Test concernant l'instruction OP : soustraction de deux variables (entieres)
    procedure test_instruction_soustraction_var is  
        use MemoireEntier; use Decode2Entier;
        
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement, MemoireX : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (une ligne : "x <- x - y")
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "x", 5);
        Modifier(MemoirePremierElement, "y", 2);
        
        -- test : opération soustraction avec variables
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        MemoireX := RecupererVariable(MemoirePremierElement, "x");
        pragma Assert (MemoireX.all.Data = 3);       
        
    end;
    
    
    -- Test concernant l'instruction OP : soustraction d'une variable et d'une constante (entieres)
    procedure test_instruction_soustraction_mix is  
        use MemoireEntier; use Decode2Entier;
        
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement, MemoireX : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (une ligne : "x <- y - 3")
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "y", 5);
        
        -- test : opération soustraction variable + constante
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        MemoireX := RecupererVariable(MemoirePremierElement, "x");
        pragma Assert (MemoireX.all.Data = 2);       
        
    end;
    
    -- Test concernant l'instruction OP : multiplication de deux constantes (entieres)
    procedure test_instruction_multiplication_const is  
        use MemoireEntier; use Decode2Entier;
        
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement, MemoireX : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (une ligne : "x <- 2 * 3")
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x : Entier");
        Modifier(MemoirePremierElement, "x", 4);
        
        -- test : opération multiplication avec constantes
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        MemoireX := RecupererVariable(MemoirePremierElement, "x");
        pragma Assert (MemoireX.all.Data = 6);       
        
    end;
    
    
    -- Test concernant l'instruction OP : multiplication de deux variables (entieres)
    procedure test_instruction_multiplication_var is  
        use MemoireEntier; use Decode2Entier;
        
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement, MemoireX : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (une ligne : "x <- x + y")
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "x", 2);
        Modifier(MemoirePremierElement, "y", 0);
        
        -- test : opération multiplication avec variables
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        MemoireX := RecupererVariable(MemoirePremierElement, "x");
        pragma Assert (MemoireX.all.Data = 0);       
        
    end;
    
    
    -- Test concernant l'instruction OP : multiplication d'une variable et d'une constante (entieres)
    procedure test_instruction_multiplication_mix is  
        use MemoireEntier; use Decode2Entier;
        
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement, MemoireX : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc);--remplir avec les instructions (une ligne : "x <- y * 4")
        CP := 1;
        
        --initialisation de la mémoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "y", 2);
        
        -- test : opération multiplication variable + constante
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- vérifications
        MemoireX := RecupererVariable(MemoirePremierElement, "x");
        pragma Assert (MemoireX.all.Data = 8);       
        
    end;
    
begin
    Null;
    
   
end test_decode;
