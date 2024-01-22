with Decode; 
with Memoire;     use Memoire;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Utils; use Utils;

procedure test_decode is
    
    File_Name : constant String := "temp_instruct.txt";
    
    package Decode2Entier is new Decode(Capacite => 2);
      
        
    -- Test concernant l'instruction NULL
    procedure test_instruction_NULL is  
        use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp); 
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "X : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "X <- 3");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;

        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);    
        Modifier_Entier(Memoire, To_Unbounded_String("X"), 2);
        -- test : operation NULL
        Decode2Entier.effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        pragma Assert (CP = 2);
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("X"));
        pragma Assert (X_value = 2); 
        
    end;
    
    
    -- Test concernant l'instruction d'affectation
    procedure test_instruction_affectation is
        use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        CP : Integer;
        Valeur_X : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- 3");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        -- initialisation memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 0); --variable initialisee Ã  0
        
        -- test : operation division variable + constante
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        Valeur_X := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (Valeur_X = 3);  -- la valeur de X a bien ete modifiee
        pragma Assert (CP = 2); -- CP a bien ete augmente
        
    end;
    
    
    -- Test concernant l'instruction OP : addition de deux constantes (entieres)
    procedure test_instruction_addition_entier_const is  
        use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- 2 + 3");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 2);
        
        -- test : operation add avec constantes
        effectuer_instru(Tab_Instruc, CP, Memoire);

        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 5);       
        pragma Assert (CP = 2); -- CP a bien ete augmente
    end;
    
    
    -- Test concernant l'instruction OP : addition de deux variables (entieres)
    procedure test_instruction_addition_entier_var is  
        use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x, y : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- x + y");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 2);
        Modifier_Entier(Memoire, To_Unbounded_String("y"), 5);
        
        -- test : operation add avec variables
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 7);       
        pragma Assert (CP = 2); -- CP a bien ete augmente
    end;
    
    
    -- Test concernant l'instruction OP : addition d'une variable et d'une constante (entieres)
    procedure test_instruction_addition_entier_mix is  
        use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x, y : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- y + 3");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("y"), 5);
        
        -- test : operation add variable + constante
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 8);       
        pragma Assert (CP = 2); -- CP a bien ete augmente
    end;
    

    -- Test concernant l'instruction OP : soustraction de deux constantes (entieres)
    procedure test_instruction_soustraction_const is  
        use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin

        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- 2 - 5");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 4);
        
        -- test : operation soustraction avec constantes
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = -3);       
        pragma Assert (CP = 2); -- CP a bien ete augmente
    end;
    
    
    -- Test concernant l'instruction OP : soustraction de deux variables (entieres)
    procedure test_instruction_soustraction_var is  
        use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
      
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x, y : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- x - y");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        
        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 5);
        Modifier_Entier(Memoire, To_Unbounded_String("y"), 2);
        
        -- test : operation soustraction avec variables
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 3);       
        pragma Assert (CP = 2); -- CP a bien ete augmente
    end;
    
    
    -- Test concernant l'instruction OP : soustraction d'une variable et d'une constante (entieres)
    procedure test_instruction_soustraction_mix is  
        use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
     
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x, y : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- y - 3");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("y"), 5);
        
        -- test : operation soustraction variable + constante
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 2);       
        pragma Assert (CP = 2); -- CP a bien ete augmente
    end;
    
    -- Test concernant l'instruction OP : multiplication de deux constantes (entieres)
    procedure test_instruction_multiplication_const is  
        use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- 2 * 3");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 4);
        
        -- test : operation multiplication avec constantes
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 6);       
        pragma Assert (CP = 2); -- CP a bien ete augmente
    end;
    
    
    -- Test concernant l'instruction OP : multiplication de deux variables (entieres)
    procedure test_instruction_multiplication_var is  
         use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x, y : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- x * y");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 2);
        Modifier_Entier(Memoire, To_Unbounded_String("y"), 0);
        
        -- test : operation multiplication avec variables
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 0);       
        pragma Assert (CP = 2); -- CP a bien ete augmente
    end;
    
    
    -- Test concernant l'instruction OP : multiplication d'une variable et d'une constante (entieres)
    procedure test_instruction_multiplication_mix is  
         use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
                   
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x, y : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- y * 4");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("y"), 2);
        
        -- test : operation multiplication variable + constante
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 8);       
        pragma Assert (CP = 2); -- CP a bien ete augmente
    end;
    
     -- Test concernant l'instruction OP : division de deux constantes (entieres)
    procedure test_instruction_division_const is  
         use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- 4 / 2");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 3);
        
        -- test : operation division avec constantes
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 2);       
        pragma Assert (CP = 2); -- CP a bien ete augmente
    end;
    
    -- Test concernant l'instruction OP : cas d'exception de la division par zero
    procedure test_instruction_division_parzero is
         use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        CP : Integer;
        est_leve : Boolean; --indique si l'exception est levee
    
    begin
        est_leve := False;
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- 4 / 0");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
    
        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);
    
        begin
            -- test : operation division par zero (doit lever une erreur)
            effectuer_instru(Tab_Instruc, CP, Memoire);
            
        exception
            when Constraint_Error => 
                est_leve := True;
        end;
    
        -- verifications
        pragma Assert (est_leve);
    end;
    
    
    -- Test concernant l'instruction OP : division de deux variables (entieres)
    procedure test_instruction_division_entier_var is  
         use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x, y : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- x / y");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 6);
        Modifier_Entier(Memoire, To_Unbounded_String("y"), 2);
        
        -- test : operation division avec variables
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 3);       
        pragma Assert (CP = 2); -- CP a bien ete augmente
    end;
    
    
    -- Test concernant l'instruction OP : division d'une variable et d'une constante (entieres)
    procedure test_instruction_division_entier_mix is  
         use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x, y : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- y / 3");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        --initialisation de la memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("y"), 6);
        
        -- test : operation division variable + constante
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 2);       
        pragma Assert (CP = 2); -- CP a bien ete augmente
    end;
    
    
    -- Test concernant l'instruction GOTO avec un numero de ligne anterieur
    procedure test_instruction_goto_ligne_ante is  
         use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "x : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "x <- 3");
        Put_Line (Fichier_temp, "GOTO 1");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 2;
        
        -- initialisation memoire
        DeclarerVariables(Memoire, File_Name);
        
        -- test : operation division variable + constante
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        pragma Assert (CP = 1);       
        
    end;
    
    
    -- Test concernant l'instruction GOTO avec un numero de ligne posterieur
    procedure test_instruction_goto_ligne_post is
        package Decode5Entier is new Decode(Capacite => 5);
        use Decode5Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "GOTO 5");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        -- initialisation memoire
        DeclarerVariables(Memoire, File_Name);
        
        -- test : operation division variable + constante
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        pragma Assert (CP = 5);       
        
    end;
    
    -- Test concernant l'instruction GOTO avec un numero de ligne invalide (<1)
    procedure test_instruction_goto_invalide_inf is
         use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        CP : Integer;
        est_leve : Boolean;
        
    begin
        est_leve := False;    
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "GOTO 0");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        -- initialisation memoire
        DeclarerVariables(Memoire, File_Name);
        
        begin
            -- test : operation goto invalide
            effectuer_instru(Tab_Instruc, CP, Memoire); 
            
        exception
            when GOTO_OUT_OF_RANGE_EXCEPTION => 
                est_leve := True;    
        end;
        
        -- verifications
        pragma Assert (est_leve);
    
    end;
    
    -- Test concernant l'instruction GOTO avec un numero de ligne invalide (> Capacite de Tab_instruc)
    procedure test_instruction_goto_invalide_sup is
         use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        CP : Integer;
        est_leve : Boolean;
        
    begin
        est_leve := False;    
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "GOTO 3");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        -- initialisation memoire
        DeclarerVariables(Memoire, File_Name);
        
        begin
            -- test : operation goto invalide
            effectuer_instru(Tab_Instruc, CP, Memoire); 
            
        exception
            when GOTO_OUT_OF_RANGE_EXCEPTION => 
                est_leve := True;    
        end;
        
        -- verifications
        pragma Assert (est_leve);
    
    end;
    
    -- Test concernant l'instruction IF avec une condition = true (le goto prend effet)
    procedure test_instruction_if_true is
        package Decode5Entier is new Decode(Capacite => 5);
         use Decode5Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "T1 : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "IF T1 GOTO 5");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        -- initialisation memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("t1"), 1); --valeur autre que 0 donc True

        -- test : operation division variable + constante
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        pragma Assert (CP = 5);    -- consequence du goto   
        
    end;
    
    
    -- Test concernant l'instruction IF avec une condition = false (le goto ne prend pas effet)
    procedure test_instruction_if_false is
        package Decode5Entier is new Decode(Capacite => 5);
         use Decode5Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        Memoire : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme Test est");
        Put_Line (Fichier_temp, "T1 : Entier");
        Put_Line (Fichier_temp, "Début");
        Put_Line (Fichier_temp, "IF T1 GOTO 3");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "Fin");
        Close(Fichier_temp);
        remplir_tab_instruc(Tab_Instruc, File_Name);
        CP := 1;
        
        -- initialisation memoire
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("t1"), 0); --valeur 0 donc False
        
        -- test : operation division variable + constante
        effectuer_instru(Tab_Instruc, CP, Memoire);
        
        -- verifications
        pragma Assert (CP = 2);  -- le CP est incremente de 1 car le goto ne prend pas effet   
        
    end;
        
   
   -- Test pour verifier qu'Ã  l'initialisation cp vaut 1
   procedure test_initialisation_cp is
      use Decode2Entier;
      cp : Integer;
   begin
      -- Initialisation de cp
      init_CP(cp);      
      -- Verification de si cp vaut bien 1
      pragma Assert(cp = 1);
   end;
   
   -- Test qui permet de verifier que le tableau d'insructions est correctement rempli
   procedure test_remplir_tab_instruc is
      package Decode16Entier is new Decode(Capacite => 16);
      use Decode16Entier;
      
      tab : T_tab_instruc;
      fichier : File_Type;
      
   begin
      -- Creation d'un fichier avec du code intermediaire
      createFileInstruct(fichier);
      Put_Line(fichier, "-- Ce code intermediaire calcule la factorielle F");
      Put_Line(fichier, "-- d'un entier n avec numeros de ligne");
      Put_Line(fichier, "Programme Facto est");
      Put_Line(fichier, "n, i, Fact : Entier");
      Put_Line(fichier, "T1, T2, T3 : Booleen");
      Put_Line(fichier, "Début");
      Put_Line(fichier, "n <- 5");
      Put_Line(fichier, "i <- 1");
      Put_Line(fichier, "Fact <- 1");
      Put_Line(fichier, "T1 <- i < n");
      Put_Line(fichier, "T2 <- i = n");
      Put_Line(fichier, "T3 <- T1 OR T2");
      Put_Line(fichier, "IF T3 GOTO 9");
      Put_Line(fichier, "GOTO 15");
      Put_Line(fichier, "Fact <- Fact * i");
      Put_Line(fichier, "i <- i + 1");
      Put_Line(fichier, "T1 <- i < n");
      Put_Line(fichier, "T2 <- i = n");
      Put_Line(fichier, "T3 <- T1 OR T2");
      Put_Line(fichier, "GOTO 7");
      Put_Line(fichier, "NULL");
      Put_Line(fichier, "Fin");
      Close(fichier);
      -- Remplissage du tableau d'instructions
      remplir_tab_instruc(tab, File_Name);
      
      pragma Assert(recuperer_instru_pos1(tab, 1) = "n");
      pragma Assert(recuperer_instru_pos2(tab, 1) = "5");
      pragma Assert(recuperer_instru_pos1(tab, 2) = "i");
      pragma Assert(recuperer_instru_pos4(tab, 4) = "n");
      pragma Assert(recuperer_instru_pos3(tab, 4) = "<");
      pragma Assert(recuperer_instru_pos1(tab, 4) = "T1");
      pragma Assert(recuperer_instru_pos1(tab, 8) = "GOTO");
      pragma Assert(recuperer_instru_pos2(tab, 8) = "15");
      pragma Assert(recuperer_instru_pos1(tab, 15) = "NULL");
   end;

   
    
begin
    test_instruction_NULL;
    test_instruction_affectation;
    test_instruction_addition_entier_const;
    test_instruction_addition_entier_var;
    test_instruction_addition_entier_mix;
    test_instruction_soustraction_const;
    test_instruction_soustraction_var;
    test_instruction_soustraction_mix;
    test_instruction_multiplication_const;
    test_instruction_multiplication_var;
    test_instruction_multiplication_mix;
    test_instruction_division_const;
    test_instruction_division_parzero;
    test_instruction_division_entier_var;
    test_instruction_division_entier_mix;
    test_instruction_goto_ligne_ante;
    test_instruction_goto_ligne_post;
    test_instruction_goto_invalide_inf;
    test_instruction_goto_invalide_sup;
    test_instruction_if_true;
    test_instruction_if_false;
    test_initialisation_cp;
    test_remplir_tab_instruc;
    deleteFileInstruct(File_Name);
   
end test_decode;
