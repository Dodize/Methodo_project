with Decode; 
with Memoire;
with Ada.Text_IO; use Ada.Text_IO;

procedure test_decode is
    
    package MemoireEntier is new Memoire(T_Elt => Integer);
    
    package Decode2Entier is new Decode(P_Memoire => MemoireEntier, Capacite => 2);
    
    -- Procedure permettant de créer un fichier temporaire d'instructions pour réaliser les tests
    -- @param : Fichier le fichier créé
    procedure createFileInstruct(Fichier : out File_Type) is
        File_Name : constant String := "temp_instruct.txt";
    begin
        Create (Fichier, Out_File, File_Name);
    end;
    
    
    -- Procedure permettant de supprimer le fichier temporaire d'instructions pour réaliser les tests
    -- @param : Fichier le fichier à supprimer
    procedure deleteFileInstruct(Fichier : out File_Type) is
        File_Name : constant String := "temp_instruct.txt";
    begin
        Delete(Fichier);
    end;
    
    
    
    
        
    -- Test concernant l'instruction NULL
    procedure test_instruction_NULL is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : MemoireEntier.T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "x <- 3");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x : Entier");
        Modifier(MemoirePremierElement, "x", 2);
        
        -- test : operation NULL
        Decode2Entier.effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        pragma Assert (CP = 2);
        X_value := RecupererValeur(MemoirePremierElement, "x");
        pragma Assert (X_value = 2); 
        
        
    end;
    
    -- Test concernant l'instruction OP : addition de deux constantes (entieres)
    procedure test_instruction_addition_entier_const is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- 2 + 3");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x : Entier");
        Modifier(MemoirePremierElement, "x", 2);
        
        -- test : operation add avec constantes
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        X_value := RecupererValeur(MemoirePremierElement, "x");
        pragma Assert (X_value = 5);       
        
    end;
    
    
    -- Test concernant l'instruction OP : addition de deux variables (entieres)
    procedure test_instruction_addition_entier_var is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- x + y");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "x", 2);
        Modifier(MemoirePremierElement, "y", 5);
        
        -- test : operation add avec variables
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        X_value := RecupererValeur(MemoirePremierElement, "x");
        pragma Assert (X_value = 7);       
        
    end;
    
    
    -- Test concernant l'instruction OP : addition d'une variable et d'une constante (entieres)
    procedure test_instruction_addition_entier_mix is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- y + 3");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "y", 5);
        
        -- test : operation add variable + constante
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        X_value := RecupererValeur(MemoirePremierElement, "x");
        pragma Assert (X_value = 8);       
        
    end;
    

    -- Test concernant l'instruction OP : soustraction de deux constantes (entieres)
    procedure test_instruction_soustraction_const is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin

        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- 2 - 5");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x : Entier");
        Modifier(MemoirePremierElement, "x", 4);
        
        -- test : operation soustraction avec constantes
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        X_value := RecupererValeur(MemoirePremierElement, "x");
        pragma Assert (X_value = -3);       
        
    end;
    
    
    -- Test concernant l'instruction OP : soustraction de deux variables (entieres)
    procedure test_instruction_soustraction_var is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
      
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- x - y");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "x", 5);
        Modifier(MemoirePremierElement, "y", 2);
        
        -- test : operation soustraction avec variables
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        X_value := RecupererValeur(MemoirePremierElement, "x");
        pragma Assert (X_value = 3);       
        
    end;
    
    
    -- Test concernant l'instruction OP : soustraction d'une variable et d'une constante (entieres)
    procedure test_instruction_soustraction_mix is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
     
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- y - 3");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "y", 5);
        
        -- test : operation soustraction variable + constante
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        X_value := RecupererValeur(MemoirePremierElement, "x");
        pragma Assert (X_value = 2);       
        
    end;
    
    -- Test concernant l'instruction OP : multiplication de deux constantes (entieres)
    procedure test_instruction_multiplication_const is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- 2 * 3");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x : Entier");
        Modifier(MemoirePremierElement, "x", 4);
        
        -- test : operation multiplication avec constantes
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        X_value := RecupererValeur(MemoirePremierElement, "x");
        pragma Assert (X_value = 6);       
        
    end;
    
    
    -- Test concernant l'instruction OP : multiplication de deux variables (entieres)
    procedure test_instruction_multiplication_var is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- x * y");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "x", 2);
        Modifier(MemoirePremierElement, "y", 0);
        
        -- test : operation multiplication avec variables
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        X_value := RecupererValeur(MemoirePremierElement, "x");
        pragma Assert (X_value = 0);       
        
    end;
    
    
    -- Test concernant l'instruction OP : multiplication d'une variable et d'une constante (entieres)
    procedure test_instruction_multiplication_mix is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
                   
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- y * 4");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "y", 2);
        
        -- test : operation multiplication variable + constante
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        X_value := RecupererValeur(MemoirePremierElement, "x");
        pragma Assert (X_value = 8);       
        
    end;
    
     -- Test concernant l'instruction OP : division de deux constantes (entieres)
    procedure test_instruction_division_const is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- 4 / 2");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x : Entier");
        Modifier(MemoirePremierElement, "x", 3);
        
        -- test : operation division avec constantes
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        X_value := RecupererValeur(MemoirePremierElement, "x");
        pragma Assert (X_value = 2);       
        
    end;
    
    -- Test concernant l'instruction OP : cas d'exception de la division par zero
    procedure test_instruction_division_parzero is
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        CP : Integer;
        est_leve : Boolean; --indique si l'exception est levee
    
    begin
        est_leve := False;
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- 4 / 0");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
    
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x : Entier");
    
        begin
            -- test : operation division par zero (doit lever une erreur)
            effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
            
        exception
            when Constraint_Error => 
                est_leve := True;
        end;
    
        -- verifications
        deleteFileInstruct(Fichier_temp);
        pragma Assert (est_leve = True);
    
    end;
    
    
    -- Test concernant l'instruction OP : division de deux variables (entieres)
    procedure test_instruction_division_entier_var is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- x / y");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "x", 6);
        Modifier(MemoirePremierElement, "y", 2);
        
        -- test : operation division avec variables
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        X_value := RecupererValeur(MemoirePremierElement, "x");
        pragma Assert (X_value = 3);       
        
    end;
    
    
    -- Test concernant l'instruction OP : division d'une variable et d'une constante (entieres)
    procedure test_instruction_division_entier_mix is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        X_value : Integer;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- y / 3");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        --initialisation de la memoire
        Initialiser(MemoirePremierElement);
        DeclarerVariables(MemoirePremierElement, "x, y : Entier");
        Modifier(MemoirePremierElement, "y", 6);
        
        -- test : operation division variable + constante
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        X_value := RecupererValeur(MemoirePremierElement, "x");
        pragma Assert (X_value = 2);       
        
    end;
    
    
    -- Test concernant l'instruction GOTO avec un numero de ligne anterieur
    procedure test_instruction_goto_ligne_ante is  
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "x <- 3");
        Put_Line (Fichier_temp, "GOTO 1");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 2;
        
        -- initialisation memoire
        Initialiser(MemoirePremierElement);
        
        -- test : operation division variable + constante
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        pragma Assert (CP = 1);       
        
    end;
    
    
    -- Test concernant l'instruction GOTO avec un numero de ligne posterieur
    procedure test_instruction_goto_ligne_post is
        package Decode5Entier is new Decode(P_Memoire => MemoireEntier, Capacite => 5);
        use MemoireEntier; use Decode5Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        CP : Integer;
        
    begin
              
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "GOTO 5");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "NULL");
        Put_Line (Fichier_temp, "NULL");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        -- initialisation memoire
        Initialiser(MemoirePremierElement);
        
        -- test : operation division variable + constante
        effectuer_instru(Tab_Instruc, CP, MemoirePremierElement);
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        pragma Assert (CP = 5);       
        
    end;
    
    -- Test concernant l'instruction GOTO avec un numero de ligne invalide (<1)
    procedure test_instruction_goto_invalide_inf is
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        CP : Integer;
        est_leve : Boolean;
        
    begin
        est_leve := False;    
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "GOTO 0");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        -- initialisation memoire
        Initialiser(MemoirePremierElement);
        
        begin
            -- test : operation goto invalide
            effectuer_instru(Tab_Instruc, CP, MemoirePremierElement); 
            
        exception
            when GOTO_OUT_OF_RANGE_EXCEPTION => 
                est_leve := True;    
        end;
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        pragma Assert (est_leve = True);
    
    end;
    
    -- Test concernant l'instruction GOTO avec un numero de ligne invalide (> Capacite de Tab_instruc)
    procedure test_instruction_goto_invalide_sup is
        use MemoireEntier; use Decode2Entier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Tab_Instruc : T_tab_instruc;
        MemoirePremierElement : T_Memoire;
        CP : Integer;
        est_leve : Boolean;
        
    begin
        est_leve := False;    
        --initialisation du tableau d'instruction
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "GOTO 10");
        init_tab_instruc(Tab_Instruc); 
        remplir_tab_instruc(Tab_Instruc, Fichier_temp);
        CP := 1;
        
        -- initialisation memoire
        Initialiser(MemoirePremierElement);
        
        begin
            -- test : operation goto invalide
            effectuer_instru(Tab_Instruc, CP, MemoirePremierElement); 
            
        exception
            when GOTO_OUT_OF_RANGE_EXCEPTION => 
                est_leve := True;    
        end;
        
        -- verifications
        deleteFileInstruct(Fichier_temp);
        pragma Assert (est_leve = True);
    
   end;
   
   -- Test pour verifier qu'à l'initialisation un T_tab_instruc est null
   procedure test_initialisation_t_tab_instruc is
      use Decode2Entier;
      
      Tab_Instruc : T_tab_instruc;
      
   begin
      -- Initialisation du tableau d'instruction
      init_tab_instruc(Tab_Instruc);
      -- Verification de si le tableau est bien vide
      pragma Assert(est_null(Tab_Instruc));
   end;
   
   -- Test pour verifier qu'à l'initialisation cp vaut 1
   procedure test_initialisation_cp is
      use Decode2Entier;
      
      cp : Integer;
      
   begin
      -- Initialisation de cp
      init_CP(cp);      
      -- Verification de si cp vaut bien 1
      pragma Assert(cp = 1);
   end;
   
   -- Test pour verifier que cp est incremente de 1 apres utilisation de la procedure increm_CP
   procedure test_increm_cp is
      use Decode2Entier;
      
      current_cp : Integer;
      old_cp : Integer;
      
   begin
      -- Initialisation de current_cp
      init_CP(current_cp);
      -- Stockage de sa valeur dans old_cp
      old_cp := current_cp;
      -- Incrémentation de current_cp
      increm_CP(current_cp);
      -- Verification de si current_cp vaut bien old_cp + 1
      pragma Assert(current_cp = (old_cp + 1));
   end;
    
begin
    Null;
    
   
end test_decode;
