with Memoire; use Memoire;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Utils; use Utils;

procedure test_memoire is
    File_Name : constant String := "temp_instruct.txt";
       
   
    -- Test concernant la declaration de variable contenant seulement des entiers
    procedure test_DeclarerVariables_entiers is
        Fichier_temp : File_Type; -- le fichier d'instruction
        Memoire : T_Memoire;
        X_value : Integer;
        Y_value : Integer;
    begin
        --initialisation de la memoire avec une variable
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme_test est");
        Put_Line (Fichier_temp, "x : Entier");
        Put_Line (Fichier_temp, "y : Entier");
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 2);
        Modifier_Entier(Memoire, To_Unbounded_String("y"), 5);
        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        Y_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("y"));
        pragma Assert (X_value = 2);
        pragma Assert (Y_value = 5);
              
        --initialisation de la memoire avec plusieurs variables
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme_test est");
        Put_Line (Fichier_temp, "x, y, z : Entier");
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("y"), 2);
        -- verifications
        Y_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("y"));
        pragma Assert (Y_value = 2);
    end;
   
    -- Test concernant la declaration de variables contenant des entiers et des booleens (traites de la meme maniere)
    procedure test_DeclarerVariables_booleens is
        Fichier_temp : File_Type; -- le fichier d'instruction
        Memoire : T_Memoire;
        X_value : Integer;
        T1_value : Integer;
    begin
        --initialisation de la memoire avec une variable
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme_test est");
        Put_Line (Fichier_temp, "x : Entier");
        Put_Line (Fichier_temp, "T1 : Booléen");
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 2);
        Modifier_Entier(Memoire, To_Unbounded_String("T1"), 0);
        -- verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        T1_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("T1"));
        pragma Assert (X_value = 2);
        pragma Assert (T1_value = 0);
    end;
    
     -- Test concernant la declaration de variables contenant des chaines et des caracteres (traites de la meme maniere)
    procedure test_DeclarerVariables_carac is
        Fichier_temp : File_Type; -- le fichier d'instruction
        Memoire : T_Memoire;
        X_value : Unbounded_String;
        Y_value : Unbounded_String;
    begin
        --initialisation de la memoire avec une variable
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme_test est");
        Put_Line (Fichier_temp, "x : Chaine");
        Put_Line (Fichier_temp, "y : Caractère");
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);
        DeclarerVariables(Memoire, File_Name);
        Modifier_Chaine(Memoire, To_Unbounded_String("x"), To_Unbounded_String("une chaine"));
        Modifier_Chaine(Memoire, To_Unbounded_String("y"), To_Unbounded_String("c"));
        -- verifications
        X_value := RecupererValeur_Chaine(Memoire, To_Unbounded_String("x"));
        Y_value := RecupererValeur_Chaine(Memoire, To_Unbounded_String("y"));
        pragma Assert (X_value = "une chaine");
        pragma Assert (Y_value = "c");
    end;
    
    
    -- Test concernant la recuperation de valeur et type.
    procedure test_Recuperer_Entier is  
        --use MemoireEntier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Memoire : T_Memoire;
        X_value : Integer;
        Y_value : Integer;
        X_type : Unbounded_String;
        Y_type : Unbounded_String;
    begin
        --initialisation de la memoire avec une seule variable
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme_test est");
        Put_Line (Fichier_temp, "x : Entier");
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 2);
        --verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 2);
        X_type := RecupererType(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_type = "Entier");
      
        --initialisation de la memoire avec plusieurs variables
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme_test est");
        Put_Line (Fichier_temp, "x, y, z : Entier");
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("y"), 2);
        --verifications
        Y_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("y"));
        pragma Assert (Y_value = 2);
        Y_type := RecupererType(Memoire, To_Unbounded_String("y"));
        pragma Assert (Y_type = "Entier");
    end;
    
    -- Test concernant la recuperation de valeur et type.
    procedure test_Recuperer_Chaine is  
        --use MemoireEntier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Memoire      : T_Memoire;
        X_value      : Unbounded_String;
        Y_value      : Unbounded_String;
        X_type       : Unbounded_String;
        Y_type       : Unbounded_String;
    begin
        --initialisation de la memoire avec une seule variable
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme_test est");
        Put_Line (Fichier_temp, "x : Chaine");
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);
        DeclarerVariables(Memoire, File_Name);
        Modifier_Chaine(Memoire, To_Unbounded_String("x"), To_Unbounded_String("chaine"));
        --verifications
        X_value := RecupererValeur_Chaine(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = "chaine");
        X_type := RecupererType(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_type = "Chaine");
      
        --initialisation de la memoire avec plusieurs variables
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme_test est");
        Put_Line (Fichier_temp, "x, y, z : Chaine");
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);
        DeclarerVariables(Memoire, File_Name);
        Modifier_Chaine(Memoire, To_Unbounded_String("y"), To_Unbounded_String("nouvelle valeur"));
        --verifications
        Y_value := RecupererValeur_Chaine(Memoire, To_Unbounded_String("y"));
        pragma Assert (Y_value = "nouvelle valeur");
        Y_type := RecupererType(Memoire, To_Unbounded_String("y"));
        pragma Assert (Y_type = "Chaine");
    end;
    
    -- Test concernant la recuperation de chaines et entiers melanges
    procedure test_Recuperer_Chaine_Entier_Melange is  
        --use MemoireEntier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Memoire      : T_Memoire;
        X_value      : Integer;
        Y_value      : Unbounded_String;
        Z_value      : Integer;
    begin
        --initialisation de la memoire avec une seule variable
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme_test est");
        Put_Line (Fichier_temp, "x : Entier");
        Put_Line (Fichier_temp, "y : Chaine");
        Put_Line (Fichier_temp, "z : Entier");
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 3);
        Modifier_Chaine(Memoire, To_Unbounded_String("y"), To_Unbounded_String("chaine"));
        Modifier_Entier(Memoire, To_Unbounded_String("z"), 5);
        --verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 3);
        Y_value := RecupererValeur_Chaine(Memoire, To_Unbounded_String("y"));
        pragma Assert (Y_value = "chaine");
        Z_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("z"));
        pragma Assert (Z_value = 5);
      
    end;
   
    -- Test concernant la modification de variable entiere.
    procedure test_ModifierVariables_Entier is  
        --use MemoireEntier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Memoire : T_Memoire;
        X_value : Integer;
    begin
        --initialisation de la memoire avec une variable
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme_test est");
        Put_Line (Fichier_temp, "x : Entier");
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);  
        DeclarerVariables(Memoire, File_Name);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 2);
        --verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 2);
        Modifier_Entier(Memoire, To_Unbounded_String("x"), 5);
        --verifications
        X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = 5);
    end;
   
    -- Test concernant la modification de variable string.
    procedure test_ModifierVariables_Chaine is  
        --use MemoireEntier;
        Fichier_temp : File_Type; -- le fichier d'instruction
        Memoire : T_Memoire;
        X_value : Unbounded_String;
    begin
        --initialisation de la memoire avec une variable
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme_test est");
        Put_Line (Fichier_temp, "x : Chaine");
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);  
        DeclarerVariables(Memoire, File_Name);
        Modifier_Chaine(Memoire, To_Unbounded_String("x"), To_Unbounded_String("hello"));
        --verifications
        X_value := RecupererValeur_Chaine(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = "hello");
        Modifier_Chaine(Memoire, To_Unbounded_String("x"), To_Unbounded_String("new string"));
        --verifications
        X_value := RecupererValeur_Chaine(Memoire, To_Unbounded_String("x"));
        pragma Assert (X_value = "new string");
    end;
    
    
    -- Test concernant la modification de variable string.
    procedure test_RecupererType is  
        Fichier_temp : File_Type; -- le fichier d'instruction
        Memoire : T_Memoire;
    begin
        --initialisation de la memoire avec une variable
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme_test est");
        Put_Line (Fichier_temp, "x : Chaine");
        Put_Line (Fichier_temp, "y, z : Entier");
        Put_Line (Fichier_temp, "estBooleen : Booléen");
        Put_Line (Fichier_temp, "unCarac : Caractère");
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);  
        DeclarerVariables(Memoire, File_Name);
        --verifications
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("x")) = To_Unbounded_String("Chaine"));
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("y")) = To_Unbounded_String("Entier"));
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("z")) = To_Unbounded_String("Entier"));
        -- les booleens sont consideres comme des entiers
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("estBooleen")) = To_Unbounded_String("Entier"));
        -- les caracteres sont consideres comme des chaines
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("unCarac")) = To_Unbounded_String("Chaine"));
    end;
    
    -- Test concernant la modification de variable string.
    procedure test_RecupererTypeTableaux is  
        Fichier_temp : File_Type; -- le fichier d'instruction
        Memoire : T_Memoire;
    begin
        --initialisation de la memoire avec une variable
        createFileInstruct(Fichier_temp);
        Put_Line (Fichier_temp, "Programme_test est");
        Put_Line (Fichier_temp, "Type T_Tab_Entier est tableau (1..5) d'entiers");
        Put_Line (Fichier_temp, "Type T_Tab_Chaine est tableau (1..5) de chaines");
        Put_Line (Fichier_temp, "Type T_Tab_Booleen est tableau (5..10) de booléens");
        Put_Line (Fichier_temp, "Type T_Tab_Carac est tableau (2..5) de caractères");
        Put_Line (Fichier_temp, "Tab1, Tab2 : T_Tab_Entier");
        Put_Line (Fichier_temp, "Tab3, Tab4 : T_Tab_Chaine");
        Put_Line (Fichier_temp, "Tab5, Tab6 : T_Tab_Booleen");
        Put_Line (Fichier_temp, "Tab7, Tab8 : T_Tab_Carac");
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);  
        DeclarerVariables(Memoire, File_Name);
        --verifications
        -- les tableaux d'entiers
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("Tab1(1)")) = To_Unbounded_String("TabEntier"));
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("Tab2(3)")) = To_Unbounded_String("TabEntier"));
        -- les tableaux de chaines
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("Tab3(1)")) = To_Unbounded_String("TabChaine"));
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("Tab4(5)")) = To_Unbounded_String("TabChaine"));
        -- les tableaux de booleens
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("Tab5(6)")) = To_Unbounded_String("TabEntier"));
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("Tab6(9)")) = To_Unbounded_String("TabEntier"));
        -- les tableaux de caractères
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("Tab7(2)")) = To_Unbounded_String("TabChaine"));
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("Tab8(4)")) = To_Unbounded_String("TabChaine"));
        

    end;
   
begin
    test_DeclarerVariables_entiers;
    test_DeclarerVariables_booleens;
    test_DeclarerVariables_carac;
    test_Recuperer_Entier;
    test_Recuperer_Chaine;
    test_Recuperer_Chaine_Entier_Melange;
    test_ModifierVariables_Entier;
    test_ModifierVariables_Chaine;
    test_RecupererType;
    test_RecupererTypeTableaux;
    deleteFileInstruct(File_Name);
end test_memoire;
