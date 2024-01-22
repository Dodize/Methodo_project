with Memoire; use Memoire;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Utils; use Utils;

procedure test_memoire is
    File_Name : constant String := "temp_instruct.txt";
   
   
   -- Test concernant la declaration de variable.
   procedure test_DeclarerVariables is
      Fichier_temp : File_Type; -- le fichier d'instruction
      Memoire : T_Memoire;
      X_value : Integer;
      Y_value : Integer;
   begin
      --initialisation de la memoire avec une variable
      createFileInstruct(Fichier_temp);
      Put_Line (Fichier_temp, "Programme_test est");
      Put_Line (Fichier_temp, "x : Entier");
      Put_Line (Fichier_temp, "Debut");
      Close(Fichier_temp);
      DeclarerVariables(Memoire, File_Name);
      Modifier_Entier(Memoire, To_Unbounded_String("x"), 2);
      -- verifications
      X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
      pragma Assert (X_value = 2);
      
      --initialisation de la memoire avec plusieurs variables
      createFileInstruct(Fichier_temp);
      Put_Line (Fichier_temp, "Programme_test est");
      Put_Line (Fichier_temp, "x, y, z : Entier");
      Put_Line (Fichier_temp, "Debut");
      Close(Fichier_temp);
      DeclarerVariables(Memoire, File_Name);
      Modifier_Entier(Memoire, To_Unbounded_String("y"), 2);
      -- verifications
      Y_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("y"));
      pragma Assert (Y_value = 2);
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
      Put_Line (Fichier_temp, "Debut");
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
        Put_Line (Fichier_temp, "Début");
        Close(Fichier_temp);  
        DeclarerVariables(Memoire, File_Name);
        --verifications
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("x")) = To_Unbounded_String("Chaine"));
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("y")) = To_Unbounded_String("Entier"));
        pragma Assert (RecupererType(Memoire, To_Unbounded_String("z")) = To_Unbounded_String("Entier"));
    end;
   
begin
    test_DeclarerVariables;
    test_Recuperer_Entier;
    test_Recuperer_Chaine;
    test_ModifierVariables_Entier;
    test_ModifierVariables_Chaine;
    test_RecupererType;
    deleteFileInstruct(File_Name);
end test_memoire;
