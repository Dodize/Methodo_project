with Memoire; use Memoire;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;

procedure test_memoire is
    File_Name : constant String := "temp_instruct.txt";
   
   -- Procedure permettant de creer un fichier temporaire d'instructions pour realiser les tests
   -- @param : Fichier le fichier cree
   procedure createFileInstruct(Fichier : out File_Type) is
      File_Name : constant String := "temp_instruct.txt";
    begin
      Create (Fichier, Out_File, File_Name);
   end;
   
   -- Procedure permettant de supprimer le fichier temporaire d'instructions pour realiser les tests
   -- @param : Fichier le fichier a  supprimer
   procedure deleteFileInstruct(Fichier : out File_Type) is
   begin
      Delete(Fichier);
   end;
   
   -- Test concernant la declaration de variable.
   procedure test_DeclarerVariables is
      Fichier_temp : File_Type; -- le fichier d'instruction
      Memoire : T_Memoire;
      X_value : Integer;
      Y_value : Integer;
      File_Name : constant String := "temp_instruct.txt";
   begin
      --initialisation de la memoire avec une variable
      createFileInstruct(Fichier_temp);
      Put_Line (Fichier_temp, "Programme_test est");
      Put_Line (Fichier_temp, "x : Entier");
      Put_Line (Fichier_temp, "Début");
      Close(Fichier_temp);
      Open(Fichier_temp, In_File, File_Name); 
      DeclarerVariables(Memoire, Fichier_temp);   
      Modifier_Entier(Memoire, To_Unbounded_String("x"), 2);   
      -- verifications
      X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
      pragma Assert (X_value = 2);
      deleteFileInstruct(Fichier_temp);      
      
      --initialisation de la memoire avec plusieurs variables
      createFileInstruct(Fichier_temp);
      Put_Line (Fichier_temp, "Programme_test est");
      Put_Line (Fichier_temp, "x, y, z : Entier");
      Put_Line (Fichier_temp, "Début");
      Close(Fichier_temp);
      Open(Fichier_temp, In_File, File_Name);
      DeclarerVariables(Memoire, Fichier_temp);
      Modifier_Entier(Memoire, To_Unbounded_String("y"), 2);
      -- verifications
      Y_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("y"));
      pragma Assert (Y_value = 2);
      deleteFileInstruct(Fichier_temp);
   end;
   
    
   -- Test concernant la recuperation de valeur et type.
   procedure test_Recuperer is  
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
      Open(Fichier_temp, In_File, File_Name); 
      DeclarerVariables(Memoire, Fichier_temp);
      Modifier_Entier(Memoire, To_Unbounded_String("x"), 2);
      --verifications
      X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
      pragma Assert (X_value = 2);
      X_type := RecupererType(Memoire, To_Unbounded_String("x"));
      pragma Assert (X_type = "Entier");
      deleteFileInstruct(Fichier_temp);
      
      --initialisation de la memoire avec plusieurs variables
      createFileInstruct(Fichier_temp);
      Put_Line (Fichier_temp, "Programme_test est");
      Put_Line (Fichier_temp, "x, y, z : Entier");
      Put_Line (Fichier_temp, "Début");
      Close(Fichier_temp);
      Open(Fichier_temp, In_File, File_Name);
      DeclarerVariables(Memoire, Fichier_temp);
      Modifier_Entier(Memoire, To_Unbounded_String("y"), 2);
      --verifications
      Y_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("y"));
      pragma Assert (Y_value = 2);
      Y_type := RecupererType(Memoire, To_Unbounded_String("y"));
      pragma Assert (Y_type = "Entier");
      deleteFileInstruct(Fichier_temp);
   end;
   
   -- Test concernant la modification de variable.
   procedure test_ModifierVariables is  
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
      Open(Fichier_temp, In_File, File_Name);
      DeclarerVariables(Memoire, Fichier_temp);
      Modifier_Entier(Memoire, To_Unbounded_String("x"), 2);
      --verifications
      X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
      pragma Assert (X_value = 2);
      Modifier_Entier(Memoire, To_Unbounded_String("x"), 5);
      --verifications
      X_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("x"));
      pragma Assert (X_value = 5);
      deleteFileInstruct(Fichier_temp);
   end;
   
begin
   test_DeclarerVariables;
   test_Recuperer;
   test_ModifierVariables;
end test_memoire;
