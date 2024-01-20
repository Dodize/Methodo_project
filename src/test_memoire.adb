with Memoire; use Memoire;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Utils; use Utils;

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
    procedure deleteFileInstruct(NomFichier : in String) is
        Fichier : File_Type;
    begin
        Ouvrir_Fichier_Lecture(NomFichier, Fichier);
        Delete(Fichier);
   end;
   
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
      Put_Line (Fichier_temp, "Début");
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
      Put_Line (Fichier_temp, "Début");
      Close(Fichier_temp);
      DeclarerVariables(Memoire, File_Name);
      Modifier_Entier(Memoire, To_Unbounded_String("y"), 2);
      -- verifications
      Y_value := RecupererValeur_Entier(Memoire, To_Unbounded_String("y"));
      pragma Assert (Y_value = 2);
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
   
begin
   test_DeclarerVariables;
   test_Recuperer;
   test_ModifierVariables;
   deleteFileInstruct(File_Name);
end test_memoire;
