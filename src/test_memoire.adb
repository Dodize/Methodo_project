with Memoire;
with Ada.Text_IO; use Ada.Text_IO;

procedure test_memoire is
   
   package MemoireEntier is new Memoire(T_Elt => Integer);
   
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
      File_Name : constant String := "temp_instruct.txt";
   begin
      Delete(Fichier);
   end;
   
   -- Test concernant la declaration de variable.
   procedure test_DeclarerVariables is  
      use MemoireEntier;
      Fichier_temp : File_Type; -- le fichier d'instruction
      MemoirePremierElement : MemoireEntier.T_Memoire;
      X_value : Integer;
      Y_value : Integer;
   begin
      --initialisation de la memoire avec une variable
      createFileInstruct(Fichier_temp);
      Put_Line (Fichier_temp, "Programme_test est");
      Put_Line (Fichier_temp, "x : Entier");
      Put_Line (Fichier_temp, "Début");
      DeclarerVariables(MemoirePremierElement, Fichier_temp);
      Modifier(MemoirePremierElement, "x", 2);
      -- verifications
      X_value := RecupererValeur(MemoirePremierElement, "x");
      pragma Assert (X_value = 2);  
      deleteFileInstruct(Fichier_temp);      
      
      --initialisation de la memoire avec plusieurs variables
      createFileInstruct(Fichier_temp);
      Put_Line (Fichier_temp, "Programme_test est");
      Put_Line (Fichier_temp, "x, y, z : Entier");
      Put_Line (Fichier_temp, "Début");
      DeclarerVariables(MemoirePremierElement, Fichier_temp);
      Modifier(MemoirePremierElement, "y", 2);
      -- verifications
      Y_value := RecupererValeur(MemoirePremierElement, "y");
      pragma Assert (Y_value = 2);
      deleteFileInstruct(Fichier_temp);
   end;
   
   -- Test concernant la recuperation de valeur et type.
   procedure test_Recuperer is  
      use MemoireEntier;
      Fichier_temp : File_Type; -- le fichier d'instruction
      MemoirePremierElement : MemoireEntier.T_Memoire;
      X_value : Integer;
      Y_value : Integer;
      X_type : T_Type;
      Y_type : T_Type;
   begin
      --initialisation de la memoire avec une seule variable
      createFileInstruct(Fichier_temp);
      Put_Line (Fichier_temp, "Programme_test est");
      Put_Line (Fichier_temp, "x : Entier");
      Put_Line (Fichier_temp, "Début");
      DeclarerVariables(MemoirePremierElement, Fichier_temp);
      -- verifications
      X_value := RecupererValeur(MemoirePremierElement, "x");
      pragma Assert (X_value = 2);  
      X_type := RecupererType(MemoirePremierElement, "x");
      pragma Assert (X_type = ENTIER);
      deleteFileInstruct(Fichier_temp);
      
      --initialisation de la memoire avec plusieurs variables
      createFileInstruct(Fichier_temp);
      Put_Line (Fichier_temp, "Programme_test est");
      Put_Line (Fichier_temp, "x, y, z : Entier");
      Put_Line (Fichier_temp, "Début");
      DeclarerVariables(MemoirePremierElement, Fichier_temp);
      Modifier(MemoirePremierElement, "y", 2);
      -- verifications
      Y_value := RecupererValeur(MemoirePremierElement, "y");
      pragma Assert (Y_value = 2);  
      Y_type := RecupererType(MemoirePremierElement, "y");
      pragma Assert (Y_type = ENTIER);
      deleteFileInstruct(Fichier_temp);
   end;
   
   -- Test concernant la modification de variable.
   procedure test_ModifierVariables is  
      use MemoireEntier;
      Fichier_temp : File_Type; -- le fichier d'instruction
      MemoirePremierElement : MemoireEntier.T_Memoire;
      X_value : Integer;
   begin
      --initialisation de la memoire avec une variable
      createFileInstruct(Fichier_temp);
      Put_Line (Fichier_temp, "Programme_test est");
      Put_Line (Fichier_temp, "x : Entier");
      Put_Line (Fichier_temp, "Début");
      DeclarerVariables(MemoirePremierElement, Fichier_temp);
      Modifier(MemoirePremierElement, "x", 2);
      -- verifications
      X_value := RecupererValeur(MemoirePremierElement, "x");
      pragma Assert (X_value = 2);  
      Modifier(MemoirePremierElement, "x", 5);
      -- verifications
      X_value := RecupererValeur(MemoirePremierElement, "x");
      pragma Assert (X_value = 5);  
      deleteFileInstruct(Fichier_temp);
   end;
   
begin
   test_DeclarerVariables;
   test_Recuperer;
   test_ModifierVariables;
end test_memoire;
