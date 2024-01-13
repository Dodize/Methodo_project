with Memoire;

procedure test_memoire is
   
    package MemoireEntier is new Memoire(T_Elt => Integer);
   
   -- Test concernant l'initialise une case memoire.
   procedure test_Initialiser is  
      use MemoireEntier;
      MemoirePremierElement : MemoireEntier.T_Memoire;
   begin
      --initialisation de la memoire
      Initialiser(MemoirePremierElement);
      pragma Assert (Est_Vide(MemoirePremierElement));
   end;
   
   -- Test concernant la declaration de variable.
   procedure test_DeclarerVariables is  
      use MemoireEntier;
      MemoirePremierElement : MemoireEntier.T_Memoire;
      X_value : Integer;
      Y_value : Integer;
   begin
      --initialisation de la memoire avec une seule variable
      Initialiser(MemoirePremierElement);
      DeclarerVariables(MemoirePremierElement, "x : Entier");
      Modifier(MemoirePremierElement, "x", 2);
      -- verifications
      X_value := RecupererValeur(MemoirePremierElement, "x");
      pragma Assert (X_value = 2);  
      
      --initialisation de la memoire avec plusieurs variables
      Initialiser(MemoirePremierElement);
      DeclarerVariables(MemoirePremierElement, "x,y,z : Entier");
      Modifier(MemoirePremierElement, "y", 2);
      -- verifications
      Y_value := RecupererValeur(MemoirePremierElement, "y");
      pragma Assert (Y_value = 2);  
   end;
   
   -- Test concernant la recuperation de valeur et type.
   procedure test_Recuperer is  
      use MemoireEntier;
      MemoirePremierElement : MemoireEntier.T_Memoire;
      X_value : Integer;
      Y_value : Integer;
      X_type : T_Type;
      Y_type : T_Type;
   begin
      --initialisation de la memoire avec une seule variable
      Initialiser(MemoirePremierElement);
      DeclarerVariables(MemoirePremierElement, "x : Entier");
      Modifier(MemoirePremierElement, "x", 2);
      -- verifications
      X_value := RecupererValeur(MemoirePremierElement, "x");
      pragma Assert (X_value = 2);  
      X_type := RecupererType(MemoirePremierElement, "x");
      pragma Assert (X_type = ENTIER);
      
      --initialisation de la memoire avec plusieurs variables
      Initialiser(MemoirePremierElement);
      DeclarerVariables(MemoirePremierElement, "x,y,z : Entier");
      Modifier(MemoirePremierElement, "y", 2);
      -- verifications
      Y_value := RecupererValeur(MemoirePremierElement, "y");
      pragma Assert (Y_value = 2);  
      Y_type := RecupererType(MemoirePremierElement, "Y");
      pragma Assert (Y_type = ENTIER);
   end;
   
begin
   null;
end test_memoire;
