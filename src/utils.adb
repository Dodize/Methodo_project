package body utils is

    -- permet de récupérer dans mot les caractères d'une ligne présent avant délimiteur
    -- @param Ligne : ligne dont on extrait le mot
    -- @param Mot : a placer le mot extrait
    -- @param Delimiteur: caractere signant la fin du mot
    procedure slice_mot(Ligne : in out Unbounded_String; Mot : out Unbounded_String; Delimiteur : in String) is
        Pos : Natural;
    begin
        -- Trouver l'indice du delimiteur
        Pos := Index(Ligne, Delimiteur);
        -- Extraire la sous-chaine jusqu'au delimiteur
        if Pos /= 0 then
            Mot := Unbounded_Slice(Ligne, 1, Pos-1);
            -- Supprimer la partie extraite de la ligne
            Delete(Ligne, 1, Pos);
        else
            -- Si le delimiteur n'est pas trouve, copier la ligne entiere dans le mot
            Mot := Ligne;
            -- Reinitialiser la ligne
            Delete(Ligne, 1, Length(Ligne));
        end if;
    end slice_mot;


    -- Permet de récupérer une chaine de caractères d'une ligne
    -- @param Ligne : ligne dont on extrait la chaine
    -- @param First_part : la partie de la ligne située avant la première chaine
    -- @param Second_part : la partie de la ligne avec la première chaine trouvée
    -- @param Third_part : la partie de la ligne située après la première chaine
    -- @param first_quote_found : indique si on a trouvé une chaine
    procedure slice_string(Ligne : in String; First_part : out Unbounded_String; Second_part : out Unbounded_String; Third_part : out Unbounded_String; first_quote_found : out Boolean) is
        first_quote : Character;
        last_quote_found : Boolean;
    begin
        first_quote := '0';
        first_quote_found := false;
        last_quote_found := false;
        First_part := To_Unbounded_String("");
        Second_part := To_Unbounded_String("");
        Third_part := To_Unbounded_String("");

        for i in Ligne'Range loop
            -- Si on tombe sur le premier quote
            if (Ligne(i) = '"' or Ligne(i) = ''') and not first_quote_found then
                first_quote := Ligne(i);
                first_quote_found := True;
                -- Ajout du caractere dans la partie chaine
                Second_part := Second_part & Ligne(i);
            else
                -- Si on tombe sur le dernier quote
                if Ligne(i) = first_quote and first_quote_found and not last_quote_found then
                    last_quote_found := True;
                    -- Ajout du caractere dans la partie chaine
                    Second_part := Second_part & Ligne(i);
                -- Si on a le quote de début mais pas celui de fin
                elsif first_quote_found and not last_quote_found then
                    -- Ajout du caractere dans la partie chaine
                    Second_part := Second_part & Ligne(i);
                -- Si on a pas encore le premier quote
                elsif not first_quote_found then
                    -- Ajout du caractere dans la partie pré chaine
                    First_part := First_part & Ligne(i);
                -- Si on a trouvé le dernier quote et donc la fin de la chaine
                elsif last_quote_found then
                    -- Ajout du caractere dans la partie post chaine
                    Third_part := Third_part & Ligne(i);
                end if;
            end if;
        end loop;
    end;


    -- Ouvre le fichier en lecture pour pouvoir ensuite le parcourir
    -- @param NomFichier : le nom du fichier que l'on souhaite ouvrir en lecture
    -- @param FichierOuvert : l'object fichier apres ouverture
    procedure Ouvrir_Fichier_Lecture(NomFichier : in String; FichierOuvert : out File_Type) is
    begin
        Open(FichierOuvert, In_File, NomFichier);
    end Ouvrir_Fichier_Lecture;

    -- Parcours le fichier jusqu'à touver le mot "Début" et se place juste après
    -- @param Fichier : fichier à parcourir
    procedure parcourir_debut(Fichier : in File_Type) is
        Ligne : Unbounded_String;
    begin
        Ligne := To_Unbounded_String(Get_Line(Fichier));
        -- Parcours jusqu'a "Début" pour remplir ignorer la declaration des variables
        while (Index(Ligne, "Début")) = 0 or else (Index(Ligne, ":")) > 0 loop
            Ligne := To_Unbounded_String(Get_Line(Fichier));
        end loop;
    end parcourir_debut;

    -- Permet de retirer les espaces d'une chaine
    -- @pram Chaine : la chaine origine
    -- @return String : renvoie la chaine de départ sans aucun espace
    function Strip_Space (Chaine : in String) return String is
    begin
        if Chaine'Length = 0 then
            return "";
        elsif Chaine (Chaine'First) = ' ' then
            return Strip_Space (Chaine (Chaine'First + 1 .. Chaine'Last));
        else
            return Chaine (Chaine'First) & Strip_Space (Chaine (Chaine'First + 1 .. Chaine'Last));
        end if;
    end Strip_Space;


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

end utils;
