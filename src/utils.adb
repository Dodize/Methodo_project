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


    -- Permet de récupérer les chaines de caractères d'une ligne
    -- @param Ligne : ligne dont on extrait la chaine
    -- @param Ligne_restante : la partie de la ligne pas modifiée
    -- @param first_quote_found : indique si on a trouvé une chaine
    -- @return la chaine
    function slice_string(Ligne : in String; Ligne_restante : out Unbounded_String; first_quote_found : out Boolean) return Unbounded_String is
        first_quote : Character;
        last_quote_found : Boolean;
        new_string : Unbounded_String;
    begin
        first_quote := '0';
        first_quote_found := false;
        last_quote_found := false;
        Ligne_restante := To_Unbounded_String(Ligne);
        for i in Ligne'Range loop
            if (Ligne(i) = '"' or Ligne(i) = ''') and not first_quote_found then
                first_quote := Ligne(i);
                Delete(Ligne_restante, 1, 1);
                first_quote_found := True;
            else
                if Ligne(i) = first_quote and first_quote_found and not last_quote_found then
                    last_quote_found := True;
                    Put_Line("ici");
                elsif first_quote_found then
                    last_quote_found := False;
                    new_string := new_string & Ligne(i);
                    Delete(Ligne_restante, 1, 1);
                end if;
            end if;

            if i = Ligne'Length or (Ligne(i + 1) = ' ' and last_quote_found) then
                Delete(Ligne_restante, 1, 1);
                return new_string;
            end if;

        end loop;
        return new_string;
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
