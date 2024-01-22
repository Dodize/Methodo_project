with Interpreteur; use Interpreteur;
with Ada.Text_IO ;                use Ada.Text_IO ;

procedure exemple_utilisation_terminal is
    FileName : String(1..100);
    TailleFileName : Integer;
begin
    Put_Line("Quel est le nom du fichier à exécuter (nom du fichier de 1 à 100 caractères, extension comprise) ?");
    TailleFileName := 100;
    Get_Line(FileName, TailleFileName);
    Executer(FileName(1..TailleFileName));
end exemple_utilisation_terminal;
