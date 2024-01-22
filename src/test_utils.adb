with utils;                       use utils;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

procedure test_utils is

    procedure test_slice_mot is
        Ligne : Unbounded_String;
        Mot : Unbounded_String;
    begin
        Ligne := To_Unbounded_String("Test de slice");
        --verification
        slice_mot(Ligne, Mot, " ");
        pragma assert (Mot = "Test");
        slice_mot(Ligne, Mot, " ");
        pragma assert (Mot = "de");
        slice_mot(Ligne, Mot, " ");
        pragma assert (Mot = "slice");
        slice_mot(Ligne, Mot, " ");
        pragma assert (Mot = "");

        --test avec autre delimiteur
        Ligne := To_Unbounded_String("Test-de-slice");
        slice_mot(Ligne, Mot, "-");
        pragma assert (Mot = "Test");
        slice_mot(Ligne, Mot, "-");
        pragma assert (Mot = "de");
        slice_mot(Ligne, Mot, "-");
        pragma assert (Mot = "slice");
        slice_mot(Ligne, Mot, "-");
        pragma assert (Mot = "");
    end test_slice_mot;


    procedure test_Strip_Space is
        Chaine : Unbounded_String;
    begin
        Chaine := To_Unbounded_String("test de découpe");
        pragma assert ("testdedécoupe" = Strip_Space(To_String(Chaine)));
        Chaine := To_Unbounded_String("   test-de dé    coupe");
        pragma assert ("test-dedécoupe" = Strip_Space(To_String(Chaine)));
    end test_Strip_Space;


begin
    test_slice_mot;
    test_Strip_Space;
end test_utils;
