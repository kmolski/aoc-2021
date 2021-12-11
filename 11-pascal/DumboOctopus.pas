program DumboOctopus;

uses Math;

type OctopusArray = Array[1..10, 1..10] of Integer;

var inputFile: Text;
    rowCount, i: Integer;
    line: String;
    octopuses: OctopusArray;

procedure MarkAround(var octopuses: OctopusArray; i, j: Integer);
var m, n: Integer;
begin
    for m := max(1, i - 1) to min(10, i + 1) do
        for n := max(1, j - 1) to min(10, j + 1) do
            Inc(octopuses[m, n]);
end;

function simulateStep(var octopuses: OctopusArray): Integer;
var i, j: Integer;
    didFlash: Boolean;
begin
    simulateStep := 0;
    for i := 1 to 10 do // step 1: raise energy of each octopus
        for j := 1 to 10 do
            Inc(octopuses[i, j]);
    repeat
        didFlash := false;
        for i := 1 to 10 do // step 2: flash any octopus that has > 9 energy
            for j := 1 to 10 do
                if octopuses[i, j] > 9 then
                begin
                    didFlash := true;
                    Inc(simulateStep);
                    MarkAround(octopuses, i, j);
                    octopuses[i, j] := -100; // skip for the rest of step
                end;
    until not didFlash;
    for i := 1 to 10 do // step 3: reset energy if flashed
        for j := 1 to 10 do
            if octopuses[i, j] < 0 then
                octopuses[i, j] := 0;
end;

function solvePart1(octopuses: OctopusArray): Integer;
var i: Integer;
begin
    solvePart1 := 0;
    for i := 1 to 100 do
        solvePart1 += simulateStep(octopuses);
end;

function synchronized(var octopuses: OctopusArray): Boolean;
var first, col: Integer;
    row: Array[1..10] of Integer;
begin
    synchronized := true;
    first := octopuses[1][1];
    for row in octopuses do
        for col in row do
            if col <> first then Exit(false);
end;

function solvePart2(octopuses: OctopusArray): Integer;
begin
    solvePart2 := 0;
    while not synchronized(octopuses) do
    begin
        simulateStep(octopuses);
        Inc(solvePart2);
    end;
end;

BEGIN

if paramCount() = 1 then
begin
    Assign(inputFile, paramStr(1));
    Reset(inputFile);

    rowCount := 1;
    while not eof(inputFile) do
    begin
        Readln(inputFile, line);

        if rowCount > length(octopuses) then
        begin
            Writeln('ERROR: Input file is too long!');
            Exit();
        end;

        for i := 1 to 10 do // parse digits
            octopuses[rowCount, i] := ord(line[i]) - ord('0');
        Inc(rowCount);
    end;
    Close(inputFile);

    Writeln('Part 1: ', solvePart1(octopuses));
    Writeln('Part 2: ', solvePart2(octopuses));
end
else Writeln('ERROR: An input file is required!');

END.
