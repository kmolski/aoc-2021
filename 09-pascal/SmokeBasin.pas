program SmokeBasin;

type Point = record
         x, y: Integer;
     end;

// function findLowPoints(heatmap: Array of Array of Integer): Array of Point;
// begin
// end;

var inputFile: Text;
    rowCount, rowLength, riskLevelSum, i, j, m, n: Integer;
    line: String;
    isLowPoint: Boolean;
    heatmap: Array of Array of Integer;
    row: Array of Integer; // REMOVE

BEGIN

if paramCount() = 1 then
begin
    Assign(inputFile, paramStr(1));
    Reset(inputFile);

    rowCount := 0;
    while not eof(inputFile) do
    begin
        Readln(inputFile, line);
        rowLength := length(line);
        SetLength(heatmap, rowCount + 1, rowLength);
        for i := 1 to rowLength do
        begin
            heatmap[rowCount][i - 1] := ord(line[i]) - ord('0');
        end;
        Inc(rowCount);
    end;
    Close(inputFile);

    riskLevelSum := 0;
    for i := low(heatmap) to high(heatmap) do
    begin
        for j := low(heatmap[i]) to high(heatmap[i]) do
        begin
            isLowPoint := true;
            for m := -1 to 1 do
            begin
                for n := -1 to 1 do
                begin
                    if not ((m = 0) and (n = 0)) and
                       (low(heatmap) <= i + m) and (i + m <= high(heatmap)) and
                       (low(heatmap[i]) <= j + n) and (j + n <= high(heatmap[i])) then
                        isLowPoint := isLowPoint and (heatmap[i][j] < heatmap[i + m][j + n]);
                end;
            end;

            if isLowPoint then
                riskLevelSum += 1 + heatmap[i][j];
        end;
    end;

    Writeln('Part 1: ', riskLevelSum);
    // Writeln('Part 2: ');
end
else Writeln('ERROR: An input file is required!');

END.
