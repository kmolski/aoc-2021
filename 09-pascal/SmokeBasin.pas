program SmokeBasin;

type Heatmap = Array of Array of Integer;
type Point = record x, y: Integer end;

var inputFile: Text;
    rowCount, rowLength, i: Integer;
    part1, part2: LongInt;
    line: String;
    hmap: Heatmap;
    points: Array of Point;

procedure FindLowPoints(hmap: Heatmap);
var p: Point;
    isLowPoint: Boolean;
    i, j, m, n: Integer;
    pointCount: Integer = 0;
begin
    SetLength(points, length(hmap) * length(hmap[0])); // max possible size
    for i := low(hmap) to high(hmap) do
    begin
        for j := low(hmap[i]) to high(hmap[i]) do
        begin
            isLowPoint := true;
            for m := -1 to 1 do // low point if all neighbours are higher
            begin
                for n := -1 to 1 do
                begin
                    if not ((m = 0) and (n = 0)) and
                       (low(hmap) <= i + m) and (i + m <= high(hmap)) and
                       (low(hmap[i]) <= j + n) and (j + n <= high(hmap[i])) then
                        isLowPoint := isLowPoint and (hmap[i, j] < hmap[i + m, j + n]);
                end;
            end;

            if isLowPoint then // append low point
            begin
                p.y := i; p.x := j;
                points[pointCount] := p;
                Inc(pointCount);
            end;
        end;
    end;
    SetLength(points, pointCount); // shrink array to fit
end;

function solvePart1(): Integer;
var p: Point;
begin
    solvePart1 := 0;
    for p in points do
    begin
        solvePart1 += (1 + hmap[p.y, p.x]);
    end;
end;

function pointExists(positions: Array of Point; np: Point): Boolean;
var p: Point;
begin
    for p in positions do
    begin
        if (p.x = np.x) and (p.y = np.y) then
            Exit(true);
    end;
    pointExists := false;
end;

function validPointOffset(p: Point; i, j: Integer): Boolean;
begin
    // check if point is in bounds, and its value on the heatmap
    // is smaller than 9 and larger than the previous point
    validPointOffset := (low(hmap) <= p.y + i) and (p.y + i <= high(hmap)) and
        (low(hmap[p.y]) <= p.x + j) and (p.x + j <= high(hmap[p.y])) and
        (hmap[p.y + i, p.x + j] < 9) and (hmap[p.y + i, p.x + j] > hmap[p.y, p.x]);
end;

function computeBasinSize(p: Point): Integer;
var temp: Point;
    i, j, k, positionCount, start, newStart: Integer;
    positions: Array of Point;
    gotNewPositions: Boolean = true;
    offset: Array[1..2] of Integer;
    offsets: Array[1..4, 1..2] of Integer = ((1, 0), (-1, 0), (0, 1), (0, -1));
begin
    positionCount := 1; start := 0;
    SetLength(positions, positionCount);
    positions[0] := p;

    while gotNewPositions do
    begin
        gotNewPositions := false;
        newStart := high(positions) + 1;
        for k := start to high(positions) do // start off with newly added positions
        begin
            for offset in offsets do
            begin
                i := offset[1]; j := offset[2];
                if validPointOffset(positions[k], i, j) then
                begin
                    temp.y := positions[k].y + i; temp.x := positions[k].x + j;
                    if not pointExists(positions, temp) then // add if point does not exist
                    begin
                        gotNewPositions := true;
                        SetLength(positions, positionCount + 1);
                        positions[positionCount] := temp;
                        Inc(positionCount);
                    end;
                end;
            end;
        end;
        start := newStart; // set new start position
    end;
    computeBasinSize := positionCount;
end;

function solvePart2(): LongInt;
var i, j, index, len: Integer;
    basinSizes: Array of Integer;
begin
    len := length(points);

    SetLength(basinSizes, len);
    for i := 0 to len do
    begin
        basinSizes[i] := computeBasinSize(points[i]);
    end;

    for i := 2 to len - 1 do // insertion sort
    begin
        index := basinSizes[i];
        j := i;
        while ((j > 1) and (basinSizes[j - 1] > index)) do
        begin
            basinSizes[j] := basinSizes[j-1];
            j := j - 1;
        end;
        basinSizes[j] := index;
    end;
    solvePart2 := basinSizes[len - 1] * basinSizes[len - 2] * basinSizes[len - 3];
end;

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
        SetLength(hmap, rowCount + 1, rowLength);
        for i := 1 to rowLength do // parse digits
        begin
            hmap[rowCount, i - 1] := ord(line[i]) - ord('0');
        end;
        Inc(rowCount);
    end;
    Close(inputFile);

    FindLowPoints(hmap);

    part1 := solvePart1();
    Writeln('Part 1: ', part1);

    part2 := solvePart2();
    Writeln('Part 2: ', part2);
end
else Writeln('ERROR: An input file is required!');

END.
