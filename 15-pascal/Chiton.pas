program Chiton;

type RiskLevelMap = Array of Array of Integer;
type Point = record x, y: Integer end;
type Vertex = record pos: Point; risk: Integer; end;

var inputFile: Text;
    rowCount, rowLength, i: Integer;
    line: String;
    riskmap: RiskLevelMap;

function solve(riskmap: RiskLevelMap): LongInt;
var i, j, rowLength, vertices, count, minDist, nextNode, visitedCount: LongInt;
    dist, prev, cost: Array of LongInt;
    visited: Array of Boolean;
    V: Array of Vertex;
    newDist: Int64;
    pi, pj: Point;
    vtx: Vertex;
begin
    rowLength := length(riskmap[0]);
    vertices := length(riskmap) * rowLength;
    SetLength(V, vertices);
    for i := 0 to high(riskmap) do // convert risk level map to vertices
    begin
        for j := 0 to high(riskmap[0]) do
        begin
            pi.y := i; pi.x := j;
            vtx.pos := pi; vtx.risk := riskmap[i, j];
            V[i * rowLength + j] := vtx;
        end;
    end;

    // find shortest path using Dijkstra's algorithm
    SetLength(dist, vertices);
    SetLength(prev, vertices);
    SetLength(visited, vertices);
    pi := V[0].pos; 
    for i := 0 to high(V) do
    begin
        pj := V[i].pos;
        if ((abs(pi.x - pj.x) = 1) and (pi.y = pj.y)) or
           ((abs(pi.y - pj.y) = 1) and (pi.x = pj.x)) then
            dist[i] := V[i].risk
        else
            dist[i] := 2147483647;
        prev[i] := 0;
        visited[i] := false;
    end;

    dist[0] := 0;
    visited[0] := true;
    count := 1;
    visitedCount := 1;

    Writeln();
    SetLength(cost, vertices);
    while count < (vertices - 1) do
    begin
        Write('Visited ', visitedCount, ' out of ', vertices, ' nodes'#13);
        minDist := 2147483647;
        for i := 0 to high(V) do
            if (dist[i] < minDist) and not visited[i] then
            begin
                minDist := dist[i];
                nextNode := i;
            end;

        visited[nextNode] := true;
        visitedCount += 1;

        pi := V[nextNode].pos; 
        for i := 0 to high(V) do
        begin
            pj := V[i].pos;
            if ((abs(pi.x - pj.x) = 1) and (pi.y = pj.y)) or
               ((abs(pi.y - pj.y) = 1) and (pi.x = pj.x)) then
                cost[i] := V[i].risk
            else if nextNode = i then
                cost[i] := 0
            else
                cost[i] := 2147483647;
        end;

        for i := 0 to high(V) do
            if not visited[i] then
            begin
                newDist := minDist + cost[i];
                if newDist < dist[i] then
                begin
                    dist[i] := newDist;
                    prev[i] := nextNode;
                end;
            end;
        Inc(count);
    end;
    Writeln();

    solve := dist[high(V)]; // get total risk of path
end;

function expandMap(riskmap: RiskLevelMap): RiskLevelMap;
var i, j, m, n, rowCount, rowLength: LongInt;
begin
    rowCount := length(riskmap); rowLength := length(riskmap[0]);
    SetLength(expandMap, rowCount * 5, rowLength * 5);
    for i := 0 to high(riskmap) do
        for m := 0 to 4 do
            for j := 0 to high(riskmap[0]) do
                for n := 0 to 4 do
                    expandMap[i + m * rowCount, j + n * rowLength] :=
                        (riskmap[i, j] - 1 + m + n) mod 9 + 1;
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
        SetLength(riskmap, rowCount + 1, rowLength);
        for i := 1 to rowLength do // parse digits
            riskmap[rowCount, i - 1] := ord(line[i]) - ord('0');
        Inc(rowCount);
    end;
    Close(inputFile);

    Writeln('Part 1: ', solve(riskmap));
    Writeln('Part 2: ', solve(expandMap(riskmap)));
end
else Writeln('ERROR: An input file is required!');

END.
