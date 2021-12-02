program Dive;

var inputFile: Text;
    lineCount, intValue: Integer;
    part1_horizontal, part1_depth, part2_horizontal, part2_depth, part2_aim: Longint;
    l: String;
    lines: Array of String;

BEGIN

if paramCount() = 1 then
begin
    assign(inputFile, paramStr(1));
    reset(inputFile);

    lineCount := 0;
    while not eof(inputFile) do
    begin
        setLength(lines, lineCount + 1);
        readln(inputFile, lines[lineCount]);
        inc(lineCount);
    end;
    close(inputFile);

    part1_horizontal := 0; part2_horizontal := 0;
    part1_depth := 0; part2_depth := 0; part2_aim := 0;
    intValue := 0;
    for l in lines do
    begin
        if Pos('forward', l) = 1 then
        begin
            val(copy(l, pos(' ', l), 255), intValue);
            part1_horizontal += intValue;
            part2_horizontal += intValue;
            part2_depth += part2_aim * intValue;
        end
        else if Pos('up', l) = 1 then
        begin
            val(copy(l, pos(' ', l), 255), intValue);
            part1_depth -= intValue;
            part2_aim -= intValue;
        end
        else if Pos('down', l) = 1 then
        begin
            val(copy(l, pos(' ', l), 255), intValue);
            part1_depth += intValue;
            part2_aim += intValue;
        end;
    end;

    writeln('Part 1: ', part1_horizontal * part1_depth);
    writeln('Part 2: ', part2_horizontal * part2_depth);
end
else writeln('ERROR: An input file is required!');

END.
