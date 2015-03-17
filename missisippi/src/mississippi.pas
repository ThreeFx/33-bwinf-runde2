{ (c) Ben Fiedler 2015 }

PROGRAM Mississippi;

USES SuffixTree, DateUtils, SysUtils;

FUNCTION ReadFromFile(filename : String) : String;
VAR
	f : Text;
BEGIN
	Assign(f, filename);
	Reset(f);
	Read(f, result);
END;




VAR
	tree : TSuffixTree;
	s : String;
	fromtime : TDateTime;
	ms : Integer;

BEGIN
	IF ParamCount = 1 THEN
	BEGIN
		s := ReadFromFile(ParamStr(1));
		WriteLn('Creating suffix tree...');
		FromTime := Now;
		tree := CreateSuffixTreeFrom(s);
		ms := MillisecondsBetween(Now, FromTime);
		WriteLn('Created suffix tree with ', tree.nodes, ' nodes in ', ms, 'ms');
	END
	ELSE
	BEGIN
		WRiteLn('Wrong number of Parameters');
	END;
END.
