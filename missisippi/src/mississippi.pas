{ (c) Ben Fiedler 2015 }

PROGRAM Mississippi;

USES SuffixTree, DateUtils, SysUtils;

FUNCTION ReadFromFile(filename : String) : String;
VAR
	f : Text;
BEGIN
	Assign(f, filename);
	Reset(f);
END;




VAR
	tree : TSuffixTree;
	s : String;
	fromtime : TDateTime;
	ms : Integer;

BEGIN
	s := 'abaaba';
	WriteLn('Creating suffix tree for: ', s);
	FromTime := Now;
	tree := CreateSuffixTreeFrom(s);
	ms := MillisecondsBetween(Now, FromTime);
	WriteLn('Created suffix tree with ', tree.nodes, ' nodes in ', ms, 'ms');
END.
