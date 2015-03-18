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
	IF (ParamCount = 1) AND FileExists(ParamStr(1)) THEN
	BEGIN
		s := ReadFromFile(ParamStr(1));
		WriteLn('[Main] Creating suffix tree...');
		FromTime := Now;
		tree := CreateSuffixTree(s);
		ms := MillisecondsBetween(Now, FromTime);
		WriteLn('[Main] Created suffix tree with ', tree.nodes, ' nodes in ', ms, 'ms');
		WriteLn('[Main] Suffix tree has ',CountLeaves(tree),' leaves');
	END
	ELSE
	BEGIN
		WRiteLn('Wrong number of parameters or file not found');
	END;
END.
