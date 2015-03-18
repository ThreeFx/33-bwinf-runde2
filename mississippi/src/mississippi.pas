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
	len, rep, ms : Integer;

BEGIN
	IF (ParamCount = 3) AND FileExists(ParamStr(1)) THEN
	BEGIN
		s := ReadFromFile(ParamStr(1));
		WriteLn('[Main] Creating suffix tree...');
		FromTime := Now;
		tree := CreateSuffixTree(s);
		ms := MillisecondsBetween(Now, FromTime);
		WriteLn('[Main] Created suffix tree with ', tree.nodes, ' nodes in ', ms, 'ms');
		WriteLn('[Main] Suffix tree has ',CountLeaves(tree),' leaves');
		len := Integer(ParamStr(2));
		rep := Integer(ParamStr(3));
		IF (len > 0) AND (rep > 0) THEN
		BEGIN
			FindSubstrings(len, rep, tree);
		END
		ELSE
		BEGIN
			WriteLn('[Main] Please enter positive values for length and repetitions');
		END;
	END
	ELSE
	BEGIN
		Writeln('[Main] Wrong number of parameters or file not found, please enter:');
		WriteLn('[Main] ./prog filename length repetitions');
	END;
END.
