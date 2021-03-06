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
		WriteLn('[Main] Beginning read...');
		s := ReadFromFile(ParamStr(1));
		len := StrtoInt(ParamStr(2));
		rep := StrToInt(ParamStr(3));

		IF (len > 0) AND (rep > 0) THEN
		BEGIN
			WriteLn('[Main] Creating suffix tree...');
			FromTime := Now;
			tree := CreateSuffixTree(s);
			ms := MillisecondsBetween(Now, FromTime);
			WriteLn('[Main] Created suffix tree with ', tree.nodes, ' nodes in ', ms, 'ms');
			WriteLn('[Main] Suffix tree has ',CountLeaves(tree),' leaves');

			WriteLn('[Main] Arguments are: length (',len,'); repetitions (',rep,')');
			FromTime := Now;
			FindSubstrings(len, rep, tree);
			ms := MillisecondsBetween(Now, FromTime);
			WriteLn('[Main] Query for substrings repeated at least ',rep,' times ',
				           'and are at least ',len,' characters long ',
				           'took ',ms,'ms');

			DisposeTree(tree);
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
