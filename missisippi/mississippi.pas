{ (c) Ben Fiedler 2015 }

PROGRAM Mississippi;

USES SuffixTree;

VAR
	tree : TSuffixTree;
	s : String;

BEGIN
	s := 'abaaba';
	WriteLn('Creating suffix tree for: ', s);
	tree := CreateSuffixTreeFrom(s);
	WriteLn('Created suffix tree with ', tree.nodes, ' nodes.');
	Write('Press enter to exit...');
	ReadLn;
END.
